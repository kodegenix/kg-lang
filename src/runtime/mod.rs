use super::*;


mod script;

use std::collections::VecDeque;


#[derive(Debug)]
pub enum Error {
    Lexer(LexerError),
    Parser(ParserError),
    Unspecified(u32),
}

impl From<LexerError> for Error {
    fn from(err: LexerError) -> Error {
        Error::Lexer(err)
    }
}

impl From<ParserError> for Error {
    fn from(err: ParserError) -> Error {
        Error::Parser(err)
    }
}


pub trait Runtime {
    fn process(&mut self, reader: &mut dyn ByteReader) -> Result<NodeRef, Error>;

    fn reset(&mut self);
}


enum TokenProcessResult {
    Done = 0,
    Skip = 1,
    More = 2,
}


#[derive(Debug)]
struct JsRuntimeContext {
    token_input_queue: VecDeque<Token>,
    token_merge_queue: VecDeque<Token>,
    mode: usize,
    mode_stack: Vec<usize>,
    channel: usize,
    channel_stack: Vec<usize>,
}

impl JsRuntimeContext {
    fn new() -> JsRuntimeContext {
        JsRuntimeContext {
            token_input_queue: VecDeque::new(),
            token_merge_queue: VecDeque::new(),
            mode: 0,
            mode_stack: Vec::new(),
            channel: 0,
            channel_stack: Vec::new(),
        }
    }

    fn set_mode(&mut self, mode: usize) {
        if self.mode != mode {
            debug!("entering mode: {}", mode);
            self.mode = mode;
        }
    }

    fn push_mode(&mut self, mode: usize) {
        self.mode_stack.push(self.mode);
        self.set_mode(mode);
    }

    fn pop_mode(&mut self) {
        let mode = self.mode_stack.pop().unwrap();
        self.set_mode(mode);
    }

    fn set_channel(&mut self, channel: usize) {
        if self.channel != channel {
            debug!("entering channel: {}", channel);
            self.channel = channel;
        }
    }

    fn push_channel(&mut self, channel: usize) {
        self.channel_stack.push(self.channel);
        self.set_channel(channel);
    }

    fn pop_channel(&mut self) {
        let channel = self.channel_stack.pop().unwrap();
        self.set_channel(channel);
    }

    fn register(&mut self, engine: &mut Engine, obj_index: i32) {
        engine.put_prop_function(obj_index, self, "__push_mode", 1);
        engine.put_prop_function(obj_index, self, "__pop_mode", 0);
        engine.put_prop_function(obj_index, self, "__set_mode", 1);
        engine.put_prop_function(obj_index, self, "__push_channel", 1);
        engine.put_prop_function(obj_index, self, "__pop_channel", 0);
        engine.put_prop_function(obj_index, self, "__set_channel", 1);
        engine.put_prop_function(obj_index, self, "__enqueue_token", 1);
    }
}

impl Default for JsRuntimeContext {
    fn default() -> JsRuntimeContext {
        JsRuntimeContext::new()
    }
}

impl CallJs for JsRuntimeContext {
    fn call(&mut self, engine: &mut Engine, func_name: &str) -> i32 {
        match func_name {
            "__push_mode" => {
                let mode = engine.get_number(0) as usize;
                self.push_mode(mode);
            }
            "__pop_mode" => {
                self.pop_mode();
            }
            "__set_mode" => {
                let mode = engine.get_number(0) as usize;
                self.set_mode(mode);
            }
            "__push_channel" => {
                let channel = engine.get_number(0) as usize;
                self.push_channel(channel);
            }
            "__pop_channel" => {
                self.pop_channel();
            }
            "__set_channel" => {
                let channel = engine.get_number(0) as usize;
                self.set_channel(channel);
            }
            "__enqueue_token" => {
                let mut token = Token::default();
                engine.read(&mut token, 0);
                self.token_input_queue.push_back(token);
            }
            _ => unreachable!()
        }
        0
    }
}


pub struct JsRuntime {
    grammar: GrammarRef,
    engine: Engine,
    lexers: Vec<Box<dyn Lexer>>,
    parsers: Vec<Box<dyn Parser>>,
    context: Box<JsRuntimeContext>,
}

impl JsRuntime {
    pub fn new(grammar: &GrammarRef) -> Result<JsRuntime, Error> {
        let g = grammar.borrow();
        let mut r = JsRuntime {
            grammar: grammar.clone(),
            engine: Engine::new(),
            lexers: Vec::with_capacity(g.modes().len()),
            parsers: Vec::with_capacity(g.channels().len()),
            context: Box::new(JsRuntimeContext::new()),
        };
        r.init()?;
        Ok(r)
    }

    fn build_lexers(&mut self) -> Result<(), Error> {
        use lexer::prog::Matcher;

        let g = self.grammar.borrow();
        for mode in g.modes().iter() {
            self.lexers.push(Box::new(Matcher::new(&self.grammar, mode.unmatched(), mode.index())));
        }
        Ok(())
    }

    fn build_parsers(&mut self) -> Result<(), Error> {
        use parser::clr::ClrParser;

        let g = self.grammar.borrow();
        for channel in g.channels().iter() {
            self.parsers.push(Box::new(ClrParser::build(&self.grammar, channel.index())?));
        }
        Ok(())
    }

    fn build_actions_js(&self, code: &mut dyn std::fmt::Write) -> std::fmt::Result {
        let g = self.grammar.borrow();
        let mut iter = g.rules().iter().peekable();
        while let Some(r) = iter.next() {
            write!(code, "{}: function (", r.id())?;
            if r.symbols().len() > 0 {
                for k in 1..r.symbols().len() {
                    write!(code, "${}, ", k)?;
                }
                write!(code, "${}", r.symbols().len())?;
            }
            write!(code, ") {{\n  var $$;\n  {}\n  return $$;\n}}", r.action())?;
            if iter.peek().is_some() {
                write!(code, ",\n")?;
            }
        }
        Ok(())
    }

    fn build_commands_js(&self, code: &mut dyn std::fmt::Write) -> std::fmt::Result {
        let g = self.grammar.borrow();
        let mut empty = true;
        for t in g.terminals().iter() {
            for (ci, c) in t.commands().iter().enumerate() {
                if let Cmd::Custom(ref action) = *c.cmd() {
                    if !empty {
                        write!(code, ",\n")?;
                    }
                    write!(code, "c{}_{}: function ($token) {{\n  {}\n}}", t.index(), ci, action)?;
                    empty = false;
                }
            }
        }
        Ok(())
    }

    fn build_modes_js(&self, code: &mut dyn std::fmt::Write) -> std::fmt::Result {
        let g = self.grammar.borrow();
        let mut iter = g.modes().iter().peekable();
        while let Some(m) = iter.next() {
            write!(code, "'{}': {{\n\tindex: {},\n\tname: '{}',\n\tlexemes: {{\n", m.id(), m.index(), m.id())?;
            let terminals = m.terminals();
            let mut lit = terminals.iter().peekable();
            while let Some(&t) = lit.next() {
                let t = g.terminal(t);
                write!(code, "\t\t{:?}: {{\n\t\t\tindex: {},\n\t\t\tname: {:?}\n\t\t}}", t.id(), t.index(), t.id())?;
                if lit.peek().is_some() {
                    write!(code, ",\n")?;
                }
            }
            write!(code, "\n\t}}\n}}")?;
            if iter.peek().is_some() {
                write!(code, ",\n")?;
            }
        }
        Ok(())
    }

    fn build_channels_js(&self, code: &mut dyn std::fmt::Write) -> std::fmt::Result {
        let g = self.grammar.borrow();
        let mut iter = g.channels().iter().peekable();
        while let Some(c) = iter.next() {
            write!(code, "'{}': {}", c.id(), c.index())?;
            if iter.peek().is_some() {
                write!(code, ",\n")?;
            }
        }
        Ok(())
    }

    fn init_js(&mut self) -> Result<(), Error> {
        let mut code = String::with_capacity(128 * 1024);
        let mut actions = String::new();
        self.build_actions_js(&mut actions).map_err(|_| Error::Unspecified(line!()))?;
        let mut commands = String::new();
        self.build_commands_js(&mut commands).map_err(|_| Error::Unspecified(line!()))?;
        let mut modes = String::new();
        self.build_modes_js(&mut modes).map_err(|_| Error::Unspecified(line!()))?;
        let mut channels = String::new();
        self.build_channels_js(&mut channels).map_err(|_| Error::Unspecified(line!()))?;
        {
            let g = self.grammar.borrow();
            for line in script::SCRIPT_TEMPLATE.lines() {
                match line.trim() {
                    "<%STRICT_MODE%>" => {
                        if g.strict_mode() {
                            code.push_str("'use strict';\n");
                        }
                    }
                    "<%GLOBALS%>" => {
                        code.push_str(g.globals());
                        code.push('\n');
                    }
                    "<%ACTIONS%>" => {
                        code.push_str(&actions);
                        code.push('\n');
                    }
                    "<%COMMANDS%>" => {
                        code.push_str(&commands);
                        code.push('\n');
                    }
                    "<%MODES%>" => {
                        code.push_str(&modes);
                        code.push('\n');
                    }
                    "<%CHANNELS%>" => {
                        code.push_str(&channels);
                        code.push('\n');
                    }
                    _ => {
                        code.push_str(line);
                        code.push('\n');
                    }
                }
            }
        }

        self.engine.eval("parse.js", &code);
        self.engine.get_global_string("parser");

        self.context.register(&mut self.engine, -1);

        Ok(())
    }

    fn init(&mut self) -> Result<(), Error> {
        self.build_lexers()?;
        self.build_parsers()?;
        self.init_js()
    }


    fn current_parser(&self) -> &dyn Parser {
        self.parsers[self.context.channel].as_ref()
    }

    fn current_parser_mut(&mut self) -> &mut dyn Parser {
        self.parsers[self.context.channel].as_mut()
    }


    fn current_lexer(&self) -> &dyn Lexer {
        self.lexers[self.context.mode].as_ref()
    }

    fn current_lexer_mut(&mut self) -> &mut dyn Lexer {
        self.lexers[self.context.mode].as_mut()
    }


    fn process_token(&mut self, token: &mut Token) -> Result<TokenProcessResult, Error> {
        let g = self.grammar.borrow();
        let lexeme = g.terminal(token.lexeme());
        let mut result = TokenProcessResult::Done;
        for (ci, cmd) in lexeme.commands().iter_mode(self.context.mode).enumerate() {
            match *cmd {
                Cmd::Skip => {
                    if let TokenProcessResult::Done = result {
                        result = TokenProcessResult::Skip;
                    } else {
                        return Err(Error::Unspecified(line!()));
                    }
                }
                Cmd::More => {
                    if let TokenProcessResult::Done = result {
                        result = TokenProcessResult::More;
                    } else {
                        return Err(Error::Unspecified(line!()));
                    }
                }
                Cmd::Type(t) => {
                    let term = g.terminal(t);
                    token.set_lexeme(term.index());
                    token.set_id(term.id());
                }
                Cmd::Mode(mode) => {
                    self.context.set_mode(mode);
                }
                Cmd::ModePush(mode) => {
                    self.context.push_mode(mode);
                }
                Cmd::ModePop => {
                    self.context.pop_mode();
                }
                Cmd::Channel(channel) => {
                    self.context.set_channel(channel);
                }
                Cmd::ChannelPush(channel) => {
                    self.context.push_channel(channel);
                }
                Cmd::ChannelPop => {
                    self.context.pop_channel();
                }
                Cmd::Custom(_) => {
                    use std::fmt::Write;

                    //FIXME (jc) turn off reading/writing of unused args
                    let mut id = String::with_capacity(10);
                    write!(id, "c{}_{}", lexeme.index(), ci).unwrap();
                    self.engine.push_string("command");
                    self.engine.push_string(&id);
                    self.engine.write(token);
                    self.engine.call_prop(-4, 2);
                    self.engine.read_top(token);
                    self.engine.pop();
                }
            }
        }
        Ok(result)
    }

    fn lex_token(&mut self, reader: &mut dyn ByteReader) -> Result<Token, Error> {
        let mut token: Token;

        loop {
            if self.context.token_input_queue.is_empty() {
                token = self.current_lexer_mut().lex(reader)?;
                token.set_mode(self.context.mode);
                token.set_channel(self.context.channel);
            } else {
                token = self.context.token_input_queue.pop_front().unwrap();
                token.set_mode(self.context.mode);
                token.set_channel(self.context.channel);
            };

            match self.process_token(&mut token)? {
                TokenProcessResult::Done => break,
                TokenProcessResult::Skip => continue,
                TokenProcessResult::More => {
                    self.context.token_merge_queue.push_back(token);
                }
            }
        }

        if !self.context.token_merge_queue.is_empty() {
            self.context.token_merge_queue.push_back(token);
            token = self.context.token_merge_queue.pop_front().unwrap();
            for t in self.context.token_merge_queue.drain(..) {
                token.set_lexeme(t.lexeme());
                token.set_id(t.id());
                token.append_value(t.value());
            }
        }

        Ok(token)
    }

    fn parse_token(&mut self, token: &Token) -> Result<(), Error> {
        loop {
            let step = self.current_parser_mut().parse(token)?;
            match step {
                Step::Accept => {
                    self.engine.push_string("accept");
                    self.engine.call_prop(-2, 0);
                    break;
                }
                Step::Shift => {
                    self.engine.push_string("shift");
                    self.engine.write(token);
                    self.engine.call_prop(-3, 1);
                    self.engine.pop();
                    break;
                }
                Step::Reduce(r) => {
                    let g = self.grammar.borrow();
                    let rule = g.rule(r);
                    let n = rule.symbols().len();

                    self.engine.push_string("reduce");
                    self.engine.push_string(rule.id());
                    self.engine.push_number(n as f64);
                    self.engine.call_prop(-4, 2);
                    self.engine.pop();
                }
            }
        }
        Ok(())
    }
}

impl Runtime for JsRuntime {
    fn process(&mut self, reader: &mut dyn ByteReader) -> Result<NodeRef, Error> {
        self.reset();

        loop {
            let t = self.lex_token(reader)?;
            if t.lexeme() != 0 {
                self.parse_token(&t)?;
            } else {
                self.parse_token(&t)?;
                self.parse_token(&t)?;
                break;
            }
        }

        let res = self.engine.read_node(-1);
        self.engine.pop_n(2);

        Ok(res)
    }

    fn reset(&mut self) {
        for lexer in self.lexers.iter_mut() {
            lexer.reset();
        }

        for parser in self.parsers.iter_mut() {
            parser.reset();
        }

        self.engine.push_string("clear");
        self.engine.call_prop(-2, 0);
        self.engine.pop();
    }
}
