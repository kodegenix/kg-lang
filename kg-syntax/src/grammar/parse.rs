use super::*;


#[derive(Debug)]
pub enum Error {
    Unspecified(u32),
    IoError,
    Utf8Error,
    RegexError,
    UnbalancedBlockComment,
}

impl From<std::str::Utf8Error> for Error {
    fn from(_: std::str::Utf8Error) -> Error {
        Error::Utf8Error
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(_: std::num::ParseIntError) -> Error {
        Error::Unspecified(line!())
    }
}

//FIXME (jc)
impl From<ParseDiag> for Error {
    fn from(_: ParseDiag) -> Error {
        Error::IoError
    }
}

//FIXME (jc)
impl From<IoErrorDetail> for Error {
    fn from(_: IoErrorDetail) -> Error {
        Error::IoError
    }
}

pub fn parse(r: &mut dyn CharReader) -> Result<GrammarRef, Error> {
    let g = GrammarRef::new();

    let mut p = Parser::new();

    let mut pg = p.parse(r)?;

    let mut modes: Vec<Mode> = Vec::with_capacity(32);
    let mut channels: Vec<Channel> = Vec::with_capacity(32);
    let mut terminals: Vec<Lexeme> = Vec::with_capacity(128);
    let mut productions: Vec<Production> = Vec::with_capacity(128);
    let mut rules: Vec<Rule> = Vec::with_capacity(128);

    {
        let t0 = Lexeme::new(0, "$", Regex::Empty, &g);
        terminals.push(t0);

        let p0 = Production::new(0, "@s", &g);
        productions.push(p0);

        let mut r0 = Rule::new(0, 0, &g);
        r0.symbols.push(Symbol::Production(1));
        r0.symbols.push(Symbol::Terminal(0));
        r0.action = Action::Assign(1);
        rules.push(r0);
    }

    fn add_terminal<'a>(id: &'a str, regex: Regex, terminals: &'a mut Vec<Lexeme>, g: &'a GrammarRef) -> Result<&'a mut Lexeme, Error> {
        let mut index = ::std::usize::MAX;
        let tid: Cow<'a, str> = if id.is_empty() {
            Cow::Owned(match regex {
                //FIXME (jc) handle icase for default id
                Regex::Literal { ref chars, .. } => {
                    let mut n = String::with_capacity(chars.len() + 2);
                    n.push('\'');
                    n.extend(chars.iter().cloned());
                    n.push('\'');
                    n
                }
                _ => regex.to_string(),
            })
        } else {
            Cow::Borrowed(id)
        };

        for t in terminals.iter_mut() {
            if t.id == tid {
                if t.regex == regex || regex == Regex::Empty {
                    index = t.index;
                    break;
                } else {
                    return Err(Error::Unspecified(line!()));
                }
            }
        }
        if index == ::std::usize::MAX {
            index = terminals.len();
            let t = Lexeme::new(index, tid, regex, g);
            terminals.push(t);
        }
        Ok(&mut terminals[index])
    }

    fn add_production<'a>(id: String, productions: &'a mut Vec<Production>, g: &'a GrammarRef) -> Result<&'a mut Production, Error> {
        let mut index = ::std::usize::MAX;
        for p in productions.iter_mut() {
            if p.id == id {
                index = p.index;
                break;
            }
        }
        if index == ::std::usize::MAX {
            index = productions.len();
            productions.push(Production::new(index, id, g));
        }
        Ok(&mut productions[index])
    }

    let mut mark = 1;
    for mi in 0..pg.modes.len() {
        for ri in 0..pg.modes[mi].rules.len() {
            p.resolve_lexer_rule_regex(r, &mut pg, mi, ri, mark)?;
            mark += 1;
        }
    }

    //TODO (jc) add support for mode inheritance
    for m in pg.modes.iter() {
        let mode = Mode::new(modes.len(), m.name.clone(), &g);
        m.mode.set(mode.index);
        modes.push(mode);
    }

    //TODO (jc) add support for channel inheritance
    for c in pg.channels.iter() {
        let channel = Channel::new(channels.len(), c.name.clone(), &g);
        c.channel.set(channel.index);
        channels.push(channel);
    }

    //FIXME (jc) remove redundant lexemes
    for m in pg.modes.iter() {
        if m.is_active() {
            let mode_index = m.mode.get();
            for rule in m.rules.iter() {
                if !rule.var {
                    let regex = rule.expr.regex.as_ref().unwrap().clone();
                    let lexeme = add_terminal(&rule.name, regex, &mut terminals, &g)?;
                    lexeme.add_mode(mode_index);
                    rule.terminal.set(lexeme.index);
                    if lexeme.regex == Regex::Any {
                        modes[mode_index].unmatched = lexeme.index;
                    }
                }
            }
        }
    }

    for c in pg.channels.iter() {
        if c.is_active() {
            for rule in c.rules.iter() {
                //FIXME (jc) add support for production variables
                if !rule.var {
                    let production = add_production(rule.name.clone(), &mut productions, &g)?;
                    rule.production.set(production.index);
                }
                for f in rule.fragments.iter() {
                    for e in f.elements.iter() {
                        if let Element::Literal(ref s) = *e {
                            let regex = Regex::Literal {
                                chars: s.chars().skip(1).take(s.len() - 2).collect(),
                                icase: false,
                            };
                            let lexeme = add_terminal("", regex, &mut terminals, &g)?;
                            lexeme.add_mode(0);
                        }
                    }
                }
            }
        }
    }

    for c in pg.channels.iter() {
        if c.is_active() {
            let channel_index = c.channel.get();
            for r in c.rules.iter() {
                if !r.var && r.is_active() {
                    let production = r.production.get();
                    // TODO (jc) support for nested fragments and repeats
                    // TODO (jc) support for mid-rule actions
                    for f in r.fragments.iter() {
                        let mut rule = Rule::new(rules.len(), production, &g);
                        for e in f.elements.iter() {
                            match *e {
                                Element::Literal(ref s) => {
                                    //FIXME (jc) implement faster terminal lookup by id
                                    if let Some(t) = terminals.iter().find(|&t| *s == t.id) {
                                        rule.symbols.push(Symbol::Terminal(t.index));
                                    } else {
                                        return Err(Error::Unspecified(line!())); //undefined terminal literal, this cannot happen
                                    }
                                }
                                Element::Terminal(ref id) => {
                                    //FIXME (jc) implement faster terminal lookup by id
                                    if let Some(t) = terminals.iter().find(|&t| t.id == id.as_ref()) {
                                        rule.symbols.push(Symbol::Terminal(t.index));
                                    } else {
                                        return Err(Error::Unspecified(line!())); //undefined terminal
                                    }
                                }
                                Element::Production(ref id) => {
                                    //FIXME (jc) implement faster production lookup by id
                                    if let Some(p) = productions.iter().find(|&p| p.id == id.as_ref()) {
                                        rule.symbols.push(Symbol::Production(p.index));
                                    } else {
                                        return Err(Error::Unspecified(line!())); //undefined production
                                    }
                                }
                                Element::Action(ref a) => {
                                    if rule.action.is_empty() {
                                        rule.action = a.clone();
                                    } else {
                                        return Err(Error::Unspecified(line!())); //mid-rule actions not implemented for now
                                    }
                                }
                                Element::Fragment(_) => {
                                    return Err(Error::Unspecified(line!())); //nested fragments not implemented for now
                                }
                            }
                        }
                        rule.add_channel(channel_index);
                        rules.push(rule);
                    }
                }
            }
        }
    }

    modes.shrink_to_fit();
    channels.shrink_to_fit();
    terminals.shrink_to_fit();
    productions.shrink_to_fit();
    rules.shrink_to_fit();

    {
        let mut grammar = g.borrow_mut();
        grammar.modes = modes;
        grammar.channels = channels;
        grammar.terminals = terminals;
        grammar.productions = productions;
        grammar.rules = rules;
        grammar.globals = pg.globals;

        for m in pg.modes.iter() {
            if m.is_active() {
                let mode_index = m.mode.get();
                for rule in m.rules.iter() {
                    if rule.is_active() {
                        let t = rule.terminal.get();
                        let mut commands = LexerCommand::commands(rule.commands.iter(), &grammar)?;
                        let ref mut lexeme = grammar.terminals[t];
                        let ref mut lcmds = lexeme.commands;
                        if lcmds.is_empty() {
                            lcmds.reserve_exact(commands.len());
                            for cmd in commands.drain(..) {
                                let mut c = Command::new(cmd);
                                c.add_mode(mode_index);
                                lcmds.push(c)
                            }
                        } else {
                            for (i, cmd) in commands.drain(..).enumerate() {
                                let mut insert = true;
                                if let Some(ref mut c) = lcmds.get_mut(i) {
                                    if c.cmd == cmd {
                                        c.add_mode(mode_index);
                                        insert = false;
                                    }
                                }
                                if insert {
                                    let mut c = Command::new(cmd);
                                    c.add_mode(mode_index);
                                    lcmds.insert(i, c);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(g)
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseContext {
    Main,
    Channel(usize),
    Mode(usize),
    LexerRule,
    LexerCommands,
    ParserRule,
    Globals,
    Options,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseTerminal {
    End,
    Options,
    Globals,
    Channel,
    Mode,
    Import,
    Id(bool),
    Var(bool),
    Arrow,
    ActionStart,
    ActionEnd,
    Literal,
    True,
    False,
    Int,
    Float,
    CharSet,
    ParenLeft,
    ParenRight,
    BraceLeft,
    BraceRight,
    Colon,
    Semicolon,
    Comma,
    Dot,
    Question,
    Asterisk,
    Plus,
    Hyphen,
    Bar,
}

#[derive(Debug, Clone, Copy)]
struct ParseToken {
    term: ParseTerminal,
    from: Position,
    to: Position,
}

impl ParseToken {
    fn new(term: ParseTerminal, from: Position, to: Position) -> ParseToken {
        ParseToken {
            term,
            from,
            to,
        }
    }
}


fn skip_ws(r: &mut dyn CharReader, allow_nested_comments: bool) -> Result<bool, Error> {
    let mut level = 0;
    let mut skipped = false;
    while let Some(c) = r.peek_char(0)? {
        if c == '/' {
            if let Some(c) = r.peek_char(1)? {
                if level == 0 && c == '/' {
                    r.skip_until(&mut |c| c == '\n')?;
                    skipped = true;
                } else if c == '*' {
                    r.skip_chars(2)?;
                    if allow_nested_comments {
                        level += 1;
                    } else {
                        level = 1;
                    }
                    skipped = true;
                } else if level > 0 {
                    r.next_char()?;
                } else {
                    break;
                }
            } else {
                break;
            }
        } else if level > 0 && c == '*' {
            if let Some(c) = r.peek_char(1)? {
                if c == '/' {
                    r.skip_chars(2)?;
                    level -= 1;
                } else {
                    r.next_char()?;
                }
            } else {
                break;
            }
        } else if level > 0 || c.is_whitespace() {
            r.next_char()?;
        } else {
            break;
        }
    }
    if level > 0 {
        Err(Error::UnbalancedBlockComment)
    } else {
        Ok(skipped)
    }
}

fn parse_token(r: &mut dyn CharReader, ctx: ParseContext) -> Result<ParseToken, Error> {
    skip_ws(r, true)?;

    match r.peek_char(0)? {
        Some('<') => {
            if r.match_str("<%")? {
                let p1 = r.position();
                r.skip_chars(2)?;
                let p2 = r.position();
                Ok(ParseToken::new(ParseTerminal::ActionStart, p1, p2))
            } else {
                Err(Error::Unspecified(line!())) //FIXME (jc) invalid character '<'
            }
        }
        Some('{') => {
            let p1 = r.position();
            r.next_char()?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::BraceLeft, p1, p2))
        }
        Some('}') => {
            let p1 = r.position();
            r.next_char()?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::BraceRight, p1, p2))
        }
        Some('(') => {
            let p1 = r.position();
            r.next_char()?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::ParenLeft, p1, p2))
        }
        Some(')') => {
            let p1 = r.position();
            r.next_char()?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::ParenRight, p1, p2))
        }
        Some(':') => {
            let p1 = r.position();
            r.next_char()?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::Colon, p1, p2))
        }
        Some(';') => {
            let p1 = r.position();
            r.next_char()?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::Semicolon, p1, p2))
        }
        Some(',') => {
            let p1 = r.position();
            r.next_char()?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::Comma, p1, p2))
        }
        Some('.') => {
            let p1 = r.position();
            r.next_char()?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::Dot, p1, p2))
        }
        Some('|') => {
            let p1 = r.position();
            r.next_char()?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::Bar, p1, p2))
        }
        Some('?') => {
            let p1 = r.position();
            r.next_char()?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::Question, p1, p2))
        }
        Some('*') => {
            let p1 = r.position();
            r.next_char()?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::Asterisk, p1, p2))
        }
        Some('+') => {
            let p1 = r.position();
            r.next_char()?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::Plus, p1, p2))
        }
        Some('-') => {
            if let Some('>') = r.peek_char(1)? {
                let p1 = r.position();
                r.skip_chars(2)?;
                let p2 = r.position();
                Ok(ParseToken::new(ParseTerminal::Arrow, p1, p2))
            } else {
                let p1 = r.position();
                r.next_char()?;
                let p2 = r.position();
                Ok(ParseToken::new(ParseTerminal::Hyphen, p1, p2))
            }
        }
        Some('%') => {
            let p1 = r.position();
            r.next_char()?;
            r.skip_while(&mut |c| c.is_alphanumeric())?;
            let p2 = r.position();
            let d = r.slice(p1.offset, p2.offset)?;
            match d.as_ref() {
                "%options" => Ok(ParseToken::new(ParseTerminal::Options, p1, p2)),
                "%globals" => Ok(ParseToken::new(ParseTerminal::Globals, p1, p2)),
                "%channel" => Ok(ParseToken::new(ParseTerminal::Channel, p1, p2)),
                "%mode" => Ok(ParseToken::new(ParseTerminal::Mode, p1, p2)),
                "%import" => Ok(ParseToken::new(ParseTerminal::Import, p1, p2)),
                _ => {
                    Err(Error::Unspecified(line!()))
                },
            }
        }
        Some(c) if c.is_alphabetic() => {
            let p1 = r.position();
            r.skip_while(&mut |c| c.is_alphanumeric() || c == '_')?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::Id(c.is_uppercase()), p1, p2))
        }
        Some('@') => {
            let p1 = r.position();
            if let Some(c) = r.next_char()? {
                if c.is_alphabetic() {
                    r.skip_while(&mut |c| c.is_alphanumeric() || c == '_')?;
                    let p2 = r.position();
                    Ok(ParseToken::new(ParseTerminal::Var(c.is_uppercase()), p1, p2))
                } else {
                    Err(Error::Unspecified(line!()))
                }
            } else {
                Err(Error::Unspecified(line!()))
            }
        }
        Some(q) if q == '\'' || q == '"' => {
            let p1 = r.position();
            let mut quote = false;
            let mut term = false;
            while let Some(c) = r.next_char()? {
                if !quote && c == q {
                    term = true;
                    r.next_char()?;
                    break;
                }
                if c == '\\' {
                    if !quote {
                        quote = true;
                    }
                } else {
                    quote = false;
                }
            }
            if term {
                let p2 = r.position();
                Ok(ParseToken::new(ParseTerminal::Literal, p1, p2))
            } else {
                Err(Error::Unspecified(line!()))
            }
        }
        Some('[') => {
            let p1 = r.position();
            let mut term = false;
            while let Some(c) = r.next_char()? {
                if c == ']' {
                    term = true;
                    r.next_char()?;
                    break;
                }
            }
            if term {
                let p2 = r.position();
                Ok(ParseToken::new(ParseTerminal::CharSet, p1, p2))
            } else {
                Err(Error::Unspecified(line!()))
            }
        }
        Some(c) if c.is_digit(10) => {
            let p1 = r.position();
            r.scan(&mut |c| c.is_digit(10))?;
            let p2 = r.position();
            Ok(ParseToken::new(ParseTerminal::Int, p1, p2))
        }
        Some(c) => {
            Err(Error::Unspecified(line!()))
        }
        None => {
            Ok(ParseToken::new(ParseTerminal::End, r.position(), r.position()))
        }
    }
}


#[derive(Debug)]
struct Parser {
    pass: bool,
    tokens: Vec<ParseToken>,
    token_stack: Vec<ParseToken>,
    context_stack: Vec<ParseContext>,
}

impl Parser {
    fn new() -> Parser {
        Parser {
            pass: false,
            tokens: Vec::with_capacity(1024),
            token_stack: Vec::with_capacity(16),
            context_stack: Vec::with_capacity(16),
        }
    }

    fn parse(&mut self, r: &mut dyn CharReader) -> Result<ParseGrammar, Error> {
        let mut pg = ParseGrammar::new();
        pg.modes.push(ParseMode::new("default"));
        pg.channels.push(ParseChannel::new("default"));

        loop {
            let ctx = self.get_context();
            let t = self.next_token(r)?;
            match t.term {
                ParseTerminal::Options => {
                    match ctx {
                        ParseContext::Main => {
                            let opt = self.parse_options(r)?;
                            pg.options = opt;
                        }
                        ParseContext::Channel(c) => {
                            let opt = self.parse_options(r)?;
                            pg.channels[c].options = opt;
                        }
                        ParseContext::Mode(m) => {
                            let opt = self.parse_options(r)?;
                            pg.modes[m].options = opt;
                        }
                        _ => return Err(Error::Unspecified(line!())),
                    }
                }
                ParseTerminal::Globals => {
                    match ctx {
                        ParseContext::Main => {
                            pg.globals = Some(self.parse_globals(r)?);
                        }
                        _ => return Err(Error::Unspecified(line!())),
                    }
                }
                ParseTerminal::Channel => {
                    match ctx {
                        ParseContext::Main => {
                            let channel = self.parse_channel(r, pg.channels.len())?;
                            pg.channels.push(channel);
                        }
                        _ => return Err(Error::Unspecified(line!())),
                    }
                }
                ParseTerminal::Mode => {
                    match ctx {
                        ParseContext::Main | ParseContext::Channel(_) => {
                            let mode = self.parse_mode(r, pg.modes.len())?;
                            pg.modes.push(mode);
                        }
                        _ => return Err(Error::Unspecified(line!())),
                    }
                }
                ParseTerminal::BraceRight => {
                    match ctx {
                        ParseContext::Channel(_) | ParseContext::Mode(_) => {
                            self.pop_context();
                        }
                        _ => return Err(Error::Unspecified(line!())),
                    }
                }
                ParseTerminal::Id(true) | ParseTerminal::Var(true) | ParseTerminal::Dot | ParseTerminal::ParenLeft | ParseTerminal::CharSet | ParseTerminal::Literal => {
                    match ctx {
                        ParseContext::Main | ParseContext::Channel(_) | ParseContext::Mode(_) => {
                            self.push_token(t);
                            let rule = self.parse_lexer_rule(r)?;
                            let m = self.current_mode();
                            pg.modes[m].rules.push(rule);
                        }
                        _ => return Err(Error::Unspecified(line!())),
                    }
                }
                ParseTerminal::Id(false) | ParseTerminal::Var(false) => {
                    match ctx {
                        ParseContext::Main | ParseContext::Channel(_) => {
                            self.push_token(t);
                            let rule = self.parse_parser_rule(r)?;
                            let c = self.current_channel();
                            pg.channels[c].rules.push(rule);
                        }
                        _ => return Err(Error::Unspecified(line!())),
                    }
                }
                ParseTerminal::End => break,
                _ => return Err(Error::Unspecified(line!())),
            }
        }
        Ok(pg)
    }

    fn get_context(&self) -> ParseContext {
        if self.context_stack.is_empty() {
            ParseContext::Main
        } else {
            *self.context_stack.last().unwrap()
        }
    }

    fn pop_context(&mut self) -> ParseContext {
        self.context_stack.pop().unwrap()
    }

    fn push_context(&mut self, ctx: ParseContext) {
        self.context_stack.push(ctx)
    }

    fn next_token(&mut self, r: &mut dyn CharReader) -> Result<ParseToken, Error> {
        if self.token_stack.is_empty() {
            let t = parse_token(r, self.get_context())?;
            self.tokens.push(t);
            Ok(t)
        } else {
            Ok(self.token_stack.pop().unwrap())
        }
    }

    fn push_token(&mut self, t: ParseToken) {
        self.token_stack.push(t);
    }

    fn next_token_index(&self) -> usize {
        self.tokens.len() - self.token_stack.len()
    }

    fn expect_token(&mut self, r: &mut dyn CharReader, term: ParseTerminal) -> Result<ParseToken, Error> {
        let t = self.next_token(r)?;
        if t.term == term {
            Ok(t)
        } else {
            Err(Error::Unspecified(line!()))
        }
    }

    fn expect_tokens(&mut self, r: &mut dyn CharReader, term1: ParseTerminal, term2: ParseTerminal) -> Result<ParseToken, Error> {
        let t = self.next_token(r)?;
        if t.term == term1 || t.term == term2 {
            Ok(t)
        } else {
            Err(Error::Unspecified(line!()))
        }
    }

    fn current_channel(&self) -> usize {
        if !self.context_stack.is_empty() {
            for ctx in self.context_stack.iter().rev() {
                match *ctx {
                    ParseContext::Channel(c) => return c,
                    ParseContext::Mode(_) => break,
                    _ => {}
                }
            }
        }
        0
    }

    fn current_mode(&self) -> usize {
        if !self.context_stack.is_empty() {
            for ctx in self.context_stack.iter().rev() {
                match *ctx {
                    ParseContext::Mode(m) => return m,
                    ParseContext::Channel(_) => break,
                    _ => {}
                }
            }
        }
        0
    }

    fn parse_options(&mut self, r: &mut dyn CharReader) -> Result<Options, Error> {
        let mut opt = Options::new();
        self.push_context(ParseContext::Options);
        self.expect_token(r, ParseTerminal::BraceLeft)?;
        loop {
            let tprop = self.next_token(r)?;
            match tprop.term {
                ParseTerminal::Id(false) => {}
                ParseTerminal::BraceRight => break,
                _ => return Err(Error::Unspecified(line!())),
            }
            let prop = r.slice(tprop.from.offset, tprop.to.offset)?.to_string();
            self.expect_token(r, ParseTerminal::Colon)?;
            let tvalue = self.next_token(r)?;
            match tvalue.term {
                ParseTerminal::Id(false) => {},
                _ => return Err(Error::Unspecified(line!())),
            }
            let value = r.slice(tvalue.from.offset, tvalue.to.offset)?.to_string();
            match prop.as_ref() {
                "case_sensitive" => {
                    if value == "true" {
                        opt.case_sensitive = Some(true);
                    } else if value == "false" {
                        opt.case_sensitive = Some(false);
                    } else {
                        return Err(Error::Unspecified(line!()));
                    }
                }
                "greedy" => {
                    if value == "true" {
                        opt.greedy = Some(true);
                    } else if value == "false" {
                        opt.greedy = Some(false);
                    } else {
                        return Err(Error::Unspecified(line!()));
                    }
                }
                "strict_mode" => {
                    if value == "true" {
                        opt.strict_mode = Some(true);
                    } else if value == "false" {
                        opt.strict_mode = Some(false);
                    } else {
                        return Err(Error::Unspecified(line!()));
                    }
                }
                _ => return Err(Error::Unspecified(line!()))
            }
            let t = self.next_token(r)?;
            match t.term {
                ParseTerminal::Comma => {}
                ParseTerminal::BraceRight => break,
                _ => return Err(Error::Unspecified(line!()))
            }
        }
        self.pop_context();
        Ok(opt)
    }

    fn parse_globals(&mut self, r: &mut dyn CharReader) -> Result<Code, Error> {
        self.push_context(ParseContext::Globals);
        let t = self.expect_token(r, ParseTerminal::Action)?;
        self.pop_context();
        Ok(r.slice(t.from.offset, t.to.offset)?.to_string())
    }

    fn parse_channel(&mut self, r: &mut dyn CharReader, index: usize) -> Result<ParseChannel, Error> {
        self.push_context(ParseContext::Channel(index));
        let tname = self.expect_tokens(r, ParseTerminal::Id(false), ParseTerminal::Id(true))?;
        let mut channel = ParseChannel::new(r.slice(tname.from.offset, tname.to.offset)?.as_ref());
        let t = self.next_token(r)?;
        match t.term {
            ParseTerminal::Colon => {
                loop {
                    let tname = self.expect_tokens(r, ParseTerminal::Id(false), ParseTerminal::Id(true))?;
                    channel.parents.push(r.slice(tname.from.offset, tname.to.offset)?.to_string());
                    let t = self.next_token(r)?;
                    match t.term {
                        ParseTerminal::Comma => {},
                        ParseTerminal::BraceLeft => break,
                        _ => return Err(Error::Unspecified(line!())),
                    }
                }
            }
            ParseTerminal::BraceLeft => {}
            _ => return Err(Error::Unspecified(line!())),
        }
        Ok(channel)
    }

    fn parse_mode(&mut self, r: &mut dyn CharReader, index: usize) -> Result<ParseMode, Error> {
        self.push_context(ParseContext::Mode(index));
        let tname = self.expect_tokens(r, ParseTerminal::Id(false), ParseTerminal::Id(true))?;
        let mut mode = ParseMode::new(r.slice(tname.from.offset, tname.to.offset)?.as_ref());
        let t = self.next_token(r)?;
        match t.term {
            ParseTerminal::Colon => {
                loop {
                    let tname = self.expect_tokens(r, ParseTerminal::Id(false), ParseTerminal::Id(true))?;
                    mode.parents.push(r.slice(tname.from.offset, tname.to.offset)?.to_string());
                    let t = self.next_token(r)?;
                    match t.term {
                        ParseTerminal::Comma => {},
                        ParseTerminal::BraceLeft => break,
                        _ => return Err(Error::Unspecified(line!())),
                    }
                }
            }
            ParseTerminal::BraceLeft => {}
            _ => return Err(Error::Unspecified(line!())),
        }
        Ok(mode)
    }

    fn parse_lexer_rule(&mut self, r: &mut dyn CharReader) -> Result<LexerRule, Error> {
        self.push_context(ParseContext::LexerRule);
        let mut rule = LexerRule::new();
        rule.token_from = self.next_token_index();
        let t = self.next_token(r)?;
        match t.term {
            ParseTerminal::Id(true) => {
                rule.name = r.slice(t.from.offset, t.to.offset)?.to_string();
                rule.var = false;
                let t = self.next_token(r)?;
                match t.term {
                    ParseTerminal::Colon => {
                        // nothing to do
                    }
                    ParseTerminal::Semicolon | ParseTerminal::Arrow => {
                        self.push_token(t);
                        rule.expr.regex = Some(Regex::Empty);
                    }
                    _ => {
                        return Err(Error::Unspecified(line!()));
                    }
                }
            }
            ParseTerminal::Var(true) => {
                rule.name = r.slice(t.from.offset, t.to.offset)?.to_string();
                rule.var = true;
                self.expect_token(r, ParseTerminal::Colon)?;
            }
            ParseTerminal::Dot | ParseTerminal::ParenLeft | ParseTerminal::CharSet | ParseTerminal::Literal => {
                self.push_token(t);
            }
            _ => {
                return Err(Error::Unspecified(line!()));
            }
        }
        if !rule.expr.is_resolved() {
            rule.expr = self.parse_lexer_rule_regex(r)?;
        }
        let t = self.next_token(r)?;
        match t.term {
            ParseTerminal::Arrow => {
                rule.commands = self.parse_lexer_rule_commands(r)?;
            }
            ParseTerminal::Semicolon => {
                // nothing to do
            }
            _ => {
                return Err(Error::Unspecified(line!()));
            }
        }
        self.pop_context();
        rule.token_to = self.next_token_index() - 1;
        Ok(rule)
    }

    fn parse_lexer_rule_regex(&mut self, r: &mut dyn CharReader) -> Result<LexerRegex, Error> {
        let mut pr = LexerRegex::new();
        pr.token_from = self.next_token_index();
        loop {
            let t = self.next_token(r)?;
            match t.term {
                ParseTerminal::Semicolon | ParseTerminal::Arrow => {
                    self.push_token(t);
                    pr.token_to = self.next_token_index() - 1;
                    break;
                }
                ParseTerminal::Literal | ParseTerminal::Dot | ParseTerminal::CharSet |
                ParseTerminal::ParenLeft | ParseTerminal::ParenRight | ParseTerminal::Bar |
                ParseTerminal::Asterisk | ParseTerminal::Plus | ParseTerminal::Question |
                ParseTerminal::BraceLeft | ParseTerminal::BraceRight | ParseTerminal::Int |
                ParseTerminal::Comma |
                ParseTerminal::Var(true) | ParseTerminal::Id(true) => {
                    // continue
                },
                _ => {
                    return Err(Error::Unspecified(line!()))
                },
            }
        }
        Ok(pr)
    }

    fn resolve_lexer_rule_regex(&self, r: &mut dyn CharReader, pg: &mut ParseGrammar, mode_index: usize, rule_index: usize, mark: u32) -> Result<(), Error> {
        if !pg.modes[mode_index].rules[rule_index].expr.is_resolved() {
            if pg.modes[mode_index].rules[rule_index].expr.mark.get() == mark {
                return Err(Error::Unspecified(line!()));
            } else {
                pg.modes[mode_index].rules[rule_index].expr.mark.set(mark);
            }

            let from = pg.modes[mode_index].rules[rule_index].expr.token_from;
            let to = pg.modes[mode_index].rules[rule_index].expr.token_to;

            let mut s = String::with_capacity(1024);
            for t in &self.tokens[from..to + 1] {
                match t.term {
                    ParseTerminal::Literal => {
                        s.push('(');
                        let lit = r.slice(t.from.offset + 1, t.to.offset - 1)?;
                        for c in lit.chars() {
                            match c {
                                '.' | '-' | '+' | '*' | '?' | '[' | ']' | '{' | '}' | '(' | ')' | '^' => {
                                    s.push('\\');
                                    s.push(c);
                                }
                                _ => {
                                    s.push(c);
                                }
                            }
                        }
                        s.push(')');
                    }
                    ParseTerminal::Dot => s.push('.'),
                    ParseTerminal::CharSet => s.push_str(r.slice(t.from.offset, t.to.offset)?.as_ref()),
                    ParseTerminal::ParenLeft => s.push('('),
                    ParseTerminal::ParenRight => s.push(')'),
                    ParseTerminal::Bar => s.push('|'),
                    ParseTerminal::Asterisk => s.push('*'),
                    ParseTerminal::Plus => s.push('+'),
                    ParseTerminal::Question => s.push('?'),
                    ParseTerminal::BraceLeft => s.push('{'),
                    ParseTerminal::BraceRight => s.push('}'),
                    ParseTerminal::Int => s.push_str(r.slice(t.from.offset, t.to.offset)?.as_ref()),
                    ParseTerminal::Comma => s.push(','),
                    ParseTerminal::Var(true) | ParseTerminal::Id(true) => {
                        let mut ri = ::std::usize::MAX;
                        {
                            let name = r.slice(t.from.offset, t.to.offset)?;
                            for (i, r) in pg.modes[mode_index].rules.iter().enumerate() {
                                if r.name == name {
                                    ri = i;
                                    break;
                                }
                            }
                        }
                        if ri != ::std::usize::MAX {
                            self.resolve_lexer_rule_regex(r, pg, mode_index, ri, mark)?;
                            s.push('(');
                            s.push_str(&pg.modes[mode_index].rules[ri].expr.regex.as_ref().unwrap().to_string());
                            s.push(')');
                        } else {
                            return Err(Error::Unspecified(line!()));
                        }
                    },
                    _ => {
                        return Err(Error::Unspecified(line!()))
                    },
                }
            }
            pg.modes[mode_index].rules[rule_index].expr.regex = Some(Regex::parse(&s)?);
        }
        Ok(())
    }

    fn parse_lexer_rule_commands(&mut self, r: &mut dyn CharReader) -> Result<Vec<LexerCommand>, Error> {
        let mut cmds = Vec::new();
        self.push_context(ParseContext::LexerCommands);
        loop {
            let cmd = self.parse_lexer_rule_command(r)?;
            cmds.push(cmd);
            let t = self.next_token(r)?;
            match t.term {
                ParseTerminal::Semicolon => break,
                ParseTerminal::Comma => {},
                _ => return Err(Error::Unspecified(line!())),
            }
        }
        self.pop_context();
        Ok(cmds)
    }

    fn parse_lexer_rule_command(&mut self, r: &mut dyn CharReader) -> Result<LexerCommand, Error> {
        let t = self.next_token(r)?;
        Ok(match t.term {
            ParseTerminal::Action => {
                LexerCommand::Custom(r.slice(t.from.offset, t.to.offset)?.to_string())
            }
            ParseTerminal::Id(false) => {
                let id = r.slice(t.from.offset, t.to.offset)?.to_string();
                match id.as_ref() {
                    "skip" => LexerCommand::Skip,
                    "more" => LexerCommand::More,
                    "type" => {
                        self.expect_token(r, ParseTerminal::ParenLeft)?;
                        let tname = self.expect_token(r, ParseTerminal::Id(true))?;
                        self.expect_token(r, ParseTerminal::ParenRight)?;
                        LexerCommand::Type(r.slice(tname.from.offset, tname.to.offset)?.to_string())
                    }
                    "mode" => {
                        self.expect_token(r, ParseTerminal::ParenLeft)?;
                        let tname = self.expect_tokens(r, ParseTerminal::Id(true), ParseTerminal::Id(false))?;
                        self.expect_token(r, ParseTerminal::ParenRight)?;
                        LexerCommand::Mode(r.slice(tname.from.offset, tname.to.offset)?.to_string())
                    }
                    "mode_push" => {
                        self.expect_token(r, ParseTerminal::ParenLeft)?;
                        let tname = self.expect_tokens(r, ParseTerminal::Id(true), ParseTerminal::Id(false))?;
                        self.expect_token(r, ParseTerminal::ParenRight)?;
                        LexerCommand::ModePush(r.slice(tname.from.offset, tname.to.offset)?.to_string())
                    }
                    "mode_pop" => LexerCommand::ModePop,
                    "channel" => {
                        self.expect_token(r, ParseTerminal::ParenLeft)?;
                        let tname = self.expect_tokens(r, ParseTerminal::Id(true), ParseTerminal::Id(false))?;
                        self.expect_token(r, ParseTerminal::ParenRight)?;
                        LexerCommand::Channel(r.slice(tname.from.offset, tname.to.offset)?.to_string())
                    }
                    "channel_push" => {
                        self.expect_token(r, ParseTerminal::ParenLeft)?;
                        let tname = self.expect_tokens(r, ParseTerminal::Id(true), ParseTerminal::Id(false))?;
                        self.expect_token(r, ParseTerminal::ParenRight)?;
                        LexerCommand::ChannelPush(r.slice(tname.from.offset, tname.to.offset)?.to_string())
                    }
                    "channel_pop" => LexerCommand::ChannelPop,
                    _ => return Err(Error::Unspecified(line!())),
                }
            },
            _ => return Err(Error::Unspecified(line!())),
        })
    }

    fn parse_parser_rule(&mut self, r: &mut dyn CharReader) -> Result<ParserRule, Error> {
        self.push_context(ParseContext::ParserRule);
        let mut rule = ParserRule::new();
        let t = self.next_token(r)?;
        match t.term {
            ParseTerminal::Id(false) => {
                rule.name = r.slice(t.from.offset, t.to.offset)?.to_string();
                rule.var = false;
            }
            ParseTerminal::Var(false) => {
                rule.name = r.slice(t.from.offset, t.to.offset)?.to_string();
                rule.var = true;
            }
            _ => {
                return Err(Error::Unspecified(line!()));
            }
        }
        self.expect_token(r, ParseTerminal::Colon)?;
        loop {
            let frag = self.parse_fragment(r)?;
            rule.fragments.push(frag);
            let t = self.next_token(r)?;
            match t.term {
                ParseTerminal::Semicolon => {
                    break;
                }
                ParseTerminal::Bar => {
                    // nothing to do
                }
                _ => {
                    return Err(Error::Unspecified(line!()));
                }
            }
        }
        self.pop_context();
        Ok(rule)
    }

    fn parse_fragment(&mut self, r: &mut dyn CharReader) -> Result<Fragment, Error> {
        let mut f = Fragment::new();
        loop {
            let t = self.next_token(r)?;
            match t.term {
                ParseTerminal::Semicolon | ParseTerminal::Bar => {
                    self.push_token(t);
                    break;
                }
                ParseTerminal::Action => {
                    f.elements.push(Element::Action(r.slice(t.from.offset, t.to.offset)?.to_string()));
                }
                ParseTerminal::Literal => {
                    f.elements.push(Element::Literal(r.slice(t.from.offset, t.to.offset)?.to_string()));
                }
                ParseTerminal::Id(true) => {
                    f.elements.push(Element::Terminal(r.slice(t.from.offset, t.to.offset)?.to_string()));
                }
                ParseTerminal::Id(false) => {
                    f.elements.push(Element::Production(r.slice(t.from.offset, t.to.offset)?.to_string()));
                }
                _ => {
                    return Err(Error::Unspecified(line!()));
                }
            }
        }
        Ok(f)
    }
}


#[derive(Debug)]
struct ParseGrammar {
    modes: Vec<ParseMode>,
    channels: Vec<ParseChannel>,
    options: Options,
    globals: Option<Code>,
}

impl ParseGrammar {
    fn new() -> ParseGrammar {
        ParseGrammar {
            modes: Vec::with_capacity(32),
            channels: Vec::with_capacity(32),
            options: Options::default(),
            globals: None,
        }
    }
}


#[derive(Debug)]
struct Options {
    case_sensitive: Option<bool>,
    greedy: Option<bool>,
    strict_mode: Option<bool>,
}

impl Options {
    fn new() -> Options {
        Options {
            case_sensitive: None,
            greedy: None,
            strict_mode: None,
        }
    }
}

impl Default for Options {
    fn default() -> Options {
        Options {
            case_sensitive: Some(true),
            greedy: Some(false),
            strict_mode: Some(false),
        }
    }
}


#[derive(Debug)]
struct ParseChannel {
    name: String,
    parents: Vec<String>,
    options: Options,
    rules: Vec<ParserRule>,
    channel: Cell<usize>,
}

impl ParseChannel {
    fn new(name: &str) -> ParseChannel {
        ParseChannel {
            name: name.to_string(),
            parents: Vec::new(),
            options: Options::new(),
            rules: Vec::new(),
            channel: Cell::new(::std::usize::MAX),
        }
    }

    fn is_empty(&self) -> bool {
        self.rules.is_empty()
    }

    fn is_active(&self) -> bool {
        self.channel.get() != ::std::usize::MAX
    }
}


#[derive(Debug)]
struct ParseMode {
    name: String,
    parents: Vec<String>,
    options: Options,
    rules: Vec<LexerRule>,
    mode: Cell<usize>,
}

impl ParseMode {
    fn new(name: &str) -> ParseMode {
        ParseMode {
            name: name.to_string(),
            parents: Vec::new(),
            options: Options::new(),
            rules: Vec::new(),
            mode: Cell::new(::std::usize::MAX),
        }
    }

    fn is_empty(&self) -> bool {
        self.rules.is_empty()
    }

    fn is_active(&self) -> bool {
        self.mode.get() != ::std::usize::MAX
    }
}


#[derive(Debug)]
struct LexerRule {
    name: String,
    expr: LexerRegex,
    commands: Vec<LexerCommand>,
    var: bool,
    token_from: usize,
    token_to: usize,
    terminal: Cell<usize>,
}

impl LexerRule {
    fn new() -> LexerRule {
        LexerRule {
            name: String::new(),
            expr: LexerRegex::new(),
            commands: Vec::new(),
            var: false,
            token_from: 0,
            token_to: 0,
            terminal: Cell::new(0),
        }
    }

    fn is_active(&self) -> bool {
        self.terminal.get() > 0
    }
}


#[derive(Debug)]
struct LexerRegex {
    token_from: usize,
    token_to: usize,
    regex: Option<Regex>,
    mark: Cell<u32>,
}

impl LexerRegex {
    fn new() -> LexerRegex {
        LexerRegex {
            token_from: 0,
            token_to: 0,
            regex: None,
            mark: Cell::new(0),
        }
    }

    fn is_resolved(&self) -> bool {
        self.regex.is_some()
    }
}


#[derive(Debug, PartialEq, Eq)]
enum LexerCommand {
    Skip,
    More,
    Type(String),
    Mode(String),
    ModePush(String),
    ModePop,
    Channel(String),
    ChannelPush(String),
    ChannelPop,
    Custom(String),
}

impl LexerCommand {
    fn commands<'a, I>(cmds: I, g: &Grammar) -> Result<Vec<Cmd>, Error>
        where I: Iterator<Item = &'a LexerCommand>
    {
        let mut commands = Vec::new();
        for c in cmds {
            commands.push(c.convert(g)?);
        }
        Ok(commands)
    }

    fn convert(&self, g: &Grammar) -> Result<Cmd, Error> {
        fn find_terminal<'a>(id: &str, g: &'a Grammar) -> Result<&'a Lexeme, Error> {
            g.terminals().iter().find(|t| t.id() == id).ok_or(Error::Unspecified(line!()))
        }

        fn find_mode<'a>(id: &str, g: &'a Grammar) -> Result<&'a Mode, Error> {
            g.modes().iter().find(|m| m.id() == id).ok_or(Error::Unspecified(line!()))
        }

        fn find_channel<'a>(id: &str, g: &'a Grammar) -> Result<&'a Channel, Error> {
            g.channels().iter().find(|c| c.id() == id).ok_or(Error::Unspecified(line!()))
        }

        Ok(match *self {
            LexerCommand::Skip => {
                Cmd::Skip
            },
            LexerCommand::More => {
                Cmd::More
            },
            LexerCommand::Type(ref id) => {
                Cmd::Type(find_terminal(id, g)?.index())
            },
            LexerCommand::Mode(ref id) => {
                Cmd::Mode(find_mode(id, g)?.index())
            },
            LexerCommand::ModePush(ref id) => {
                Cmd::ModePush(find_mode(id, g)?.index())
            },
            LexerCommand::ModePop => {
                Cmd::ModePop
            },
            LexerCommand::Channel(ref id) => {
                Cmd::Channel(find_channel(id, g)?.index())
            },
            LexerCommand::ChannelPush(ref id) => {
                Cmd::ChannelPush(find_channel(id, g)?.index())
            },
            LexerCommand::ChannelPop => {
                Cmd::ChannelPop
            },
            LexerCommand::Custom(ref s) => {
                Cmd::Custom(s.clone())
            },
        })
    }
}


#[derive(Debug)]
struct ParserRule {
    name: String,
    fragments: Vec<Fragment>,
    var: bool,
    production: Cell<usize>,
}

impl ParserRule {
    fn new() -> ParserRule {
        ParserRule {
            name: String::new(),
            fragments: Vec::new(),
            var: false,
            production: Cell::new(0),
        }
    }

    fn is_active(&self) -> bool {
        self.production.get() > 0
    }
}


#[derive(Debug)]
struct Fragment {
    elements: Vec<Element>,
    repeat: Repeat,
}

impl Fragment {
    fn new() -> Fragment {
        Fragment {
            elements: Vec::new(),
            repeat: Repeat::One,
        }
    }
}


#[derive(Debug)]
enum Repeat {
    One,
    OneOrMany,
    ZeroOrOne,
    ZeroOrMany,
}


#[derive(Debug)]
enum Element {
    Literal(String),
    Terminal(String),
    Production(String),
    Action(String),
    Fragment(Fragment),
}

