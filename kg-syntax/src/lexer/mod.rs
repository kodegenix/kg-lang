use super::*;

use self::commons::*;
use self::prog::{Program, Opcode, ProgMatcher, ProgLexer};
use self::nfa::Nfa;
use self::dfa::Dfa;

mod commons;

pub mod prog;
pub mod nfa;
pub mod dfa;


//FIXME (jc)
#[derive(Debug)]
pub enum LexerError {
    UnexpectedEof(Position),
    UnexpectedInput(Position),
    IoError,
}

//FIXME (jc)
impl From<ParseDiag> for LexerError {
    fn from(_: ParseDiag) -> LexerError {
        LexerError::IoError
    }
}

//FIXME (jc)
impl From<IoErrorDetail> for LexerError {
    fn from(_: IoErrorDetail) -> LexerError {
        LexerError::IoError
    }
}


#[derive(Debug, Clone)]
pub struct Token {
    lexeme: usize,
    id: String,
    value: String,
    start: Position,
    end: Position,
    mode: usize,
    channel: usize,
}

impl Token {
    pub fn new(lexeme: usize, value: Cow<str>,
               start: Position, end: Position) -> Token {
        Token {
            lexeme,
            value: value.into_owned(),
            start,
            end,
            ..Default::default()
        }
    }

    pub fn with_id<'a>(lexeme: usize, id: Cow<'a, str>, value: Cow<'a, str>,
                   start: Position, end: Position) -> Token {
        Token {
            lexeme,
            id: id.into_owned(),
            value: value.into_owned(),
            start,
            end,
            ..Default::default()
        }
    }

    pub fn lexeme(&self) -> usize {
        self.lexeme
    }

    pub fn set_lexeme(&mut self, lexeme: usize) {
        self.lexeme = lexeme;
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn set_id(&mut self, id: &str) {
        self.id.clear();
        self.id.push_str(id);
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    pub fn set_value(&mut self, value: String) {
        self.value = value;
    }

    pub fn set_value_str(&mut self, value: &str) {
        self.value.clear();
        self.value.push_str(value);
    }

    pub fn append_value(&mut self, value: &str) {
        self.value.push_str(value);
    }

    pub fn start(&self) -> Position {
        self.start
    }

    pub fn set_start(&mut self, start: Position) {
        self.start = start;
    }

    pub fn end(&self) -> Position {
        self.end
    }

    pub fn set_end(&mut self, end: Position) {
        self.end = end;
    }

    pub fn mode(&self) -> usize {
        self.mode
    }

    pub fn set_mode(&mut self, mode: usize) {
        self.mode = mode;
    }

    pub fn channel(&self) -> usize {
        self.channel
    }

    pub fn set_channel(&mut self, channel: usize) {
        self.channel = channel;
    }
}

impl Default for Token {
    fn default() -> Token {
        Token {
            lexeme: 0,
            id: String::new(),
            value: String::new(),
            start: Position::new(),
            end: Position::new(),
            mode: 0,
            channel: 0,
        }
    }
}

//FIXME (jc) handle errors
impl WriteJs for Token {
    fn write_js(&self, e: &mut Engine) -> i32 {
        let idx = e.push_object();
        e.push_number(self.lexeme as f64);
        e.put_prop_string(idx, "lexeme");
        e.push_string(&self.id);
        e.put_prop_string(idx, "id");
        e.push_string(&self.value);
        e.put_prop_string(idx, "value");
        e.write(&self.start);
        e.put_prop_string(idx, "start");
        e.write(&self.end);
        e.put_prop_string(idx, "end");
        e.push_number(self.mode as f64);
        e.put_prop_string(idx, "mode");
        e.push_number(self.channel as f64);
        e.put_prop_string(idx, "channel");
        idx
    }
}

//FIXME (jc) handle errors
impl ReadJs for Token {
    fn read_js(&mut self, e: &mut Engine, obj_index: i32) {
        e.get_prop_string(obj_index, "lexeme");
        self.lexeme = e.get_number(-1) as usize;
        e.get_prop_string(obj_index, "id");
        self.id = String::from(e.get_string(-1));
        e.get_prop_string(obj_index, "value");
        self.value = String::from(e.get_string(-1));
        e.get_prop_string(obj_index, "start");
        self.start.read_js_top(e);
        e.get_prop_string(obj_index, "end");
        self.end.read_js_top(e);
        e.get_prop_string(obj_index, "mode");
        self.mode = e.get_number(-1) as usize;
        e.get_prop_string(obj_index, "channel");
        self.channel = e.get_number(-1) as usize;
        e.pop_n(7);
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[{:3} m:{} c:{} {:>6}-{:<6}] ", self.lexeme, self.mode, self.channel, self.start.to_string(), self.end.to_string())?;
        if self.value.is_empty() || (self.id.len() >= 3 && self.value == &self.id[1 .. self.id.len() - 1]) {
            write!(f, "{}", self.id)?;
        } else {
            write!(f, "{}:{:?}", self.id, self.value)?;
        }
        Ok(())
    }
}


pub trait Lexer {
    fn reset(&mut self);

    fn lex(&mut self, reader: &mut dyn ByteReader) -> Result<Token, LexerError>;

    fn unmatched(&self) -> usize;

    fn mode(&self) -> usize;
}
