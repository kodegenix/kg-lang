use super::*;

pub mod clr;
pub mod glr;


#[derive(Debug)]
pub enum ParserError {
    Unspecified(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Step {
    Accept,
    Shift,
    Reduce(usize),
}

pub trait Parser {
    fn reset(&mut self);

    fn parse(&mut self, token: &Token) -> Result<Step, ParserError>;

    fn channel(&self) -> usize;
}
