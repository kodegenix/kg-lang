use kg_diag::{CharReader, MemCharReader, ParseDiag};

use super::*;
use super::Regex::*;

#[derive(Debug)]
pub enum Error {
    Unspecified(u32),
    IoError,
}

impl From<ParseDiag> for Error {
    fn from(_: ParseDiag) -> Error {
        Error::IoError
    }
}


#[derive(Debug, Clone, Copy)]
struct Flags {
    icase: bool,
}

impl Flags {
    fn new() -> Flags {
        Flags { icase: false }
    }
}


#[derive(Debug)]
enum ParseExpr {
    Regex(Regex),
    Group {
        flags: Flags,
    },
}

pub struct Parser {
    stack: Vec<ParseExpr>,
    flags: Flags,
}

impl Parser {
    pub fn parse(input: &str) -> Result<Regex, Error> {
        if input.is_empty() {
            Ok(Regex::Empty)
        } else {
            Parser {
                stack: vec![],
                flags: Flags::new(),
            }.parse_regex(input)
        }
    }

    fn parse_regex(mut self, input: &str) -> Result<Regex, Error> {
        let mut r = MemCharReader::new(input.as_bytes());
        while let Some(c) = r.next_char()? {
            let expr = match c {
                '|' => self.alternate()?,
                '(' => self.group_open()?,
                ')' => self.group_close()?,
                '[' => self.char_set(&mut r)?,
                '?' | '*' | '+' | '{' => self.repeat(&mut r)?,
                '.' => ParseExpr::Regex(Any),
                '\\' => {
                    ParseExpr::Regex(Literal {
                        chars: vec![self.escape(&mut r)?],
                        icase: self.flags.icase,
                    })
                }
                _ => {
                    ParseExpr::Regex(Literal {
                        chars: vec![c],
                        icase: self.flags.icase,
                    })
                }
            };

            self.stack.push(expr);
        }

        let mut nes = Vec::new();
        while let Some(e) = self.stack.pop() {
            match e {
                ParseExpr::Regex(Alternate { mut es }) => {
                    if !nes.is_empty() {
                        es.push(Parser::concat(nes)?);
                        nes = Vec::new();
                    }
                    nes.push(Alternate { es: es });
                }
                ParseExpr::Regex(e @ _) => nes.push(e),
                _ => return Err(Error::Unspecified(line!())),
            }
        }
        Parser::concat(nes)?.simplify(1000)
    }

    fn escape(&mut self, r: &mut dyn CharReader) -> Result<char, Error> {
        if let Some(c) = r.next_char()? {
            match c {
                '\\' | '.' | '-' | '+' | '*' | '?' | '[' | ']' | '{' | '}' | '(' | ')' | '^' => {
                    return Ok(c)
                }
                'a' => return Ok('\x07'),
                'f' => return Ok('\x0C'),
                't' => return Ok('\t'),
                'n' => return Ok('\n'),
                'r' => return Ok('\r'),
                'v' => return Ok('\x0B'),
                // '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7' => self.parse_octal(),
                // 'x' => { self.bump(); self.parse_hex() }
                // 'p'|'P' => {
                // self.bump();
                // self.parse_unicode_class(c == 'P')
                // .map(|cls| Build::Expr(Expr::Class(cls)))
                // }
                // 'd'|'s'|'w'|'D'|'S'|'W' => {
                // self.bump();
                // Ok(Build::Expr(Expr::Class(self.parse_perl_class(c))))
                // }
                _ => return Err(Error::Unspecified(line!())),
            }
        }
        return Err(Error::Unspecified(line!()));
    }

    fn alternate(&mut self) -> Result<ParseExpr, Error> {
        let mut nes = Vec::new();
        while let Some(e) = self.stack.pop() {
            match e {
                ParseExpr::Group { .. } => {
                    self.stack.push(e);
                    break;
                }
                ParseExpr::Regex(Alternate { mut es }) => {
                    if !nes.is_empty() {
                        es.push(Parser::concat(nes)?);
                    }
                    return Ok(ParseExpr::Regex(Alternate { es: es }));
                }
                ParseExpr::Regex(e @ _) => {
                    nes.push(e);
                }
            }
        }
        Ok(ParseExpr::Regex(Alternate { es: vec!(Parser::concat(nes)?) }))
    }

    fn group_open(&mut self) -> Result<ParseExpr, Error> {
        Ok(ParseExpr::Group { flags: self.flags })
    }

    fn group_close(&mut self) -> Result<ParseExpr, Error> {
        let mut nes = Vec::new();
        while let Some(e) = self.stack.pop() {
            match e {
                ParseExpr::Group { flags } => {
                    self.flags = flags;
                    return Ok(ParseExpr::Regex(Concat { es: vec!(Parser::concat(nes)?) }));
                }
                ParseExpr::Regex(Alternate { mut es }) => {
                    if !nes.is_empty() {
                        es.push(Parser::concat(nes)?);
                        nes = Vec::new();
                    }
                    nes.push(Alternate { es: es });
                }
                ParseExpr::Regex(e @ _) => {
                    nes.push(e);
                }
            }
        }
        Err(Error::Unspecified(line!()))
    }

    fn repeat(&mut self, r: &mut dyn CharReader) -> Result<ParseExpr, Error> {
        fn scan_repeat(r: &mut dyn CharReader) -> Result<(u32, u32), Error> {
            let mut min = 0;
            let mut max = 0;
            let mut n = 0;
            while let Some(c) = r.next_char()? {
                if c >= '0' && c <= '9' {
                    if n < 2 {
                        n = 1;
                        min = min * 10 + (c as u32 - '0' as u32);
                    } else {
                        max = max * 10 + (c as u32 - '0' as u32);
                    }
                } else if c == ',' {
                    if n == 1 {
                        n = 2;
                    } else {
                        return Err(Error::Unspecified(line!()));
                    }
                } else if c == '}' {
                    if n == 0 {
                        return Err(Error::Unspecified(line!()));
                    } else if n == 1 {
                        max = min;
                    }
                    return Ok((min, max));
                }
            }
            Err(Error::Unspecified(line!()))
        }

        if self.stack.is_empty() {
            Err(Error::Unspecified(line!()))
        } else {
            let min: u32;
            let max: u32;
            let mut greedy = true;

            match r.peek_char(0)?.unwrap() {
                '?' => {
                    min = 0;
                    max = 1;
                }
                '+' => {
                    min = 1;
                    max = 0;
                }
                '*' => {
                    min = 0;
                    max = 0;
                }
                _ => {
                    let r = scan_repeat(r)?;
                    min = r.0;
                    max = r.1;
                }
            }

            if let Some('?') = r.peek_char(1)? {
                greedy = false;
                r.skip_chars(1)?;
            }

            match self.stack.pop().unwrap() {
                ParseExpr::Regex(r @ _) => {
                    Ok(ParseExpr::Regex(Repeat {
                        e: Box::new(r),
                        min: min,
                        max: max,
                        greedy: greedy,
                    }))
                }
                _ => {
                    Err(Error::Unspecified(line!()))
                }
            }
        }
    }

    fn char_set(&mut self, r: &mut dyn CharReader) -> Result<ParseExpr, Error> {
        let mut set = CharSet::new();
        let mut p = '\0';
        let mut range = false;
        let mut found = false;
        let mut exclude = false;

        while let Some(mut c) = r.next_char()? {
            match c {
                '^' if !found => {
                    exclude = true;
                    continue;
                }
                ']' => {
                    if range {
                        return Err(Error::Unspecified(line!()));
                    } else {
                        break;
                    }
                }
                '-' => {
                    if !found {
                        return Err(Error::Unspecified(line!()));
                    } else {
                        range = true;
                        continue;
                    }
                }
                '\\' => {
                    c = self.escape(r)?;
                }
                _ => {}
            }

            found = true;
            if range {
                if exclude {
                    set.remove_range(p, c);
                } else {
                    set.add_range(p, c);
                }
                range = false;
            } else {
                if exclude {
                    set.remove_char(c);
                } else {
                    set.add_char(c);
                }
            }
            p = c;
        }

        if !set.is_empty() {
            Ok(ParseExpr::Regex(Set { set: set }))
        } else {
            Err(Error::Unspecified(line!()))
        }
    }

    fn concat(mut es: Vec<Regex>) -> Result<Regex, Error> {
        match es.len() {
            0 => Err(Error::Unspecified(line!())),
            1 => Ok(es.pop().unwrap()),
            _ => {
                es.reverse();
                Ok(Concat { es: es })
            }
        }
    }
}
