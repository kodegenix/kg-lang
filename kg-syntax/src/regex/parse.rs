use super::*;

use std::collections::HashMap;


//FIXME (jc)
#[derive(Debug, Clone, Detail, Display)]
#[diag(code_offset = 1200)]
pub enum ParseErrorDetail {
    #[diag(code = 1, severity = 'E')]
    #[display(fmt = "{_0}")]
    Unspecified(u32)
}

pub type Error = ParseDiag;

pub struct Parser<'a> {
    refs: HashMap<&'a str, &'a Regex>,
    allow_whitespace: bool,
    allow_refs: bool,
    stack: Vec<ParseExpr<'a>>,
    flags: Flags,
}

impl<'a> Parser<'a> {
    pub fn new() -> Parser<'a> {
        Parser {
            refs: HashMap::new(),
            allow_whitespace: false,
            allow_refs: false,
            stack: Vec::new(),
            flags: Flags::new(),
        }
    }

    pub fn with_refs(refs: HashMap<&'a str, &'a Regex>) -> Parser<'a> {
        Parser {
            refs,
            allow_whitespace: true,
            allow_refs: true,
            stack: Vec::new(),
            flags: Flags::new(),
        }
    }

    pub fn allow_whitespace(&mut self, allow: bool) -> &mut Self {
        self.allow_whitespace = allow;
        self
    }

    pub fn allow_refs(&mut self, allow: bool) -> &mut Self {
        self.allow_refs = allow;
        self
    }

    pub fn parse_str(&'a mut self, input: &str) -> Result<Regex, Error> {
        if input.is_empty() {
            Ok(Regex::Empty)
        } else {
            let mut r = MemCharReader::new(input.as_bytes());
            self.parse(&mut r)
        }
    }

    pub fn parse(&'a mut self, reader: &mut dyn CharReader) -> Result<Regex, Error> {
        if reader.eof() {
            Ok(Regex::Empty)
        } else {
            self.parse_regex(reader)
        }
    }

    fn reset(&mut self) {
        self.stack.clear();
        self.flags = Flags::new();
    }

    fn parse_regex(&'a mut self, reader: &mut dyn CharReader) -> Result<Regex, Error> {
        self.reset();
        while let Some(c) = reader.next_char()? {
            let expr = match c {
                '|' => self.alternate()?,
                '(' => self.group_open()?,
                ')' => self.group_close()?,
                '[' => self.parse_char_set(reader)?,
                '?' | '*' | '+' | '{' => self.parse_repeat(reader)?,
                '.' => ParseExpr::Regex(Regex::Any),
                '\\' => {
                    ParseExpr::Regex(Regex::Literal {
                        chars: vec![self.parse_escape(reader)?],
                        icase: self.flags.icase,
                    })
                }
                _ => {
                    ParseExpr::Regex(Regex::Literal {
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
                ParseExpr::Regex(Regex::Alternate(mut es)) => {
                    if !nes.is_empty() {
                        es.push(Parser::concat(nes)?);
                        nes = Vec::new();
                    }
                    nes.push(Regex::Alternate(es));
                }
                ParseExpr::Regex(e @ _) => nes.push(e),
                _ => return Err(ParseErrorDetail::Unspecified(line!()).into()),
            }
        }
        Parser::concat(nes)?.simplify(1000)
    }

    fn parse_escape(&mut self, r: &mut dyn CharReader) -> Result<char, Error> {
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
                _ => return Err(ParseErrorDetail::Unspecified(line!()).into()),
            }
        }
        return Err(ParseErrorDetail::Unspecified(line!()).into());
    }

    fn group_open(&mut self) -> Result<ParseExpr, Error> {
        Ok(ParseExpr::Group(self.flags))
    }

    fn group_close(&mut self) -> Result<ParseExpr, Error> {
        let mut nes = Vec::new();
        while let Some(e) = self.stack.pop() {
            match e {
                ParseExpr::Group(flags) => {
                    self.flags = flags;
                    return Ok(ParseExpr::Regex(Regex::Concat(vec!(Parser::concat(nes)?))));
                }
                ParseExpr::Regex(Regex::Alternate(mut es)) => {
                    if !nes.is_empty() {
                        es.push(Parser::concat(nes)?);
                        nes = Vec::new();
                    }
                    nes.push(Regex::Alternate(es));
                }
                ParseExpr::Regex(e @ _) => {
                    nes.push(e);
                }
            }
        }
        Err(ParseErrorDetail::Unspecified(line!()).into())
    }

    fn parse_repeat(&'a mut self, r: &mut dyn CharReader) -> Result<ParseExpr<'a>, Error> {
        fn scan_repeat(r: &mut dyn CharReader) -> Result<(u32, Option<u32>), Error> {
            let mut min = std::u32::MAX;
            let mut max = std::u32::MAX;
            let mut n = 0;
            while let Some(c) = r.next_char()? {
                if c >= '0' && c <= '9' {
                    let d = c as u32 - '0' as u32;
                    match n {
                        0 => {
                            n = 1;
                            min = d;
                        }
                        1 => {
                            min *= 10;
                            min += d;
                        }
                        2 => {
                            n = 3;
                            max = d;
                        }
                        3 => {
                            max *= 10;
                            max += d;
                        }
                        _ => unreachable!()
                    }
                } else if c == ',' {
                    if n == 1 {
                        n = 2;
                        max = 0;
                    } else {
                        return Err(ParseErrorDetail::Unspecified(line!()).into());
                    }
                } else if c == '}' {
                    if n == 0 {
                        return Err(ParseErrorDetail::Unspecified(line!()).into());
                    }
                    return Ok((min, if max < std::u32::MAX { Some(max) } else { None }));
                }
            }
            Err(ParseErrorDetail::Unspecified(line!()).into())
        }

        if self.stack.is_empty() {
            Err(ParseErrorDetail::Unspecified(line!()).into())
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
                    // max should not be less then min (if set)
                    if let Some(m) = r.1 {
                        if m != 0 && m < min {
                            return Err(ParseErrorDetail::Unspecified(line!()).into());
                        }
                        max = m;
                    } else {
                        max = min;
                    }
                }
            }

            if let Some('?') = r.peek_char(1)? {
                greedy = false;
                r.skip_chars(1)?;
            }

            match self.stack.pop().unwrap() {
                ParseExpr::Regex(r @ _) => {
                    Ok(ParseExpr::Regex(Regex::Repeat {
                        e: Box::new(r),
                        min,
                        max,
                        greedy,
                    }))
                }
                _ => {
                    Err(ParseErrorDetail::Unspecified(line!()).into())
                }
            }
        }
    }

    fn parse_char_set(&mut self, r: &mut dyn CharReader) -> Result<ParseExpr, Error> {
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
                        return Err(ParseErrorDetail::Unspecified(line!()).into());
                    } else {
                        break;
                    }
                }
                '-' => {
                    if !found {
                        return Err(ParseErrorDetail::Unspecified(line!()).into());
                    } else {
                        range = true;
                        continue;
                    }
                }
                '\\' => {
                    c = self.parse_escape(r)?;
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
            Ok(ParseExpr::Regex(Regex::Set(set)))
        } else {
            Err(ParseErrorDetail::Unspecified(line!()).into())
        }
    }

    fn alternate(&'a mut self) -> Result<ParseExpr<'a>, Error> {
        let mut nes = Vec::new();
        while let Some(e) = self.stack.pop() {
            match e {
                ParseExpr::Group { .. } => {
                    self.stack.push(e);
                    break;
                }
                ParseExpr::Regex(Regex::Alternate(mut es)) => {
                    if !nes.is_empty() {
                        es.push(Parser::concat(nes)?);
                    }
                    return Ok(ParseExpr::Regex(Regex::Alternate(es)));
                }
                ParseExpr::Regex(e @ _) => {
                    nes.push(e);
                }
            }
        }
        Ok(ParseExpr::Regex(Regex::Alternate(vec!(Parser::concat(nes)?))))
    }

    fn concat(mut es: Vec<Regex>) -> Result<Regex, Error> {
        match es.len() {
            0 => Err(ParseErrorDetail::Unspecified(line!()).into()),
            1 => Ok(es.pop().unwrap()),
            _ => {
                es.reverse();
                Ok(Regex::Concat(es))
            }
        }
    }
}


#[derive(Debug, Default, Clone, Copy)]
struct Flags {
    icase: bool,
}

impl Flags {
    fn new() -> Flags {
        Flags {
            icase: false
        }
    }
}


#[derive(Debug)]
enum ParseExpr<'a> {
    //Char(char),
    Regex(Regex),
    Group(Flags),
    Ref(&'a str),
}
