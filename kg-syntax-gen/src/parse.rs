use super::*;
use std::path::{Path, PathBuf};

pub type Error = ParseDiag;

#[derive(Debug, Display, Detail)]
pub enum ErrorDetail {
    #[display("undefined")]
    Undefined
}

pub fn parse_lexer_def(reader: &mut dyn CharReader) -> Result<LexerDef, Error> {
    let mut lexemes = Vec::new();
    while let Some(lexeme) = parse_lexeme_def_opt(reader)? {
        lexemes.push(lexeme);
    }
    Ok(LexerDef {
        lexemes,
    })
}

pub fn parse_lexeme_def_opt(reader: &mut dyn CharReader) -> Result<Option<LexemeDef>, Error> {
    let mut lexeme = LexemeDef {
        name: String::new(),
        label: None,
        regex: Regex::Empty,
        action: None,
    };
    reader.skip_whitespace_nonl()?;
    lexeme.name.push_str(&reader.scan(&mut |c| c.is_alphabetic())?);
    lexeme.name.push_str(&reader.scan(&mut |c| c.is_alphanumeric())?);
    if lexeme.name.is_empty() {
        return Ok(None);
    }
    reader.skip_whitespace_nonl()?;
    match reader.peek_char(0)? {
        Some(s) if s == '"' || s == '\'' => {
            reader.next_char()?;
            let p1 = reader.position();
            reader.skip_until(&mut |c| c == s)?;
            let p2 = reader.position();
            lexeme.label = Some(reader.slice_pos(p1, p2)?.to_string());
            reader.next_char()?;
            reader.skip_whitespace_nonl()?;
        },
        Some(':') => {},
        Some(_) => return Err(Error::new(ErrorDetail::Undefined)),
        None => return Err(Error::new(ErrorDetail::Undefined)),
    }
    if !reader.match_char(':')? {
        return Err(Error::new(ErrorDetail::Undefined));
    } else {
        reader.next_char()?;
    }
    reader.skip_whitespace_nonl()?;

    reader.skip_until(&mut |c| c == '\n')?;
    reader.skip_whitespace()?;
    Ok(Some(lexeme))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn num_parse() {
        let mut f = FileBuffer::open("resources/num.lex").unwrap();
        let mut r = f.char_reader();
        let mut l = parse_lexer_def(&mut r).unwrap();
        for l in l.lexemes.iter() {
            println!("{:?}", l);
        }
    }
}