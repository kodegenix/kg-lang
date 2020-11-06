use super::*;

pub type Error = ParseDiag;

#[derive(Debug, Display, Detail)]
pub enum ErrorDetail {
    #[display("undefined")]
    Undefined
}

pub fn parse_lexer_def(reader: &mut dyn CharReader) -> Result<LexerDef, Error> {
    let mut lexemes = Vec::new();
    let mut default_action = None;
    while let Some(lexeme) = parse_lexeme_def_opt(reader)? {
        if lexeme.regex == Regex::Empty {
            if default_action.is_some() {
                // cannot have more then one catch-all rule
                return Err(Error::new(ErrorDetail::Undefined));
            } else {
                default_action = lexeme.action;
            }
        } else {
            lexemes.push(lexeme);
        }
    }
    Ok(LexerDef {
        name: String::new(),
        lexemes,
        default_action,
    })
}

pub fn parse_lexeme_def_opt(reader: &mut dyn CharReader) -> Result<Option<LexemeDef>, Error> {
    let mut lexeme = LexemeDef {
        name: String::new(),
        label: None,
        regex: Regex::Empty,
        action: None,
    };
    reader.skip_whitespace()?;
    if let Some('.') = reader.peek_char(0)? {
        // parse default (catch all) rule
        reader.skip_chars(1)?;
        reader.skip_whitespace()?;
        if let Some(':') = reader.peek_char(0)? {
            reader.next_char()?;
            lexeme.action = parse_action(reader)?;
            if lexeme.action.is_none() {
                // catch-all rule must have an action
                return Err(Error::new(ErrorDetail::Undefined));
            }
        } else {
            // catch-all rule name '.' must be followed by ':' and action
            return Err(Error::new(ErrorDetail::Undefined));
        }
    } else {
        // parse regular rule
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
        reader.skip_whitespace()?;

        let p1 = reader.position();
        while let Some(c) = reader.next_char()? {
            if c == '\\' {
                reader.next_char()?;
            } else if c.is_whitespace() {
                break;
            }
        }
        let p2 = reader.position();

        lexeme.regex = Regex::parse(reader.slice_pos(p1, p2)?.as_ref())?;

        lexeme.action = parse_action(reader)?;
    }
    Ok(Some(lexeme))
}

fn parse_action(reader: &mut dyn CharReader) -> Result<Option<String>, Error> {
    reader.skip_whitespace_nonl()?;

    let p1 = reader.position();
    if let Some('{') = reader.peek_char(0)? {
        //FIXME (jc) skip string literals (at least quoted curly braces)
        let mut level = 1;
        while let Some(c) = reader.next_char()? {
            if c == '{' {
                level += 1;
            } else if c == '}' {
                level -= 1;
                if level == 0 {
                    break;
                }
            }
        }
        if level != 0 {
            return Err(Error::new(ErrorDetail::Undefined));
        }
    }
    reader.skip_until(&mut |c| c == '\n')?;
    let p2 = reader.position();

    let action = reader.slice_pos(p1, p2)?;
    let action = action.trim();
    Ok(if action.is_empty() {
        None
    } else {
        Some(action.to_string())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    //FIXME (jc)
    #[test]
    fn num_parse() {
        use std::path::PathBuf;
        let mut path = PathBuf::from(std::env::var_os("CARGO_MANIFEST_DIR").unwrap_or_default());
        path.push("resources/num.lex");

        let mut f = FileBuffer::open(path).unwrap();
        let mut r = f.char_reader();
        let mut l = parse_lexer_def(&mut r).unwrap();
        l.name = "Num".into();
        println!("{:#?}", l);
    }
}