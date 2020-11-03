use super::*;

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
        name: String::new(),
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
    reader.skip_whitespace()?;
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
    lexeme.action = if action.is_empty() {
        None
    } else {
        Some(action.to_string())
    };

    Ok(Some(lexeme))
}

#[cfg(test)]
mod tests {
    use super::*;

    //FIXME (jc)
    #[test]
    fn num_parse() {
        let mut f = FileBuffer::open("resources/num.lex").unwrap();
        let mut r = f.char_reader();
        let mut l = parse_lexer_def(&mut r).unwrap();
        l.name = "Num".into();
        // for l in l.lexemes.iter() {
        //     println!("{:?}", l);
        // }
        let dfa = build_dfa(&l);
        //println!("{}", dfa);

        println!("{}", gen_dfa_lexer(&l, &dfa));
    }
}