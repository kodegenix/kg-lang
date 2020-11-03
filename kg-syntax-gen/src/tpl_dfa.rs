pub(crate) const DFA_LEXER_TPL: &'static str = r#"
use kg_diag::{ByteReader, LexTerm, LexToken, ParseDiag};

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Term {
    #[display("$")]
    End, ${term_list}
}

impl LexTerm for Term {}

pub type Token = LexToken<Term>;

pub type Error = ParseDiag;

pub struct ${name}Lexer();

#[allow(unused)]
impl ${name}Lexer {
    pub fn new() -> Self {
        ${name}Lexer()
    }

    pub fn next_token(&mut self, reader: &mut dyn ByteReader) -> Result<Token, Error> {
        let start_pos = reader.position();
        if reader.eof() {
            return Ok(Token::new(Term::End, start_pos, start_pos));
        }

        let mut s: ${state_type} = 0;
        loop {
            let c = reader.peek_byte(0)?.unwrap_or(0);
            s = TRANS[s as usize][c as usize];
            if s < 0 {
                let a = -s;
                s = 0;
                let end_pos = reader.position();
                match a {
${action_list}
${num_states} => panic!("unrecognized char"),
_ => unreachable!(),
                }
            }
            reader.next_byte()?;
        }
    }
}

const TRANS: [[${state_type}; 256]; ${num_states}] = ${trans_tab};
"#;
