pub(crate) const DFA_LEXER_TPL: &'static str = r#"
use kg_diag::{CharReader, LexTerm, LexToken, ParseDiag};

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

    pub fn next_token(&mut self, reader: &mut dyn CharReader) -> Result<Token, Error> {
        let start_pos = reader.position();
        let mut s: ${state_type} = 0;
        loop {
            match reader.peek_char(0)? {
                Some(c) => {
                    s = TRANS[s as usize][c as u8 as usize];
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
                    reader.next_char()?;
                },
                None => return Ok(Token::new(Term::End, start_pos, start_pos)),
            }
        }
    }
}

const NUM_STATES: usize = ${num_states};
const TRANS: [[${state_type}; 256]; NUM_STATES] = ${trans_tab};
"#;
