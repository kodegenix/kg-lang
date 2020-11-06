use super::*;
use crate::tpl_dfa::DFA_LEXER_TPL;
use kg_syntax::EMPTY_GOTO;
use std::fmt::Write;


#[derive(Debug, Clone)]
pub struct LexerDef {
    pub name: String,
    pub lexemes: Vec<LexemeDef>,
    pub default_action: Option<String>,
}

#[derive(Debug, Clone)]
pub struct LexemeDef {
    pub name: String,
    pub label: Option<String>,
    pub regex: Regex,
    pub action: Option<String>,
}

pub fn build_dfa(lexer: &LexerDef) -> Dfa {
    let mut progs = Vec::with_capacity(lexer.lexemes.len());
    for (i, lexeme) in lexer.lexemes.iter().enumerate() {
        let p = Program::from_regex(&lexeme.regex, i + 1);
        progs.push(p);
    }
    let p = Program::merge(&progs);
    let nfa = Nfa::from_program(&p);
    Dfa::from_nfa(&nfa)
}

pub fn gen_dfa_lexer(lexer: &LexerDef, dfa: &Dfa) -> std::io::Result<String> {
    use regex::{Regex, Captures};
    use std::borrow::Cow;

    lazy_static! {
        static ref VAR_RE: Regex = Regex::new(r"\$\{(.+?)\}").unwrap();
    }

    let num_states = dfa.states().len();

    let mut tab = String::with_capacity(10 * 1024);
    tab.push_str("[\n");
    for s in dfa.states() {
        tab.push_str("\t[");

        for (i, e) in s.edges().iter().cloned().enumerate() {
            if i % 16 == 0 {
                write!(tab, "\n\t").unwrap();
            }
            if e != EMPTY_GOTO {
                write!(tab, "{:4},", e).unwrap();
            } else {
                match s.accept() {
                    Some(a) => write!(tab, "{:4},", -(a.matching() as i32)).unwrap(),
                    None => write!(tab, "{:4},", -(num_states as i32)).unwrap(),
                }
            }
        }

        tab.push_str("\n\t],\n");
    }
    tab.push_str("]");

    let mut terms = String::with_capacity(10 * 1024);
    for l in lexer.lexemes.iter() {
        write!(terms, "\n\t#[display(\"{}\")]\n\t{},", l.label.as_ref().unwrap_or(&l.regex.to_string()), l.name).unwrap();
    }

    let mut actions = String::with_capacity(10 * 1024);
    for (i, l) in lexer.lexemes.iter().enumerate() {
        if let Some(a) = l.action.as_ref() {
            write!(actions, "{} => {}\n", i + 1, a).unwrap();
        } else {
            write!(actions, "{} => return Ok(Token::new(Term::{}, start_pos, end_pos)),\n", i + 1, &l.name).unwrap();
        }
    }
    if let Some(ref a) = lexer.default_action {
        write!(actions, "{} => {}\n", num_states, a).unwrap();
    } else {
        write!(actions, "{} => panic!(format!(\"unrecognized char {{:?}}\", c)),\n", num_states).unwrap();
    }

    let code = VAR_RE.replace_all(DFA_LEXER_TPL.trim(), |c: &Captures| {
        if let Some(m) = c.get(1) {
            let prev_line_pos = (&DFA_LEXER_TPL[..m.start()]).rfind('\n').unwrap_or(m.start());
            let prefix = &DFA_LEXER_TPL[prev_line_pos + 1..m.start() - 1];
            let mut ws = String::new();
            if prefix.trim().is_empty() {
                ws.push('\n');
                ws.push_str(prefix);
            } else {
                ws.push('\n');
            };
            match m.as_str() {
                "name" => Cow::Borrowed(lexer.name.as_str()),
                "term_list" => Cow::Borrowed(terms.as_str()),
                "action_list" => Cow::Owned(actions.trim().split('\n').collect::<Vec<&str>>().join(&ws)),
                "num_states" => Cow::Owned(num_states.to_string()),
                "state_type" => Cow::Borrowed("i8"),
                "trans_tab" => Cow::Borrowed(tab.as_str()),
                s @ _ => panic!(format!("unknown variable '{}'", s)),
            }
        } else {
            unreachable!();
        }
    }).into_owned();

    Ok(code)
}
