#![feature(min_specialization)]

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate kg_diag_derive;
#[macro_use]
extern crate kg_display_derive;

use kg_diag::*;
use kg_diag::io::fs;
use kg_syntax::Regex;
use kg_syntax::prog::Program;
use kg_syntax::dfa::Dfa;
use kg_syntax::nfa::Nfa;

mod lex;
mod parse;
mod tpl_dfa;
mod tpl_nfa;

use self::lex::*;
use self::parse::*;

use std::path::Path;
use inflector::Inflector;

pub fn gen_lexer(input_path: &Path, output_path: &Path) -> Result<(), Error> {
    let f = FileBuffer::open(input_path)?;
    let mut r = f.char_reader();
    let mut l = self::parse::parse_lexer_def(&mut r)?;
    if l.name.is_empty() {
        l.name = input_path.file_stem().map(|s| s.to_str().unwrap_or("")).unwrap_or("").into();
    }
    l.name = l.name.to_class_case();
    let dfa = build_dfa(&l);
    let code = gen_dfa_lexer(&l, &dfa).map_err(|e| ParseDiag::new(IoErrorDetail::from(e.kind())))?;
    fs::write(output_path, code)?;
    Ok(())
}
