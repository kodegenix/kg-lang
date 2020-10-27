#![feature(min_specialization)]

#[macro_use]
extern crate kg_diag_derive;
#[macro_use]
extern crate kg_display_derive;

use kg_diag::*;
use kg_syntax::*;

mod lex;
mod parse;
mod tpl_dfa;
mod tpl_nfa;

use self::lex::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
