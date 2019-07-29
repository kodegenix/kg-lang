#![feature(specialization)]

use kg_diag::*;
use kg_lang::*;

#[test]
fn parse_grammar() {
    let f = FileBuffer::open("resources/java/grammar/java.grammar").unwrap();
    let mut r = f.char_reader();

    let grammar = GrammarRef::parse(&mut r).unwrap();

    println!("\n{}", grammar.borrow());
}
