#![feature(min_specialization)]

use kg_diag::*;
use kg_syntax::*;

#[test]
fn parse_grammar() {
    let f = FileBuffer::open("resources/java/grammar/java.grammar").unwrap();
    let mut r = f.char_reader();

    let grammar = GrammarRef::parse(&mut r).unwrap();

    let mut runtime = JsRuntime::new(&grammar).unwrap();
    println!("\n{}", grammar.borrow());
    for lexeme in grammar.borrow().terminals().iter() {
        println!("{:?}", lexeme);
    }

    let inp = FileBuffer::open("resources/java/src/main/java/org/example/geom/Point3.java").unwrap();
    let mut ir = inp.byte_reader();

    let n = runtime.process(&mut ir).unwrap();
    println!("\n{}", n.to_yaml());

}
