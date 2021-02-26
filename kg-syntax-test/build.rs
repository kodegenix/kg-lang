use kg_syntax_gen::gen_lexer;
use std::path::Path;

fn main() {
    gen_lexer(
        "src/num.lex".as_ref(),
        Path::new(&std::env::var("OUT_DIR").unwrap())
            .join("num.rs")
            .as_ref(),
    )
    .unwrap();
}
