use super::*;

pub struct LexerDef {
    lexemes: Vec<LexemeDef>,
}

pub struct LexemeDef {
    index: usize,
    name: String,
    regex: Regex,
    action: String,
}

