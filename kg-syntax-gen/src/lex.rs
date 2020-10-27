use super::*;

#[derive(Debug, Clone)]
pub struct LexerDef {
    pub lexemes: Vec<LexemeDef>,
}

#[derive(Debug, Clone)]
pub struct LexemeDef {
    pub name: String,
    pub label: Option<String>,
    pub regex: Regex,
    pub action: Option<String>,
}

