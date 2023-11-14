#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    FnTok,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Semi,
    Comma,
    Str(String),
    IdentTok(String),
    Colon,
    Int(i64),
}
