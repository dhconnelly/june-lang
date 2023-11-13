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
    Ident(String),
    Colon,
    Int(i64),
}
