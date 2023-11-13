#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Fn,
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
