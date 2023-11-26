#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Semi,
    Comma,
    Colon,
    Eq,
    Str(String),
    IdentTok(String),
    Int(i64),
    LetTok,
    FnTok,
    Plus,
}
