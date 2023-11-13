#[derive(Debug, PartialEq, Eq)]
pub enum TokenCargo {
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

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub typ: TokenCargo,
    pub line: usize,
    pub col: usize,
}
