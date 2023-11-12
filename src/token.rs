#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    Fn,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Semi,
    Comma,
    Str,
    Ident,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub typ: TokenType,
    pub text: String,
    pub line: usize,
    pub col: usize,
}
