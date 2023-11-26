#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OpToken {
    Plus,
    Minus,
    Star,
    Slash,
}

// TODO: stop importing * everywhere and make the names sane
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
    OpTok(OpToken),
}
