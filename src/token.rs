#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Op {
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
    Ident(String),
    Int(i64),
    Let,
    Fn,
    Op(Op),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
