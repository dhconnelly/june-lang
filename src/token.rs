#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
}

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

impl Token {
    pub fn as_op(&self) -> Option<Op> {
        if let Token::Op(op) = self {
            Some(*op)
        } else {
            None
        }
    }

    pub fn as_ident(&self) -> Option<String> {
        if let Token::Ident(name) = self {
            Some(name.to_owned())
        } else {
            None
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
