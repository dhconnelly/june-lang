use crate::token::*;
use std::io;
use std::iter;
use std::result;
use std::string;
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum Error {
    #[error("io: {0}")]
    IOError(String),
    #[error("invalid identifier: {0}")]
    UTF8Error(#[from] string::FromUtf8Error),
    #[error("unexpected eof")]
    UnexpectedEOF,
    #[error("invalid {0}")]
    InvalidToken(String),
    #[error("unknown token: {0}")]
    UnknownToken(u8),
    #[error("invalid int: {0}")]
    IntError(String),
}

impl From<&io::Error> for Error {
    fn from(err: &io::Error) -> Error {
        Error::IOError(format!("{}", err))
    }
}

type Result<T> = result::Result<T, Error>;

pub struct Scanner<R: io::Read> {
    bytes: iter::Peekable<io::Bytes<R>>,
}

fn is_delim(b: u8) -> bool {
    !b.is_ascii_alphanumeric() && b != b'_'
}

impl<R: io::Read> Scanner<R> {
    fn peek(&mut self) -> Option<Result<u8>> {
        let peeked = self.bytes.peek()?;
        let result = peeked.as_ref().map_err(Error::from).map(|ch| *ch);
        Some(result)
    }

    fn advance(&mut self) -> Result<u8> {
        let result = self.bytes.next().ok_or(Error::UnexpectedEOF)?;
        result.map_err(|err| (&err).into())
    }

    fn advance_while(&mut self, f: impl Fn(u8) -> bool) -> Result<String> {
        let mut buf = Vec::new();
        while let Some(value) = self.peek() {
            if !f(value?) {
                break;
            }
            buf.push(self.advance().unwrap());
        }
        Ok(String::from_utf8(buf)?)
    }

    fn eat(&mut self, got: &[u8], want: impl ToString) -> Result<()> {
        for ch in got {
            if self.peek() != Some(Ok(*ch)) {
                return Err(Error::InvalidToken(want.to_string()));
            }
            self.advance().unwrap();
        }
        Ok(())
    }

    fn eat_as(&mut self, s: &[u8], tok: Token) -> Result<Token> {
        self.eat(s, &tok)?;
        Ok(tok)
    }

    fn str(&mut self) -> Result<Token> {
        self.eat(b"\"", "Str")?;
        let text = self.advance_while(|b| b != b'"')?;
        self.eat(b"\"", "Str")?;
        Ok(Token::Str(text))
    }

    fn int(&mut self) -> Result<Token> {
        let text = self.advance_while(|b| !is_delim(b))?;
        let int = text.parse::<i64>().map_err(|_| Error::IntError(text))?;
        Ok(Token::Int(int))
    }

    fn keyword_or_ident(&mut self) -> Result<Token> {
        let text = self.advance_while(|b| !is_delim(b))?;
        let tok = match text.as_str() {
            "fn" => Token::Fn,
            "let" => Token::Let,
            _ => Token::Ident(text),
        };
        Ok(tok)
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek(), Some(Ok(b' ' | b'\n'))) {
            self.advance().unwrap();
        }
    }
}

impl<R: io::Read> iter::Iterator for Scanner<R> {
    type Item = result::Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let b = self.peek().transpose().ok()??;
        let result = match b {
            b'+' => self.eat_as(b"+", Token::Op(Op::Plus)),
            b'=' => self.eat_as(b"=", Token::Eq),
            b'(' => self.eat_as(b"(", Token::Lparen),
            b')' => self.eat_as(b")", Token::Rparen),
            b'{' => self.eat_as(b"{", Token::Lbrace),
            b'}' => self.eat_as(b"}", Token::Rbrace),
            b',' => self.eat_as(b",", Token::Comma),
            b';' => self.eat_as(b";", Token::Semi),
            b':' => self.eat_as(b":", Token::Colon),
            b'"' => self.str(),
            b'/' => {
                self.advance().unwrap();
                if let Some(Ok(b'/')) = self.peek() {
                    self.advance().unwrap();
                    match self.advance_while(|b| b != b'\n') {
                        Ok(_) => return self.next(),
                        Err(err) => Err(err),
                    }
                } else {
                    Ok(Token::Op(Op::Slash))
                }
            }
            b if b.is_ascii_digit() => self.int(),
            b if b.is_ascii_alphabetic() => self.keyword_or_ident(),
            b => Err(Error::UnknownToken(b)),
        };
        Some(result)
    }
}

pub fn scan<R: io::Read>(r: R) -> Scanner<R> {
    Scanner { bytes: r.bytes().peekable() }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scan_all(b: &[u8]) -> Result<Vec<Token>> {
        scan(b).collect()
    }

    #[test]
    fn test_empty() {
        let input = b"
        ";
        let toks = scan_all(input).unwrap();
        let empty = Vec::<Token>::new();
        assert_eq!(empty, toks);
    }

    #[test]
    fn test() {
        use Token::*;
        let input = b"
            fn foo(bar: int, baz: str) {
                println(\"hello, world\", 27);
                {
                    let foo: int = 7;
                }
            }
        ";
        let toks = scan_all(input).unwrap();
        let expected = vec![
            Fn,
            Ident(String::from("foo")),
            Lparen,
            Ident(String::from("bar")),
            Colon,
            Ident(String::from("int")),
            Comma,
            Ident(String::from("baz")),
            Colon,
            Ident(String::from("str")),
            Rparen,
            Lbrace,
            Ident(String::from("println")),
            Lparen,
            Str(String::from("hello, world")),
            Comma,
            Int(27),
            Rparen,
            Semi,
            Lbrace,
            Let,
            Ident(String::from("foo")),
            Colon,
            Ident(String::from("int")),
            Eq,
            Int(7),
            Semi,
            Rbrace,
            Rbrace,
        ];
        assert_eq!(expected, toks);
    }
}
