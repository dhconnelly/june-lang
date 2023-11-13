use crate::token::*;
use std::io;
use std::iter;
use std::num;
use std::result;
use std::string;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
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
    IntError(#[from] num::ParseIntError),
}

impl From<&io::Error> for Error {
    fn from(err: &io::Error) -> Error {
        Error::IOError(format!("{}", err))
    }
}

// TODO: figure out how to combine this with the ref implementation
impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::IOError(format!("{}", err))
    }
}

type Result<T> = result::Result<T, Error>;

pub struct Scanner<R: io::BufRead> {
    bytes: iter::Peekable<io::Bytes<R>>,
}

fn is_delim(b: u8) -> bool {
    !b.is_ascii_alphanumeric()
}

fn ident_type(s: &str) -> Token {
    use Token::*;
    match s {
        "fn" => Fn,
        _ => Ident(String::from(s)),
    }
}

impl<R: io::BufRead> Scanner<R> {
    fn peek(&mut self) -> Option<Result<u8>> {
        Some(self.bytes.peek()?.as_ref().map_err(Error::from).cloned())
    }

    fn advance(&mut self) -> Result<u8> {
        Ok(self.bytes.next().ok_or(Error::UnexpectedEOF)??)
    }

    fn eat(&mut self, want: u8, typ: &str) -> Result<u8> {
        if self.advance()? == want {
            Ok(want)
        } else {
            Err(Error::InvalidToken(String::from(typ)))
        }
    }

    fn advance_while<F: Fn(u8) -> bool>(&mut self, f: F) -> Result<String> {
        let mut buf = Vec::new();
        while let Some(value) = self.peek() {
            if !f(value?) {
                break;
            }
            buf.push(self.advance().unwrap());
        }
        Ok(String::from_utf8(buf)?)
    }

    fn advance_emit(&mut self, size: usize, typ: Token) -> Result<Token> {
        for _ in 0..size {
            self.advance()?;
        }
        Ok(typ)
    }

    fn str(&mut self) -> Result<Token> {
        self.eat(b'"', "str")?;
        let text = self.advance_while(|b| b != b'"')?;
        self.eat(b'"', "str")?;
        Ok(Token::Str(text))
    }

    fn int(&mut self) -> Result<Token> {
        let text = self.advance_while(|b| !is_delim(b))?;
        Ok(Token::Int(text.parse::<i64>()?))
    }

    fn keyword_or_ident(&mut self) -> Result<Token> {
        let text = self.advance_while(|b| !is_delim(b))?;
        Ok(ident_type(&text))
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek(), Some(Ok(b' ' | b'\n'))) {
            self.advance().unwrap();
        }
    }
}

impl<R: io::BufRead> iter::Iterator for Scanner<R> {
    type Item = result::Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let result = self.peek()?.and_then(|b| match b {
            b'(' => self.advance_emit(1, Token::Lparen),
            b')' => self.advance_emit(1, Token::Rparen),
            b'{' => self.advance_emit(1, Token::Lbrace),
            b'}' => self.advance_emit(1, Token::Rbrace),
            b',' => self.advance_emit(1, Token::Comma),
            b';' => self.advance_emit(1, Token::Semi),
            b':' => self.advance_emit(1, Token::Colon),
            b'"' => self.str(),
            b if b.is_ascii_digit() => self.int(),
            b if b.is_ascii_alphabetic() => self.keyword_or_ident(),
            b => Err(Error::UnknownToken(b)),
        });
        Some(result)
    }
}

pub fn scan<R: io::BufRead>(r: R) -> Scanner<R> {
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
            Rbrace,
        ];
        assert_eq!(expected, toks);
    }
}
