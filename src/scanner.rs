use std::error;
use std::fmt;
use std::io;
use std::io::BufRead;
use std::iter;
use std::iter::Iterator;
use std::string;
use thiserror::Error;

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
    typ: TokenType,
    text: String,
}

#[derive(Error, Debug)]
pub enum ErrorType {
    #[error("io: {0}")]
    IOError(#[from] io::Error),
    #[error("invalid identifier: {0}")]
    UTF8Error(#[from] string::FromUtf8Error),
    #[error("unexpected eof")]
    UnexpectedEOF,
    #[error("invalid {0:?}")]
    InvalidToken(TokenType),
    #[error("unknown token: {0}")]
    UnknownToken(u8),
}

#[derive(Debug)]
pub struct Error {
    line: usize,
    col: usize,
    err: ErrorType,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.col, self.err)
    }
}

impl error::Error for Error {}

pub struct Scanner<R: BufRead> {
    bytes: iter::Peekable<io::Bytes<R>>,
    buf: Vec<u8>,
    line: usize,
    col: usize,
}

fn is_delim(b: u8) -> bool {
    matches!(b, b'(' | b')' | b'{' | b'}' | b',' | b';' | b'\n' | b' ')
}

fn ident_type(s: &str) -> TokenType {
    use TokenType::*;
    match s {
        "fn" => Fn,
        _ => Ident,
    }
}

impl<R: BufRead> Scanner<R> {
    fn advance(&mut self) -> Option<Result<u8, ErrorType>> {
        let b = self.bytes.next()?.map_err(|err| err.into());
        if let Ok(b) = b {
            if b == b'\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
        Some(b)
    }

    fn must_advance(&mut self) -> Result<u8, ErrorType> {
        self.advance().ok_or(ErrorType::UnexpectedEOF)?
    }

    fn peek(&mut self) -> Option<Result<u8, ErrorType>> {
        match self.bytes.peek() {
            None => None,
            Some(Err(_)) => Some(self.bytes.next()?.map_err(|e| e.into())),
            Some(Ok(b)) => Some(Ok(*b)),
        }
    }

    fn must_peek(&mut self) -> Result<u8, ErrorType> {
        self.peek().ok_or(ErrorType::UnexpectedEOF)?
    }

    fn str(&mut self) -> Result<Token, ErrorType> {
        self.buf.clear();
        loop {
            let b = self.must_advance()?;
            if b == b'"' {
                break;
            } else {
                self.buf.push(b);
            }
        }
        self.emit(TokenType::Str)
    }

    fn keyword_or_ident(&mut self) -> Result<Token, ErrorType> {
        loop {
            let r = self.must_peek()?;
            if is_delim(r) {
                break;
            } else {
                let r = self.must_advance().unwrap();
                self.buf.push(r);
            }
        }
        let text = String::from_utf8(self.buf.split_off(0))?;
        self.emit_text(ident_type(&text), text)
    }

    fn eat(&mut self, want: u8, typ: TokenType) -> Result<u8, ErrorType> {
        self.eat_if(|b| b == want, typ)
    }

    fn eat_if(
        &mut self,
        f: impl Fn(u8) -> bool,
        typ: TokenType,
    ) -> Result<u8, ErrorType> {
        match self.must_advance()? {
            got if f(got) => Ok(got),
            _ => Err(ErrorType::InvalidToken(typ)),
        }
    }

    fn emit_text(
        &mut self,
        typ: TokenType,
        text: String,
    ) -> Result<Token, ErrorType> {
        Ok(Token { typ, text })
    }

    fn emit(&mut self, typ: TokenType) -> Result<Token, ErrorType> {
        let text = String::from_utf8(self.buf.split_off(0))?;
        self.emit_text(typ, text)
    }
}

impl<R: BufRead> Iterator for Scanner<R> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        use TokenType::*;
        loop {
            let line = self.line;
            let col = self.col;
            self.buf.clear();
            let b = match self.advance()? {
                Ok(b) => b,
                Err(err) => return Some(Err(Error { err, line, col })),
            };
            self.buf.push(b);
            let result = match b {
                b if b.is_ascii_whitespace() => continue,
                b'(' => self.emit(Lparen),
                b')' => self.emit(Rparen),
                b'{' => self.emit(Lbrace),
                b'}' => self.emit(Rbrace),
                b',' => self.emit(Comma),
                b';' => self.emit(Semi),
                b'"' => self.str(),
                b if b.is_ascii_alphabetic() => self.keyword_or_ident(),
                b => Err(ErrorType::UnknownToken(b)),
            };
            return Some(match result {
                Err(err) => Err(Error { line, col, err }),
                Ok(tok) => Ok(tok),
            });
        }
    }
}

pub fn scan<R: BufRead>(r: R) -> Scanner<R> {
    Scanner { bytes: r.bytes().peekable(), buf: Vec::new(), line: 1, col: 1 }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scan_all(b: &[u8]) -> Result<Vec<Token>, Error> {
        let scanner = scan(b);
        scanner.collect()
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
        use TokenType::*;
        let input = b"
            fn foo(bar, baz) {
                println(\"hello, world\");
            }
        ";
        let toks = scan_all(input).unwrap();
        let expected = vec![
            Token { typ: Fn, text: String::from("fn") },
            Token { typ: Ident, text: String::from("foo") },
            Token { typ: Lparen, text: String::from("(") },
            Token { typ: Ident, text: String::from("bar") },
            Token { typ: Comma, text: String::from(",") },
            Token { typ: Ident, text: String::from("baz") },
            Token { typ: Rparen, text: String::from(")") },
            Token { typ: Lbrace, text: String::from("{") },
            Token { typ: Ident, text: String::from("println") },
            Token { typ: Lparen, text: String::from("(") },
            Token { typ: Str, text: String::from("hello, world") },
            Token { typ: Rparen, text: String::from(")") },
            Token { typ: Semi, text: String::from(";") },
            Token { typ: Rbrace, text: String::from("}") },
        ];
        assert_eq!(expected, toks);
    }
}
