use crate::token::*;
use std::io;
use std::iter;
use std::num;
use std::result;
use std::string;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ErrorType {
    #[error("io: {0}")]
    IOError(#[from] io::Error),
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

type Result<T> = result::Result<T, ErrorType>;

pub struct Scanner<R: io::BufRead> {
    bytes: iter::Peekable<io::Bytes<R>>,
    line: usize,
    col: usize,
}

fn is_delim(b: u8) -> bool {
    !b.is_ascii_alphanumeric()
}

fn ident_type(s: &str) -> TokenCargo {
    use TokenCargo::*;
    match s {
        "fn" => Fn,
        _ => Ident(s.to_owned()),
    }
}

impl<R: io::BufRead> Scanner<R> {
    fn peek(&mut self) -> Option<Result<u8>> {
        // not all errors can be copied (e.g. io::Error), so if peek has
        // an error, we go ahead and get the owned error via advance().
        match self.bytes.peek() {
            Some(Err(_)) => Some(Err(self.advance().unwrap_err())),
            Some(Ok(b)) => Some(Ok(*b)),
            None => None,
        }
    }

    fn advance(&mut self) -> Result<u8> {
        let b = self.bytes.next().ok_or(ErrorType::UnexpectedEOF)??;
        self.col += 1;
        if b == b'\n' {
            self.line += 1;
            self.col = 1;
        }
        Ok(b)
    }

    fn eat(&mut self, want: u8, typ: &str) -> Result<u8> {
        if self.advance()? == want {
            Ok(want)
        } else {
            Err(ErrorType::InvalidToken(typ.to_owned()))
        }
    }

    fn advance_while(&mut self, f: impl Fn(u8) -> bool) -> Result<String> {
        let mut buf = Vec::new();
        while f(self.peek().ok_or(ErrorType::UnexpectedEOF)??) {
            buf.push(self.advance().unwrap());
        }
        Ok(String::from_utf8(buf)?)
    }

    fn advance_emit(&mut self, size: usize, typ: TokenCargo) -> Result<Token> {
        let (line, col) = (self.line, self.col);
        for _ in 0..size {
            self.advance()?;
        }
        Ok(Token { typ, line, col })
    }

    fn str(&mut self) -> Result<Token> {
        let (line, col) = (self.line, self.col);
        self.eat(b'"', "str")?;
        let text = self.advance_while(|b| b != b'"')?;
        self.eat(b'"', "str")?;
        Ok(Token { typ: TokenCargo::Str(text), line, col })
    }

    fn int(&mut self) -> Result<Token> {
        let (line, col) = (self.line, self.col);
        let text = self.advance_while(|b| !is_delim(b))?;
        let int = text.parse::<i64>()?;
        Ok(Token { typ: TokenCargo::Int(int), line, col })
    }

    fn keyword_or_ident(&mut self) -> Result<Token> {
        let (line, col) = (self.line, self.col);
        let text = self.advance_while(|b| !is_delim(b))?;
        Ok(Token { typ: ident_type(&text), line, col })
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek(), Some(Ok(b' ' | b'\n'))) {
            self.advance().unwrap();
        }
    }
}

#[derive(Debug, Error)]
#[error("{line}:{col}: {err:?}")]
pub struct ScannerError {
    line: usize,
    col: usize,
    err: ErrorType,
}

impl<R: io::BufRead> iter::Iterator for Scanner<R> {
    type Item = result::Result<Token, ScannerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let (line, col) = (self.line, self.col);
        let result = self
            .peek()?
            .and_then(|b| match b {
                b'(' => self.advance_emit(1, TokenCargo::Lparen),
                b')' => self.advance_emit(1, TokenCargo::Rparen),
                b'{' => self.advance_emit(1, TokenCargo::Lbrace),
                b'}' => self.advance_emit(1, TokenCargo::Rbrace),
                b',' => self.advance_emit(1, TokenCargo::Comma),
                b';' => self.advance_emit(1, TokenCargo::Semi),
                b':' => self.advance_emit(1, TokenCargo::Colon),
                b'"' => self.str(),
                b if b.is_ascii_digit() => self.int(),
                b if b.is_ascii_alphabetic() => self.keyword_or_ident(),
                b => Err(ErrorType::UnknownToken(b)),
            })
            .map_err(|err| ScannerError { line, col, err });
        Some(result)
    }
}

pub fn scan<R: io::BufRead>(r: R) -> Scanner<R> {
    Scanner { bytes: r.bytes().peekable(), line: 1, col: 1 }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scan_all(b: &[u8]) -> result::Result<Vec<Token>, ScannerError> {
        scan(b).collect()
    }

    fn tok(typ: TokenCargo, line: usize, col: usize) -> Token {
        Token { typ, line, col }
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
        use TokenCargo::*;
        let input = b"
            fn foo(bar: int, baz: str) {
                println(\"hello, world\", 27);
            }
        ";
        let toks = scan_all(input).unwrap();
        let expected = vec![
            tok(Fn, 2, 13),
            tok(Ident("foo".to_owned()), 2, 16),
            tok(Lparen, 2, 19),
            tok(Ident("bar".to_owned()), 2, 20),
            tok(Colon, 2, 23),
            tok(Ident("int".to_owned()), 2, 25),
            tok(Comma, 2, 28),
            tok(Ident("baz".to_owned()), 2, 30),
            tok(Colon, 2, 33),
            tok(Ident("str".to_owned()), 2, 35),
            tok(Rparen, 2, 38),
            tok(Lbrace, 2, 40),
            tok(Ident("println".to_owned()), 3, 17),
            tok(Lparen, 3, 24),
            tok(Str("hello, world".to_owned()), 3, 25),
            tok(Comma, 3, 39),
            tok(Int(27), 3, 41),
            tok(Rparen, 3, 43),
            tok(Semi, 3, 44),
            tok(Rbrace, 4, 13),
        ];
        assert_eq!(expected, toks);
    }
}
