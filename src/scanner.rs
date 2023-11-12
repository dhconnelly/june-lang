use std::io;
use std::iter;
use std::result;
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
    line: usize,
    col: usize,
}

#[derive(Error, Debug)]
pub enum Error {
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

type Result<T> = result::Result<T, Error>;

pub struct Scanner<R: io::BufRead> {
    bytes: iter::Peekable<io::Bytes<R>>,
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

impl<R: io::BufRead> Scanner<R> {
    fn peek(&mut self) -> Option<Result<u8>> {
        // if peek has an error, go ahead and get the owned result via advance
        match self.bytes.peek() {
            None => None,
            Some(Err(_)) => Some(self.bytes.next()?.map_err(|e| e.into())),
            Some(Ok(b)) => Some(Ok(*b)),
        }
    }

    fn advance(&mut self) -> Result<u8> {
        let b = self.bytes.next().ok_or(Error::UnexpectedEOF)??;
        if b == b'\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Ok(b)
    }

    fn eat(&mut self, want: u8, typ: TokenType) -> Result<u8> {
        match self.advance()? {
            got if got == want => Ok(got),
            _ => Err(Error::InvalidToken(typ)),
        }
    }

    fn eat_while(&mut self, f: impl Fn(u8) -> bool) -> Result<String> {
        let mut buf = Vec::new();
        while f(self.peek().ok_or(Error::UnexpectedEOF)??) {
            let b = self.advance().unwrap();
            buf.push(b);
        }
        Ok(String::from_utf8(buf)?)
    }

    fn symbol(&mut self, typ: TokenType) -> Result<Token> {
        let (line, col) = (self.line, self.col);
        let text = (self.advance()? as char).to_string();
        Ok(Token { typ, text, line, col })
    }

    fn str(&mut self) -> Result<Token> {
        let (line, col) = (self.line, self.col);
        self.eat(b'"', TokenType::Str)?;
        let text = self.eat_while(|b| b != b'"')?;
        self.eat(b'"', TokenType::Str)?;
        Ok(Token { typ: TokenType::Str, text, line, col })
    }

    fn keyword_or_ident(&mut self) -> Result<Token> {
        let (line, col) = (self.line, self.col);
        let text = self.eat_while(|b| !is_delim(b))?;
        Ok(Token { typ: ident_type(&text), text, line, col })
    }
}

#[derive(Debug, Error)]
#[error("{line}:{col}: {err:?}")]
pub struct ScannerError {
    line: usize,
    col: usize,
    err: Error,
}

impl<R: io::BufRead> iter::Iterator for Scanner<R> {
    type Item = result::Result<Token, ScannerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = loop {
            let (line, col) = (self.line, self.col);
            let b = match self.peek()? {
                Ok(b) => b,
                Err(err) => break Err(ScannerError { line, col, err }),
            };
            let result = match b {
                b if b.is_ascii_whitespace() => {
                    self.advance().unwrap();
                    continue;
                }
                b'(' => self.symbol(TokenType::Lparen),
                b')' => self.symbol(TokenType::Rparen),
                b'{' => self.symbol(TokenType::Lbrace),
                b'}' => self.symbol(TokenType::Rbrace),
                b',' => self.symbol(TokenType::Comma),
                b';' => self.symbol(TokenType::Semi),
                b'"' => self.str(),
                b if b.is_ascii_alphabetic() => self.keyword_or_ident(),
                b => Err(Error::UnknownToken(b)),
            };
            break match result {
                Err(err) => Err(ScannerError { line, col, err }),
                Ok(tok) => Ok(tok),
            };
        };
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
        let scanner = scan(b);
        scanner.collect()
    }

    fn tok(typ: TokenType, text: &str, line: usize, col: usize) -> Token {
        Token { typ, text: text.to_owned(), line, col }
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
            tok(Fn, "fn", 2, 13),
            tok(Ident, "foo", 2, 16),
            tok(Lparen, "(", 2, 19),
            tok(Ident, "bar", 2, 20),
            tok(Comma, ",", 2, 23),
            tok(Ident, "baz", 2, 25),
            tok(Rparen, ")", 2, 28),
            tok(Lbrace, "{", 2, 30),
            tok(Ident, "println", 3, 17),
            tok(Lparen, "(", 3, 24),
            tok(Str, "hello, world", 3, 25),
            tok(Rparen, ")", 3, 39),
            tok(Semi, ";", 3, 40),
            tok(Rbrace, "}", 4, 13),
        ];
        assert_eq!(expected, toks);
    }
}
