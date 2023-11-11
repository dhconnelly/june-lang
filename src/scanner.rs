use std::error;
use std::fmt;
use std::io::BufRead;
use std::iter::Iterator;

#[derive(Debug, PartialEq, Eq)]
pub struct Token;

#[derive(Debug)]
pub struct Error;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "scanner error")
    }
}

impl error::Error for Error {}

pub struct Scanner<R: BufRead> {
    _r: R,
}

impl<R: BufRead> Iterator for Scanner<R> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

pub fn scan<R: BufRead>(r: R) -> Scanner<R> {
    Scanner { _r: r }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scan_all(b: &[u8]) -> Result<Vec<Token>, Error> {
        let scanner = scan(b);
        scanner.collect()
    }

    #[test]
    fn foo() {
        let input = b"
        ";
        let toks = scan_all(input).unwrap();
        let empty = Vec::<Token>::new();
        assert_eq!(empty, toks);
    }
}
