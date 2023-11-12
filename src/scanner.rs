use std::io::BufRead;
use std::iter::Iterator;
use std::result;
use thiserror::Error;

#[derive(Debug, PartialEq, Eq)]
pub struct Token;

#[derive(Error, Debug)]
pub enum Error {}

pub type Result<T> = result::Result<T, Error>;

pub struct Scanner<R: BufRead> {
    _r: R,
}

impl<R: BufRead> Iterator for Scanner<R> {
    type Item = Result<Token>;

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

    fn scan_all(b: &[u8]) -> Result<Vec<Token>> {
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
