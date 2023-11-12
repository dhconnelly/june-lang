use crate::scanner;
use std::io;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {}

#[derive(Debug, Error)]
#[error("{line}:{col}: {err:?}")]
pub struct ParserError {
    line: usize,
    col: usize,
    err: Error,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program;

pub fn parse<R: io::BufRead>(
    _scan: scanner::Scanner<R>,
) -> result::Result<Program, ParserError> {
    Ok(Program)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_empty() {
        let input: &[u8] = b"
        ";
        let toks = scanner::scan(input);
        let ast = parse(toks).unwrap();
        assert_eq!(Program, ast);
    }
}
