use crate::ast::{Expr::*, *};
use crate::scanner;
use crate::token::{Token, TokenType::*};
use std::io;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ErrorType {
    #[error("scanner: {0}")]
    ScannerError(#[from] scanner::ScannerError),
    #[error("unexpected eof")]
    UnexpectedEOF,
    #[error("invalid {0}")]
    Invalid(String),
}

type Result<T> = result::Result<T, ErrorType>;

pub struct Parser<R: io::BufRead> {
    scanner: scanner::Scanner<R>,
}

impl<R: io::BufRead> Parser<R> {
    fn eof(&mut self) -> bool {
        self.scanner.eof()
    }

    pub fn primary(&mut self) -> Result<Expr> {
        let tok = self.scanner.next().ok_or(ErrorType::UnexpectedEOF)??;
        let (line, col, cargo) = (tok.line, tok.col, tok.text);
        match tok.typ {
            Str => Ok(StrExpr(Primary { line, col, cargo })),
            Ident => Ok(IdentExpr(Primary { line, col, cargo })),
            Int => {
                // ints are checked in the scanner
                let cargo = cargo.parse::<i64>().unwrap();
                Ok(IntExpr(Primary { line, col, cargo }))
            }
            _ => return Err(ErrorType::Invalid(String::from("primary"))),
        }
    }

    pub fn program(&mut self) -> Result<Program> {
        Ok(Program { defs: Vec::new() })
    }
}

#[derive(Debug, Error)]
#[error("{line}:{col}: {err:?}")]
pub struct ParserError {
    line: usize,
    col: usize,
    err: ErrorType,
}

pub fn parse<R: io::BufRead>(scanner: scanner::Scanner<R>) -> Parser<R> {
    Parser { scanner }
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse(input: &[u8]) -> Parser<&[u8]> {
        let s = scanner::scan(input);
        super::parse(s)
    }

    fn parse_all<T>(
        input: &[u8],
        mut f: impl FnMut(&mut Parser<&[u8]>) -> Result<T>,
    ) -> Vec<T> {
        let mut p = parse(input);
        let mut v = Vec::new();
        while !p.eof() {
            v.push(f(&mut p).unwrap());
        }
        v
    }

    #[test]
    fn test_empty() {
        let input: &[u8] = b"
        ";
        let mut p = parse(input);
        let ast = p.program().unwrap();
        assert_eq!(Program { defs: Vec::new() }, ast);
    }

    #[test]
    fn test_primary() {
        let input: &[u8] = b"foo bar 27 \"hello, world\"";
        let expected = vec![
            Expr::IdentExpr(Primary {
                line: 1,
                col: 1,
                cargo: String::from("foo"),
            }),
            Expr::IdentExpr(Primary {
                line: 1,
                col: 5,
                cargo: String::from("bar"),
            }),
            Expr::IntExpr(Primary { line: 1, col: 9, cargo: 27 }),
            Expr::StrExpr(Primary {
                line: 1,
                col: 12,
                cargo: String::from("hello, world"),
            }),
        ];
        let actual = parse_all(input, |p| p.primary());
        assert_eq!(expected, actual);
    }

    #[ignore]
    #[test]
    fn test_hello_world() {
        let input: &[u8] = b"
            fn foo(s: str, t: int) {
                println(s, t);
            }

            fn main() {
                foo(\"hello, world\");
            }
        ";

        let ast = parse(input).program().unwrap();
        let expected = Program {
            defs: vec![
                Def::FnDef(FnExpr {
                    line: 2,
                    col: 13,
                    name: String::from("foo"),
                    params: vec![
                        Param {
                            line: 2,
                            col: 20,
                            name: String::from("s"),
                            typ: String::from("str"),
                        },
                        Param {
                            line: 2,
                            col: 28,
                            name: String::from("t"),
                            typ: String::from("int"),
                        },
                    ],
                    body: Block(vec![Stmt::ExprStmt(Expr::CallExpr(Call {
                        line: 3,
                        col: 17,
                        target: Box::new(Expr::IdentExpr(Primary {
                            line: 3,
                            col: 17,
                            cargo: String::from("println"),
                        })),
                        args: vec![
                            Expr::IdentExpr(Primary {
                                line: 3,
                                col: 25,
                                cargo: String::from("s"),
                            }),
                            Expr::IdentExpr(Primary {
                                line: 3,
                                col: 28,
                                cargo: String::from("t"),
                            }),
                        ],
                    }))]),
                }),
                Def::FnDef(FnExpr {
                    line: 6,
                    col: 13,
                    name: String::from("main"),
                    params: vec![],
                    body: Block(vec![Stmt::ExprStmt(Expr::CallExpr(Call {
                        line: 7,
                        col: 17,
                        target: Box::new(Expr::IdentExpr(Primary {
                            line: 7,
                            col: 17,
                            cargo: String::from("foo"),
                        })),
                        args: vec![Expr::StrExpr(Primary {
                            line: 7,
                            col: 21,
                            cargo: String::from("hello, world"),
                        })],
                    }))]),
                }),
            ],
        };
        assert_eq!(expected, ast);
    }
}
