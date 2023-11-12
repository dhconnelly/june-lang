use crate::ast::*;
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

pub fn parse<R: io::BufRead>(
    _scan: scanner::Scanner<R>,
) -> result::Result<Program, ParserError> {
    Ok(Program { defs: Vec::new() })
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
        assert_eq!(Program { defs: Vec::new() }, ast);
    }

    #[test]
    fn test_hello_world() {
        let input: &[u8] = b"
            fn foo(s: str) {
                println(s);
            }

            fn main() {
                foo(\"hello, world\");
            }
        ";

        let toks = scanner::scan(input);
        let ast = parse(toks).unwrap();
        let expected = Program {
            defs: vec![
                Def::Fn(Fn {
                    line: 2,
                    col: 13,
                    name: String::from("foo"),
                    params: vec![Param {
                        line: 2,
                        col: 20,
                        name: String::from("s"),
                        typ: String::from("str"),
                    }],
                    body: Block(vec![Stmt::ExprStmt(Expr::CallExpr(Call {
                        line: 3,
                        col: 17,
                        target: Box::new(Expr::IdentExpr(Primary {
                            line: 3,
                            col: 17,
                            cargo: String::from("println"),
                        })),
                        args: vec![Expr::IdentExpr(Primary {
                            line: 3,
                            col: 25,
                            cargo: String::from("s"),
                        })],
                    }))]),
                }),
                Def::Fn(Fn {
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
