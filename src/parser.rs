use crate::ast::{Def::*, Expr::*, Stmt::*, *};
use crate::scanner;
use crate::token::{Token, Token::*};
use std::io;
use std::iter;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("scanner: {0}")]
    ScannerError(#[from] scanner::Error),
    #[error("unexpected eof")]
    UnexpectedEOF,
    #[error("invalid: want {want}, got {got:?}")]
    Invalid { want: String, got: Token },
}

impl Error {
    fn invalid(want: &str, got: Token) -> Error {
        Error::Invalid { want: want.to_owned(), got }
    }
}

type Result<T> = result::Result<T, Error>;

pub struct Parser<R: io::BufRead> {
    scanner: iter::Peekable<scanner::Scanner<R>>,
}

impl<R: io::BufRead> Parser<R> {
    fn eof(&mut self) -> bool {
        self.scanner.peek().is_none()
    }

    fn peek_is(&mut self, want: Token) -> bool {
        matches!(self.scanner.peek(), Some(Ok(got)) if got == &want)
    }

    fn advance(&mut self) -> Result<Token> {
        Ok(self.scanner.next().ok_or(Error::UnexpectedEOF)??)
    }

    fn eat<T>(
        &mut self,
        mut f: impl FnMut(&Token) -> Option<T>,
        want: &str,
    ) -> Result<T> {
        let got = self.advance()?;
        match f(&got) {
            Some(t) => Ok(t),
            None => Err(Error::invalid(want, got)),
        }
    }

    fn eat_tok(&mut self, want: Token) -> Result<()> {
        self.eat(
            |tok| if tok == &want { Some(()) } else { None },
            &format!("{:?}", want),
        )
    }

    fn eat_ident(&mut self) -> Result<String> {
        self.eat(
            |tok| if let Ident(s) = tok { Some(s.clone()) } else { None },
            "ident",
        )
    }

    fn list<T>(
        &mut self,
        mut f: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut list = Vec::new();
        if !self.peek_is(Rparen) {
            list.push(f(self)?);
        }
        while !self.peek_is(Rparen) {
            self.eat_tok(Comma)?;
            list.push(f(self)?);
        }
        Ok(list)
    }

    pub fn primary(&mut self) -> Result<Expr> {
        let prim = match self.scanner.next().ok_or(Error::UnexpectedEOF)?? {
            Str(cargo) => Ok(StrExpr(Primary { cargo })),
            Ident(cargo) => Ok(IdentExpr(Primary { cargo })),
            Int(cargo) => Ok(IntExpr(Primary { cargo })),
            Lparen => {
                let expr = self.expr()?;
                self.eat_tok(Rparen)?;
                Ok(expr)
            }
            tok => Err(Error::invalid("prim", tok)),
        }?;
        if let Some(Ok(Lparen)) = self.scanner.peek() {
            self.eat_tok(Lparen)?;
            let args = self.list(|p| p.expr())?;
            self.eat_tok(Rparen)?;
            Ok(CallExpr(Call { target: Box::new(prim), args }))
        } else {
            Ok(prim)
        }
    }

    pub fn stmt(&mut self) -> Result<Stmt> {
        let expr = self.expr()?;
        self.eat_tok(Semi)?;
        Ok(ExprStmt(expr))
    }

    pub fn block(&mut self) -> Result<Block> {
        self.eat_tok(Lbrace)?;
        let mut stmts = Vec::new();
        while !self.peek_is(Rbrace) {
            stmts.push(self.stmt()?);
        }
        self.eat_tok(Rbrace)?;
        Ok(Block(stmts))
    }

    pub fn param(&mut self) -> Result<Param> {
        let name = self.eat_ident()?;
        self.eat_tok(Colon)?;
        let typ = self.eat_ident()?;
        Ok(Param { name, typ })
    }

    pub fn expr(&mut self) -> Result<Expr> {
        self.primary()
    }

    pub fn fn_expr(&mut self) -> Result<FnExpr> {
        self.eat_tok(Fn)?;
        let name = self.eat_ident()?;
        self.eat_tok(Lparen)?;
        let params = self.list(|p| p.param())?;
        self.eat_tok(Rparen)?;
        let body = self.block()?;
        Ok(FnExpr { name, params, body })
    }

    pub fn def(&mut self) -> Result<Def> {
        Ok(FnDef(self.fn_expr()?))
    }

    pub fn program(&mut self) -> Result<Program> {
        let mut defs = Vec::new();
        while !self.eof() {
            defs.push(self.def()?);
        }
        Ok(Program { defs })
    }
}

pub fn parse<R: io::BufRead>(scanner: scanner::Scanner<R>) -> Parser<R> {
    Parser { scanner: scanner.peekable() }
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
        let input = b"foo bar 27 \"hello, world\"";
        let expected = vec![
            Expr::IdentExpr(Primary { cargo: String::from("foo") }),
            Expr::IdentExpr(Primary { cargo: String::from("bar") }),
            Expr::IntExpr(Primary { cargo: 27 }),
            Expr::StrExpr(Primary { cargo: String::from("hello, world") }),
        ];
        let actual = parse_all(input, |p| p.primary());
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_call() {
        let inputs = vec![
            b" foo ( ) ".as_slice(),
            b" bar ( arg ) ".as_slice(),
            b" println ( \"foo\" , 27 ) ".as_slice(),
        ];
        let expected = vec![
            CallExpr(Call {
                target: Box::new(IdentExpr(Primary {
                    cargo: "foo".to_owned(),
                })),
                args: Vec::new(),
            }),
            CallExpr(Call {
                target: Box::new(IdentExpr(Primary {
                    cargo: "bar".to_owned(),
                })),
                args: vec![IdentExpr(Primary { cargo: "arg".to_owned() })],
            }),
            CallExpr(Call {
                target: Box::new(IdentExpr(Primary {
                    cargo: "println".to_owned(),
                })),
                args: vec![
                    StrExpr(Primary { cargo: "foo".to_owned() }),
                    IntExpr(Primary { cargo: 27 }),
                ],
            }),
        ];
        let actual: Vec<Expr> = inputs
            .iter()
            .map(|input| parse(input).expr().unwrap())
            .collect();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_block() {
        let input = b" {
            27;
            foo(bar);
        }";
        let expected = Block(vec![
            ExprStmt(IntExpr(Primary { cargo: 27 })),
            ExprStmt(CallExpr(Call {
                target: Box::new(IdentExpr(Primary {
                    cargo: "foo".to_owned(),
                })),
                args: vec![IdentExpr(Primary { cargo: "bar".to_owned() })],
            })),
        ]);
        let actual = parse(input).block().unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_param() {
        let input = b" foo : bar ";
        let expected = Param { name: "foo".to_owned(), typ: "bar".to_owned() };
        let actual = parse(input).param().unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_fn() {
        let input = b" fn hello ( world: int, all: str ) { foo(27); } ";
        let expected = FnExpr {
            name: "hello".to_string(),
            params: vec![
                Param { name: "world".to_owned(), typ: "int".to_owned() },
                Param { name: "all".to_owned(), typ: "str".to_owned() },
            ],
            body: Block(vec![ExprStmt(CallExpr(Call {
                target: Box::new(IdentExpr(Primary {
                    cargo: "foo".to_owned(),
                })),
                args: vec![IntExpr(Primary { cargo: 27 })],
            }))]),
        };
        let actual = parse(input).fn_expr().unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_hello_world() {
        let input = b"
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
                    name: String::from("foo"),
                    params: vec![
                        Param {
                            name: String::from("s"),
                            typ: String::from("str"),
                        },
                        Param {
                            name: String::from("t"),
                            typ: String::from("int"),
                        },
                    ],
                    body: Block(vec![Stmt::ExprStmt(Expr::CallExpr(Call {
                        target: Box::new(Expr::IdentExpr(Primary {
                            cargo: String::from("println"),
                        })),
                        args: vec![
                            Expr::IdentExpr(Primary {
                                cargo: String::from("s"),
                            }),
                            Expr::IdentExpr(Primary {
                                cargo: String::from("t"),
                            }),
                        ],
                    }))]),
                }),
                Def::FnDef(FnExpr {
                    name: String::from("main"),
                    params: vec![],
                    body: Block(vec![Stmt::ExprStmt(Expr::CallExpr(Call {
                        target: Box::new(Expr::IdentExpr(Primary {
                            cargo: String::from("foo"),
                        })),
                        args: vec![Expr::StrExpr(Primary {
                            cargo: String::from("hello, world"),
                        })],
                    }))]),
                }),
            ],
        };
        assert_eq!(expected, ast);
    }
}
