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

pub type Result<T> = result::Result<T, Error>;

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

    fn eat<T, F: FnMut(&Token) -> Option<T>>(
        &mut self,
        mut f: F,
        want: String,
    ) -> Result<T> {
        let got = self.advance()?;
        match f(&got) {
            Some(t) => Ok(t),
            None => Err(Error::Invalid { want, got }),
        }
    }

    fn eat_tok(&mut self, want: Token) -> Result<()> {
        self.eat(
            |tok| if tok == &want { Some(()) } else { None },
            format!("{:?}", want),
        )
    }

    fn eat_ident(&mut self) -> Result<String> {
        self.eat(
            |tok| if let IdentTok(s) = tok { Some(s.clone()) } else { None },
            String::from("ident"),
        )
    }

    fn list<T, F: FnMut(&mut Self) -> Result<T>>(
        &mut self,
        mut f: F,
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
            Str(value) => Ok(StrExpr(Literal::new(value))),
            Int(value) => Ok(IntExpr(Literal::new(value))),
            IdentTok(name) => Ok(IdentExpr(Ident { name, cargo: () })),
            Lparen => {
                let expr = self.expr()?;
                self.eat_tok(Rparen)?;
                Ok(expr)
            }
            got => Err(Error::Invalid { want: String::from("prim"), got }),
        }?;
        if let Some(Ok(Lparen)) = self.scanner.peek() {
            self.eat_tok(Lparen)?;
            let args = self.list(|p| p.expr())?;
            self.eat_tok(Rparen)?;
            Ok(CallExpr(Call::untyped(prim, args)))
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
        let typ = TypeSpec::Simple(self.eat_ident()?);
        Ok(Param::untyped(name, typ))
    }

    pub fn expr(&mut self) -> Result<Expr> {
        self.primary()
    }

    pub fn fn_expr(&mut self) -> Result<Func> {
        self.eat_tok(FnTok)?;
        let name = self.eat_ident()?;
        self.eat_tok(Lparen)?;
        let params = self.list(|p| p.param())?;
        self.eat_tok(Rparen)?;
        let body = self.block()?;
        // TODO: parse type spec
        Ok(Func::untyped(name, params, body, TypeSpec::Void))
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

    fn parse_all<T, F: FnMut(&mut Parser<&[u8]>) -> Result<T>>(
        input: &[u8],
        mut f: F,
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
            Expr::IdentExpr(Ident::untyped("foo")),
            Expr::IdentExpr(Ident::untyped("bar")),
            Expr::IntExpr(Literal::new(27)),
            Expr::StrExpr(Literal::new("hello, world")),
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
            CallExpr(Call::untyped(
                IdentExpr(Ident::untyped("foo")),
                Vec::new(),
            )),
            CallExpr(Call::untyped(
                IdentExpr(Ident::untyped("bar")),
                vec![IdentExpr(Ident::untyped("arg"))],
            )),
            CallExpr(Call::untyped(
                IdentExpr(Ident::untyped("println")),
                vec![StrExpr(Literal::new("foo")), IntExpr(Literal::new(27))],
            )),
        ];
        let actual: Vec<Expr> =
            inputs.iter().map(|input| parse(input).expr().unwrap()).collect();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_block() {
        let input = b" {
            27;
            foo(bar);
        }";
        let expected = Block(vec![
            ExprStmt(IntExpr(Literal::new(27))),
            ExprStmt(CallExpr(Call::untyped(
                IdentExpr(Ident::untyped("foo")),
                vec![IdentExpr(Ident::untyped("bar"))],
            ))),
        ]);
        let actual = parse(input).block().unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_param() {
        let input = b" foo : bar ";
        let expected =
            Param::untyped("foo", TypeSpec::Simple(String::from("bar")));
        let actual = parse(input).param().unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_fn() {
        let input = b" fn hello ( world: int, all: str ) { foo(27); } ";
        let expected = Func::untyped(
            "hello",
            vec![
                Param::untyped("world", TypeSpec::Simple(String::from("int"))),
                Param::untyped("all", TypeSpec::Simple(String::from("str"))),
            ],
            Block(vec![ExprStmt(CallExpr(Call::untyped(
                IdentExpr(Ident::untyped("foo")),
                vec![IntExpr(Literal::new(27))],
            )))]),
            TypeSpec::Void,
        );
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
                Def::FnDef(Func::untyped(
                    "foo",
                    vec![
                        Param::untyped(
                            "s",
                            TypeSpec::Simple(String::from("str")),
                        ),
                        Param::untyped(
                            "t",
                            TypeSpec::Simple(String::from("int")),
                        ),
                    ],
                    Block(vec![Stmt::ExprStmt(Expr::CallExpr(Call::untyped(
                        Expr::IdentExpr(Ident::untyped("println")),
                        vec![
                            Expr::IdentExpr(Ident::untyped("s")),
                            Expr::IdentExpr(Ident::untyped("t")),
                        ],
                    )))]),
                    TypeSpec::Void,
                )),
                Def::FnDef(Func::untyped(
                    "main",
                    vec![],
                    Block(vec![Stmt::ExprStmt(Expr::CallExpr(Call::untyped(
                        Expr::IdentExpr(Ident::untyped("foo")),
                        vec![Expr::StrExpr(Literal::new("hello, world"))],
                    )))]),
                    TypeSpec::Void,
                )),
            ],
        };
        assert_eq!(expected, ast);
    }
}
