// TODO: write BNF grammar
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

type Result<T> = result::Result<T, Error>;

pub struct Parser<R: io::BufRead> {
    scanner: iter::Peekable<scanner::Scanner<R>>,
}

impl<R: io::BufRead> Parser<R> {
    fn eof(&mut self) -> bool {
        self.scanner.peek().is_none()
    }

    fn eat<T, F: FnMut(&Token) -> Option<T>>(
        &mut self,
        mut f: F,
        want: String,
    ) -> Result<T> {
        let got = self.scanner.next().ok_or(Error::UnexpectedEOF)??;
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
        while !matches!(self.scanner.peek(), Some(Ok(Rparen))) {
            if list.len() > 0 {
                self.eat_tok(Comma)?;
            }
            list.push(f(self)?);
        }
        Ok(list)
    }

    fn primary(&mut self) -> Result<Expr> {
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

    fn type_spec(&mut self) -> Result<TypeSpec> {
        // TODO: parse more complicated types
        Ok(TypeSpec::Simple(self.eat_ident()?))
    }

    fn expr_stmt(&mut self) -> Result<Stmt> {
        let expr = self.expr()?;
        self.eat_tok(Semi)?;
        Ok(ExprStmt(expr))
    }

    fn let_stmt(&mut self) -> Result<Stmt> {
        self.eat_tok(Let)?;
        let name = self.eat_ident()?;
        self.eat_tok(Colon)?;
        let typ = self.type_spec()?;
        self.eat_tok(Eq)?;
        let expr = self.expr()?;
        self.eat_tok(Semi)?;
        Ok(LetStmt(Binding { name, typ, expr, cargo: () }))
    }

    pub fn stmt(&mut self) -> Result<Stmt> {
        match self.scanner.peek() {
            Some(Ok(Let)) => self.let_stmt(),
            Some(Ok(Lbrace)) => Ok(BlockStmt(self.block()?)),
            _ => self.expr_stmt(),
        }
    }

    pub fn block(&mut self) -> Result<Block> {
        self.eat_tok(Lbrace)?;
        let mut stmts = Vec::new();
        while !matches!(self.scanner.peek(), Some(Ok(Rbrace))) {
            stmts.push(self.stmt()?);
        }
        self.eat_tok(Rbrace)?;
        Ok(Block(stmts))
    }

    fn param(&mut self) -> Result<Param> {
        let name = self.eat_ident()?;
        self.eat_tok(Colon)?;
        let typ = self.type_spec()?;
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
        // TODO: parse return type
        let body = self.block()?;
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
            IdentExpr(Ident::untyped("foo")),
            IdentExpr(Ident::untyped("bar")),
            IntExpr(Literal::new(27)),
            StrExpr(Literal::new("hello, world")),
        ];
        let mut p = parse(input);
        let mut actual = Vec::new();
        while !p.eof() {
            actual.push(p.primary().unwrap());
        }
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
    fn test_let() {
        let input = b"let foo: int = 7;";
        let expected = LetStmt(Binding {
            name: String::from("foo"),
            typ: TypeSpec::Simple(String::from("int")),
            expr: IntExpr(Literal::new(7)),
            cargo: (),
        });
        let actual = parse(input).let_stmt().unwrap();
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
                    Block(vec![ExprStmt(Expr::CallExpr(Call::untyped(
                        IdentExpr(Ident::untyped("println")),
                        vec![
                            IdentExpr(Ident::untyped("s")),
                            IdentExpr(Ident::untyped("t")),
                        ],
                    )))]),
                    TypeSpec::Void,
                )),
                Def::FnDef(Func::untyped(
                    "main",
                    vec![],
                    Block(vec![ExprStmt(Expr::CallExpr(Call::untyped(
                        IdentExpr(Ident::untyped("foo")),
                        vec![StrExpr(Literal::new("hello, world"))],
                    )))]),
                    TypeSpec::Void,
                )),
            ],
        };
        assert_eq!(expected, ast);
    }
}
