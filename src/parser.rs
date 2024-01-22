// TODO: write BNF grammar
use crate::ast::*;
use crate::scanner;
use crate::token::*;
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

pub struct Parser<R: io::Read> {
    scanner: iter::Peekable<scanner::Scanner<R>>,
}

impl<R: io::Read> Parser<R> {
    pub fn new(scanner: scanner::Scanner<R>) -> Parser<R> {
        Parser { scanner: scanner.peekable() }
    }

    pub fn eof(&mut self) -> bool {
        self.scanner.peek().is_none()
    }

    fn peek_is(&mut self, f: impl Fn(&Token) -> bool) -> bool {
        matches!(self.scanner.peek(), Some(Ok(tok)) if f(tok))
    }

    fn eat<T, Eat>(&mut self, f: Eat, want: impl ToString) -> Result<T>
    where
        Eat: Fn(&Token) -> Option<T>,
    {
        let got = self.scanner.next().ok_or(Error::UnexpectedEOF)??;
        match f(&got) {
            Some(t) => Ok(t),
            None => Err(Error::Invalid { want: want.to_string(), got }),
        }
    }

    fn eat_tok(&mut self, want: Token) -> Result<()> {
        self.eat(|tok| if tok == &want { Some(()) } else { None }, &want)
    }

    fn eat_ident(&mut self) -> Result<String> {
        self.eat(|tok| tok.as_ident(), "ident")
    }

    fn primary(&mut self) -> Result<Expr> {
        match self.scanner.next().ok_or(Error::UnexpectedEOF)?? {
            Token::Str(value) => Ok(Expr::Str(Literal::new(value))),
            Token::Int(value) => Ok(Expr::Int(Literal::new(value))),
            Token::Ident(name) => Ok(Expr::Ident(Ident::untyped(name))),
            Token::Lparen => {
                let expr = self.expr()?;
                self.eat_tok(Token::Rparen)?;
                Ok(expr)
            }
            got => Err(Error::Invalid { want: String::from("prim"), got }),
        }
    }

    fn call(&mut self) -> Result<Expr> {
        let target = self.primary()?;
        if self.peek_is(|tok| tok == &Token::Lparen) {
            self.eat_tok(Token::Lparen)?;
            let args = self.list(|p| p.expr())?;
            self.eat_tok(Token::Rparen)?;
            Ok(Expr::Call(Call::untyped(target, args)))
        } else {
            Ok(target)
        }
    }

    fn list<T, Eat>(&mut self, eat: Eat) -> Result<Vec<T>>
    where
        Eat: Fn(&mut Self) -> Result<T>,
    {
        let mut list = Vec::new();
        while !self.peek_is(|tok| tok == &Token::Rparen) {
            if !list.is_empty() {
                self.eat_tok(Token::Comma)?;
            }
            list.push(eat(self)?);
        }
        Ok(list)
    }

    fn binary<Pred, Eat>(&mut self, pred: Pred, eat: Eat) -> Result<Expr>
    where
        Pred: Fn(&Op) -> bool,
        Eat: Fn(&mut Self) -> Result<Expr>,
    {
        let mut expr = self.call()?;
        while self.peek_is(|tok| matches!(tok, Token::Op(op) if pred(op))) {
            let op = self.eat(|tok| tok.as_op(), "op")?;
            expr = Expr::Binary(Binary::untyped(op.into(), expr, eat(self)?));
        }
        Ok(expr)
    }

    fn mul_div(&mut self) -> Result<Expr> {
        self.binary(|op| matches!(op, Op::Star | Op::Slash), |p| p.call())
    }

    fn add_sub(&mut self) -> Result<Expr> {
        self.binary(|op| matches!(op, Op::Plus | Op::Minus), |p| p.mul_div())
    }

    pub fn expr(&mut self) -> Result<Expr> {
        self.add_sub()
    }

    fn type_spec(&mut self) -> Result<TypeSpec> {
        // TODO: parse more complicated types
        Ok(TypeSpec::Simple(self.eat_ident()?))
    }

    fn expr_stmt(&mut self) -> Result<Stmt> {
        let expr = self.expr()?;
        self.eat_tok(Token::Semi)?;
        Ok(Stmt::Expr(expr))
    }

    fn param(&mut self) -> Result<Param> {
        let name = self.eat_ident()?;
        self.eat_tok(Token::Colon)?;
        let typ = self.type_spec()?;
        Ok(Param::untyped(name, typ))
    }

    fn let_stmt(&mut self) -> Result<Stmt> {
        self.eat_tok(Token::Let)?;
        let param = self.param()?;
        self.eat_tok(Token::Eq)?;
        let expr = self.expr()?;
        self.eat_tok(Token::Semi)?;
        Ok(Stmt::Let(Binding::new(param.name, param.typ, expr, ())))
    }

    pub fn stmt(&mut self) -> Result<Stmt> {
        match self.scanner.peek() {
            Some(Ok(Token::Let)) => self.let_stmt(),
            Some(Ok(Token::Lbrace)) => Ok(Stmt::Block(self.block()?)),
            _ => self.expr_stmt(),
        }
    }

    pub fn block(&mut self) -> Result<Block> {
        self.eat_tok(Token::Lbrace)?;
        let mut stmts = Vec::new();
        while !self.peek_is(|tok| tok == &Token::Rbrace) {
            stmts.push(self.stmt()?);
        }
        self.eat_tok(Token::Rbrace)?;
        Ok(Block(stmts))
    }

    pub fn fn_expr(&mut self) -> Result<Func> {
        self.eat_tok(Token::Fn)?;
        let name = self.eat_ident()?;
        self.eat_tok(Token::Lparen)?;
        let params = self.list(|p| p.param())?;
        self.eat_tok(Token::Rparen)?;
        // TODO: parse return type
        let body = self.block()?;
        Ok(Func::untyped(name, params, body, None))
    }

    pub fn def(&mut self) -> Result<Def> {
        Ok(Def::FnDef(self.fn_expr()?))
    }

    pub fn program(&mut self) -> Result<Program> {
        let mut defs = Vec::new();
        while !self.eof() {
            defs.push(self.def()?);
        }
        Ok(Program { main_def: (), defs })
    }
}

pub fn parse<R: io::Read>(scanner: scanner::Scanner<R>) -> Result<Program> {
    Parser::new(scanner).program()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::BinaryOp::*;

    fn parse(input: &[u8]) -> Parser<&[u8]> {
        Parser { scanner: scanner::scan(input).peekable() }
    }

    #[test]
    fn test_empty() {
        let input: &[u8] = b"
        ";
        let mut p = parse(input);
        let ast = p.program().unwrap();
        assert_eq!(Program { main_def: (), defs: Vec::new() }, ast);
    }

    #[test]
    fn test_primary() {
        let input = b"foo bar 27 \"hello, world\"";
        let expected = vec![
            Expr::Ident(Ident::untyped("foo")),
            Expr::Ident(Ident::untyped("bar")),
            Expr::Int(Literal::new(27)),
            Expr::Str(Literal::new("hello, world")),
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
            Expr::Call(Call::untyped(
                Expr::Ident(Ident::untyped("foo")),
                Vec::new(),
            )),
            Expr::Call(Call::untyped(
                Expr::Ident(Ident::untyped("bar")),
                vec![Expr::Ident(Ident::untyped("arg"))],
            )),
            Expr::Call(Call::untyped(
                Expr::Ident(Ident::untyped("println")),
                vec![Expr::Str(Literal::new("foo")), Expr::Int(Literal::new(27))],
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
            Stmt::Expr(Expr::Int(Literal::new(27))),
            Stmt::Expr(Expr::Call(Call::untyped(
                Expr::Ident(Ident::untyped("foo")),
                vec![Expr::Ident(Ident::untyped("bar"))],
            ))),
        ]);
        let actual = parse(input).block().unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_param() {
        let input = b" foo : bar ";
        let expected = Param::untyped("foo", TypeSpec::Simple(String::from("bar")));
        let actual = parse(input).param().unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_let() {
        let input = b"let foo: int = 7;";
        let expected = Stmt::Let(Binding {
            name: String::from("foo"),
            typ: TypeSpec::Simple(String::from("int")),
            expr: Expr::Int(Literal::new(7)),
            resolved_type: (),
        });
        let actual = parse(input).let_stmt().unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_binary() {
        let input = b"x + y + (5 + 4) + foo(7, 10) + z + ((a + b) + c)";
        let expected = Expr::Binary(Binary::untyped(
            BinaryOp::Add,
            Expr::Binary(Binary::untyped(
                Add,
                Expr::Binary(Binary::untyped(
                    Add,
                    Expr::Binary(Binary::untyped(
                        Add,
                        Expr::Binary(Binary::untyped(
                            Add,
                            Expr::Ident(Ident::untyped("x")),
                            Expr::Ident(Ident::untyped("y")),
                        )),
                        Expr::Binary(Binary::untyped(
                            Add,
                            Expr::Int(Literal::new(5)),
                            Expr::Int(Literal::new(4)),
                        )),
                    )),
                    Expr::Call(Call::untyped(
                        Expr::Ident(Ident::untyped("foo")),
                        vec![
                            Expr::Int(Literal::new(7)),
                            Expr::Int(Literal::new(10)),
                        ],
                    )),
                )),
                Expr::Ident(Ident::untyped("z")),
            )),
            Expr::Binary(Binary::untyped(
                Add,
                Expr::Binary(Binary::untyped(
                    Add,
                    Expr::Ident(Ident::untyped("a")),
                    Expr::Ident(Ident::untyped("b")),
                )),
                Expr::Ident(Ident::untyped("c")),
            )),
        ));
        let actual = parse(input).expr().unwrap();
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
            Block(vec![Stmt::Expr(Expr::Call(Call::untyped(
                Expr::Ident(Ident::untyped("foo")),
                vec![Expr::Int(Literal::new(27))],
            )))]),
            None,
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
            main_def: (),
            defs: vec![
                Def::FnDef(Func::untyped(
                    "foo",
                    vec![
                        Param::untyped("s", TypeSpec::Simple(String::from("str"))),
                        Param::untyped("t", TypeSpec::Simple(String::from("int"))),
                    ],
                    Block(vec![Stmt::Expr(Expr::Call(Call::untyped(
                        Expr::Ident(Ident::untyped("println")),
                        vec![
                            Expr::Ident(Ident::untyped("s")),
                            Expr::Ident(Ident::untyped("t")),
                        ],
                    )))]),
                    None,
                )),
                Def::FnDef(Func::untyped(
                    "main",
                    vec![],
                    Block(vec![Stmt::Expr(Expr::Call(Call::untyped(
                        Expr::Ident(Ident::untyped("foo")),
                        vec![Expr::Str(Literal::new("hello, world"))],
                    )))]),
                    None,
                )),
            ],
        };
        assert_eq!(expected, ast);
    }
}
