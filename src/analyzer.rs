// TODO: write failure test cases
use crate::ast::*;
use crate::builtins;
use crate::symbol_table::*;
use crate::types::*;
use std::result;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum Error {
    #[error("undefined: {0}")]
    Undefined(String),
    #[error("wrong number of arguments: want {want}, got {got}")]
    Arity { want: usize, got: usize },
    #[error("type mismatch: want {want:?}, got {got:?}")]
    TypeMismatch { want: Type, got: Type },
    #[error("not callable: {0:?}")]
    InvalidCallable(Type),
    #[error("unknown type: {0}")]
    UnknownType(String),
    #[error("invalid types for operator {op:?}: {lhs:?}, {rhs:?}")]
    InvalidOpTypes { op: BinaryOp, lhs: Type, rhs: Type },
    #[error("entrypoint `main` undefined")]
    NoMain,
}

type Result<T> = result::Result<T, Error>;

fn check_op(op: BinaryOp, lhs: Type, rhs: Type) -> Result<Type> {
    use BinaryOp::*;
    use Type::*;
    match (op, &lhs, &rhs) {
        (Add, Int, Int) => Ok(Int),
        (Add, Str, Str) => Ok(Str),
        _ => Err(Error::InvalidOpTypes { op, lhs, rhs }),
    }
}

fn check((want, got): (&Type, &TypedExpr)) -> Result<()> {
    let got = got.typ();
    if want != &got {
        Err(Error::TypeMismatch { want: want.clone(), got })
    } else {
        Ok(())
    }
}

fn check_all(want: &[Type], got: &[TypedExpr]) -> Result<()> {
    if want.len() != got.len() {
        Err(Error::Arity { want: want.len(), got: got.len() })
    } else {
        want.iter().zip(got).try_for_each(check)
    }
}

pub struct Analyzer {
    ctx: SymbolTable,
}

impl Default for Analyzer {
    fn default() -> Analyzer {
        let mut ctx = SymbolTable::default();
        builtins::install_symbols(&mut ctx);
        Analyzer::with_context(ctx)
    }
}

impl Analyzer {
    fn with_context(ctx: SymbolTable) -> Analyzer {
        Analyzer { ctx }
    }

    fn try_map<T, U, F>(&mut self, ts: Vec<T>, f: F) -> Result<Vec<U>>
    where
        F: Fn(&mut Self, T) -> Result<U>,
    {
        ts.into_iter().map(|t| f(self, t)).collect::<Result<_>>()
    }

    fn param(&mut self, param: Param) -> Result<TypedParam> {
        let (name, typ) = (param.name, param.typ);
        let resolved_type = self.resolve_type(&typ)?;
        Ok(Param { name, typ, resolved_type })
    }

    fn func(&mut self, f: Func) -> Result<TypedFunc> {
        self.ctx.enter_function();
        let params = self.try_map(f.params, |a, param| a.param(param))?;
        self.ctx.push_frame();
        for param in &params {
            self.ctx.def_local(&param.name, param.resolved_type.clone());
        }
        let body = self.block(f.body)?;
        let ret = f.ret.clone().map(|typ| self.resolve_type(&typ)).transpose()?;
        // TODO: look for return statements when we handle return types
        let resolved_type = FnDef {
            typ: self.ctx.def_fn(params.iter().map(|p| p.typ()).collect(), ret),
            locals: self.ctx.num_locals().unwrap() - params.len(),
        };
        let func = Func { name: f.name, params, body, ret: f.ret, resolved_type };
        self.ctx.pop_frame();
        self.ctx.exit_function();
        Ok(func)
    }

    fn call(&mut self, call: Call) -> Result<TypedCall> {
        let target = Box::new(self.expr(*call.target)?);
        if let Type::Fn(f) = target.typ() {
            let args = self.try_map(call.args, |a, arg| a.expr(arg))?;
            check_all(&f.params, &args)?;
            Ok(Call { target, args, resolved_type: f.ret.into() })
        } else {
            Err(Error::InvalidCallable(target.typ()))
        }
    }

    fn ident(&mut self, ident: Ident) -> Result<TypedExpr> {
        let name = ident.name;
        let resolution =
            self.ctx.get(&name).ok_or(Error::Undefined(name.clone()))?;
        Ok(Expr::Ident(Ident { name, resolution }))
    }

    fn binary(&mut self, expr: Binary) -> Result<TypedExpr> {
        let lhs = Box::new(self.expr(*expr.lhs)?);
        let rhs = Box::new(self.expr(*expr.rhs)?);
        let op = expr.op;
        let cargo = check_op(op, lhs.typ(), rhs.typ())?;
        Ok(Expr::Binary(Binary { op, lhs, rhs, cargo }))
    }

    pub fn expr(&mut self, expr: Expr) -> Result<TypedExpr> {
        match expr {
            Expr::Call(call) => Ok(Expr::Call(self.call(call)?)),
            Expr::Int(prim) => Ok(Expr::Int(prim)),
            Expr::Str(prim) => Ok(Expr::Str(prim)),
            Expr::Ident(prim) => self.ident(prim),
            Expr::Binary(bin) => self.binary(bin),
        }
    }

    fn resolve_type(&self, typ: &TypeSpec) -> Result<Type> {
        // TODO: handle more complex types
        match typ {
            TypeSpec::Void => Ok(Type::Void),
            TypeSpec::Simple(typ) if "int" == typ => Ok(Type::Int),
            TypeSpec::Simple(typ) if "str" == typ => Ok(Type::Str),
            TypeSpec::Simple(typ) => Err(Error::UnknownType(typ.into())),
        }
    }

    fn let_stmt(&mut self, stmt: Binding) -> Result<TypedStmt> {
        let typ = self.resolve_type(&stmt.typ)?;
        let expr = self.expr(stmt.expr)?;
        check((&typ, &expr))?;
        let idx = self.ctx.def_local(&stmt.name, typ.clone());
        Ok(Stmt::Let(Binding::new(
            stmt.name,
            stmt.typ,
            expr,
            LocalBinding { typ, idx },
        )))
    }

    fn stmt(&mut self, stmt: Stmt) -> Result<TypedStmt> {
        match stmt {
            Stmt::Expr(expr) => Ok(Stmt::Expr(self.expr(expr)?)),
            Stmt::Let(stmt) => self.let_stmt(stmt),
            Stmt::Block(block) => Ok(Stmt::Block(self.block(block)?)),
        }
    }

    fn block(&mut self, Block(stmts): Block) -> Result<TypedBlock> {
        self.ctx.push_frame();
        let stmts = self.try_map(stmts, |a, stmt| a.stmt(stmt))?;
        self.ctx.pop_frame();
        Ok(Block(stmts))
    }

    fn def(&mut self, def: Def) -> Result<TypedDef> {
        match def {
            Def::FnDef(f) => {
                let func = self.func(f)?;
                let typ = func.resolved_type.clone();
                self.ctx.def_global(&func.name, Type::Fn(typ.typ));
                Ok(Def::FnDef(func))
            }
        }
    }

    fn program(&mut self, Program { defs }: Program) -> Result<TypedProgram> {
        let defs = self.try_map(defs, |a, def| a.def(def))?;
        if defs.iter().any(|d| matches!(d, Def::FnDef(f) if &f.name == "main")) {
            Ok(Program { defs })
        } else {
            Err(Error::NoMain)
        }
    }
}

pub fn analyze(prog: Program) -> Result<TypedProgram> {
    Analyzer::default().program(prog)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser;
    use crate::scanner;
    use pretty_assertions::assert_eq;

    fn parse(input: &[u8]) -> parser::Parser<&[u8]> {
        parser::Parser::new(scanner::scan(input))
    }

    fn with_locals<S: ToString, T: IntoIterator<Item = (S, Type)>>(
        locals: T,
    ) -> Analyzer {
        let mut ctx = SymbolTable::default();
        ctx.enter_function();
        ctx.push_frame();
        locals.into_iter().for_each(|(name, typ)| {
            ctx.def_local(name.to_string(), typ);
        });
        Analyzer::with_context(ctx)
    }

    #[test]
    fn test_hello() {
        let input = b"
            fn greet(name: str) {
                println(name);
            }

            fn main() {
                greet(\"the pope\");
            }
        ";
        let program = parse(input).program().unwrap();
        let expected = Program {
            defs: vec![
                Def::FnDef(Func {
                    name: String::from("greet"),
                    params: vec![Param {
                        name: String::from("name"),
                        typ: TypeSpec::simple("str"),
                        resolved_type: Type::Str,
                    }],
                    ret: None,
                    body: Block(vec![Stmt::Expr(Expr::Call(Call {
                        target: Box::new(Expr::Ident(Ident {
                            name: String::from("println"),
                            resolution: Resolution {
                                reference: Reference::Global { idx: 0 },
                                typ: Type::Fn(FnType {
                                    index: 0,
                                    params: vec![Type::Str],
                                    ret: None,
                                }),
                            },
                        })),
                        args: vec![Expr::Ident(Ident {
                            name: String::from("name"),
                            resolution: Resolution {
                                reference: Reference::Stack { local_idx: 0 },
                                typ: Type::Str,
                            },
                        })],
                        resolved_type: Type::Void,
                    }))]),
                    resolved_type: FnDef {
                        typ: FnType { index: 1, params: vec![Type::Str], ret: None },
                        locals: 0,
                    },
                }),
                Def::FnDef(Func {
                    name: String::from("main"),
                    params: vec![],
                    ret: None,
                    body: Block(vec![Stmt::Expr(Expr::Call(Call {
                        target: Box::new(Expr::Ident(Ident {
                            name: String::from("greet"),
                            resolution: Resolution {
                                reference: Reference::Global { idx: 1 },
                                typ: Type::Fn(FnType {
                                    index: 1,
                                    params: vec![Type::Str],
                                    ret: None,
                                }),
                            },
                        })),
                        args: vec![Expr::Str(Literal::new("the pope"))],
                        resolved_type: Type::Void,
                    }))]),
                    resolved_type: FnDef {
                        typ: FnType { index: 2, params: vec![], ret: None },
                        locals: 0,
                    },
                }),
            ],
        };
        let mut ctx = SymbolTable::default();
        let println = ctx.def_fn(vec![Type::Str], None);
        ctx.def_global("println", Type::Fn(println));
        let actual = Analyzer::with_context(ctx).program(program).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_func() {
        let mut ctx = SymbolTable::default();
        let itoa = ctx.def_fn(vec![Type::Int], Some(Type::Str));
        ctx.def_global("itoa", Type::Fn(itoa));
        let join = ctx.def_fn(vec![Type::Str, Type::Str], Some(Type::Str));
        ctx.def_global("join", Type::Fn(join));
        let println = ctx.def_fn(vec![Type::Str], None);
        ctx.def_global("println", Type::Fn(println));
        let input = b"
            fn greet(name: str, age: int) {
                let age_str: str = itoa(age);
                let greeting: str = join(name, age_str);
                println(greeting);
            }
        ";
        let expected = Func {
            name: String::from("greet"),
            params: vec![
                Param {
                    name: String::from("name"),
                    typ: TypeSpec::simple("str"),
                    resolved_type: Type::Str,
                },
                Param {
                    name: String::from("age"),
                    typ: TypeSpec::simple("int"),
                    resolved_type: Type::Int,
                },
            ],
            ret: None,
            body: Block(vec![
                Stmt::Let(Binding {
                    name: String::from("age_str"),
                    typ: TypeSpec::simple("str"),
                    expr: Expr::Call(Call {
                        target: Box::new(Expr::Ident(Ident {
                            name: String::from("itoa"),
                            resolution: Resolution {
                                reference: Reference::Global { idx: 0 },
                                typ: Type::Fn(FnType {
                                    index: 0,
                                    params: vec![Type::Int],
                                    ret: Some(Box::new(Type::Str)),
                                }),
                            },
                        })),
                        args: vec![Expr::Ident(Ident {
                            name: String::from("age"),
                            resolution: Resolution {
                                reference: Reference::Stack { local_idx: 1 },
                                typ: Type::Int,
                            },
                        })],
                        resolved_type: Type::Str,
                    }),
                    resolved_type: LocalBinding { typ: Type::Str, idx: 2 },
                }),
                Stmt::Let(Binding {
                    name: String::from("greeting"),
                    typ: TypeSpec::simple("str"),
                    expr: Expr::Call(Call {
                        target: Box::new(Expr::Ident(Ident {
                            name: String::from("join"),
                            resolution: Resolution {
                                reference: Reference::Global { idx: 1 },
                                typ: Type::Fn(FnType {
                                    index: 1,
                                    params: vec![Type::Str, Type::Str],
                                    ret: Some(Box::new(Type::Str)),
                                }),
                            },
                        })),
                        args: vec![
                            Expr::Ident(Ident {
                                name: String::from("name"),
                                resolution: Resolution {
                                    reference: Reference::Stack { local_idx: 0 },
                                    typ: Type::Str,
                                },
                            }),
                            Expr::Ident(Ident {
                                name: String::from("age_str"),
                                resolution: Resolution {
                                    reference: Reference::Stack { local_idx: 2 },
                                    typ: Type::Str,
                                },
                            }),
                        ],
                        resolved_type: Type::Str,
                    }),
                    resolved_type: LocalBinding { typ: Type::Str, idx: 3 },
                }),
                Stmt::Expr(Expr::Call(Call {
                    target: Box::new(Expr::Ident(Ident {
                        name: String::from("println"),
                        resolution: Resolution {
                            reference: Reference::Global { idx: 2 },
                            typ: Type::Fn(FnType {
                                index: 2,
                                params: vec![Type::Str],
                                ret: None,
                            }),
                        },
                    })),
                    args: vec![Expr::Ident(Ident {
                        name: String::from("greeting"),
                        resolution: Resolution {
                            reference: Reference::Stack { local_idx: 3 },
                            typ: Type::Str,
                        },
                    })],
                    resolved_type: Type::Void,
                })),
            ]),
            resolved_type: FnDef {
                typ: FnType {
                    index: 3,
                    params: vec![Type::Str, Type::Int],
                    ret: None,
                },
                locals: 2,
            },
        };
        let func = parse(input).fn_expr().unwrap();
        let actual = Analyzer::with_context(ctx).func(func).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_binary() {
        let input: Vec<(Analyzer, &[u8])> = vec![
            (Analyzer::default(), b"14 + 7"),
            (Analyzer::default(), b"\"a\" + \"b\""),
            (with_locals(vec![("x", Type::Int)]), b"x + 7"),
            (with_locals(vec![("x", Type::Str)]), b"x + \"s\""),
            (with_locals(vec![("x", Type::Str)]), b"x + 7"),
        ];
        let expected = vec![
            Ok(Type::Int),
            Ok(Type::Str),
            Ok(Type::Int),
            Ok(Type::Str),
            Err(Error::InvalidOpTypes {
                op: BinaryOp::Add,
                lhs: Type::Str,
                rhs: Type::Int,
            }),
        ];
        let actual: Vec<Result<Type>> = input
            .into_iter()
            .map(|(mut a, s)| a.expr(parse(s).expr().unwrap()))
            .map(|e| e.map(|te| te.typ()))
            .collect();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_block() {
        let input = b"{
            let x: int = 7;
            let y: int = x;
            {
                let z: int = y;
                let y: int = x;
                let w: int = y;
                {
                    let x: int = 7;
                }
                x;
            }
            y;
        }";
        let expected = Block(vec![
            Stmt::Let(Binding {
                name: String::from("x"),
                typ: TypeSpec::Simple(String::from("int")),
                expr: Expr::Int(Literal { value: 7 }),
                resolved_type: LocalBinding { typ: Type::Int, idx: 0 },
            }),
            Stmt::Let(Binding {
                name: String::from("y"),
                typ: TypeSpec::Simple(String::from("int")),
                expr: Expr::Ident(Ident {
                    name: String::from("x"),
                    resolution: Resolution {
                        typ: Type::Int,
                        reference: Reference::Stack { local_idx: 0 },
                    },
                }),
                resolved_type: LocalBinding { typ: Type::Int, idx: 1 },
            }),
            Stmt::Block(Block(vec![
                Stmt::Let(Binding {
                    name: String::from("z"),
                    typ: TypeSpec::Simple(String::from("int")),
                    expr: Expr::Ident(Ident {
                        name: String::from("y"),
                        resolution: Resolution {
                            typ: Type::Int,
                            reference: Reference::Stack { local_idx: 1 },
                        },
                    }),
                    resolved_type: LocalBinding { typ: Type::Int, idx: 2 },
                }),
                Stmt::Let(Binding {
                    name: String::from("y"),
                    typ: TypeSpec::Simple(String::from("int")),
                    expr: Expr::Ident(Ident {
                        name: String::from("x"),
                        resolution: Resolution {
                            typ: Type::Int,
                            reference: Reference::Stack { local_idx: 0 },
                        },
                    }),
                    resolved_type: LocalBinding { typ: Type::Int, idx: 3 },
                }),
                Stmt::Let(Binding {
                    name: String::from("w"),
                    typ: TypeSpec::Simple(String::from("int")),
                    expr: Expr::Ident(Ident {
                        name: String::from("y"),
                        resolution: Resolution {
                            typ: Type::Int,
                            reference: Reference::Stack { local_idx: 3 },
                        },
                    }),
                    resolved_type: LocalBinding { typ: Type::Int, idx: 4 },
                }),
                Stmt::Block(Block(vec![Stmt::Let(Binding {
                    name: String::from("x"),
                    typ: TypeSpec::Simple(String::from("int")),
                    expr: Expr::Int(Literal { value: 7 }),
                    resolved_type: LocalBinding { typ: Type::Int, idx: 5 },
                })])),
                Stmt::Expr(Expr::Ident(Ident {
                    name: String::from("x"),
                    resolution: Resolution {
                        typ: Type::Int,
                        reference: Reference::Stack { local_idx: 0 },
                    },
                })),
            ])),
            Stmt::Expr(Expr::Ident(Ident {
                name: String::from("y"),
                resolution: Resolution {
                    typ: Type::Int,
                    reference: Reference::Stack { local_idx: 1 },
                },
            })),
        ]);
        let block = parse(input).block().unwrap();
        let mut analyzer = Analyzer::default();
        analyzer.ctx.enter_function();
        let actual = analyzer.block(block).unwrap();
        assert_eq!(expected, actual);
    }

    fn analyze_exprs(inputs: &[&[u8]], ctx: SymbolTable) -> Vec<Result<Type>> {
        let mut analyzer = Analyzer::with_context(ctx);
        inputs
            .iter()
            .map(|b| parse(b).expr().unwrap())
            .map(|e| analyzer.expr(e).map(|e| e.typ()))
            .collect()
    }

    #[test]
    fn test_call() {
        let mut ctx = SymbolTable::default();
        ctx.enter_function();
        ctx.push_frame();
        ctx.def_local(
            String::from("println"),
            Type::Fn(FnType {
                index: 0,
                params: vec![Type::Int, Type::Str],
                ret: None,
            }),
        );
        let inputs: &[&[u8]] = &[
            b"println(\"foo\")",
            b"println(27, 34)",
            b"println(27, \"foo\")",
        ];
        let expected = vec![
            Err(Error::Arity { want: 2, got: 1 }),
            Err(Error::TypeMismatch { want: Type::Str, got: Type::Int }),
            Ok(Type::Void),
        ];
        let actual: Vec<Result<Type>> = analyze_exprs(inputs, ctx);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_ident() {
        let mut ctx = SymbolTable::default();
        ctx.enter_function();
        ctx.push_frame();
        ctx.def_local(String::from("foo"), Type::Int);
        ctx.def_local(String::from("bar"), Type::Str);
        let inputs: &[&[u8]] = &[b"foo", b"bar", b"baz"];
        let expected = vec![
            Ok(Type::Int),
            Ok(Type::Str),
            Err(Error::Undefined(String::from("baz"))),
        ];
        let actual: Vec<Result<Type>> = analyze_exprs(inputs, ctx);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_literal() {
        let inputs: &[&[u8]] = &[b"27", b"\"hello, world\""];
        let expected = vec![Ok(Type::Int), Ok(Type::Str)];
        let actual = analyze_exprs(inputs, SymbolTable::default());
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_main() {
        let input = b"fn foo() {}";
        let ast = parse(input).program().unwrap();
        let actual = analyze(ast);
        assert_eq!(Err(Error::NoMain), actual);
    }
}
