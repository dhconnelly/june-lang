// TODO: write failure test cases
use crate::ast::{Expr::*, *};
use crate::types;
use crate::types::*;
use std::collections::HashMap;
use std::result;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum Error {
    #[error("undefined: {0}")]
    Undefined(String),
    #[error("wrong number of arguments: want {want}, got {got}")]
    Arity { want: usize, got: usize },
    #[error("{0}")]
    TypeMismatch(#[from] types::Error),
    #[error("not callable: {0:?}")]
    InvalidCallable(Type),
    #[error("unknown type: {0}")]
    UnknownType(String),
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
struct SymbolInfo {
    idx: usize,
    typ: Type,
}

#[derive(Debug)]
struct SymbolTable {
    // TODO: supporting forward references will require supporting empty values
    // in the globals table
    globals: HashMap<String, SymbolInfo>,
    frames: Vec<HashMap<String, SymbolInfo>>,
}

impl SymbolTable {
    fn new() -> Self {
        Self { globals: HashMap::new(), frames: Vec::new() }
    }

    fn def_global<S: Into<String>>(&mut self, name: S, typ: Type) {
        let idx = self.globals.len();
        let name = name.into();
        self.globals.insert(name, SymbolInfo { idx, typ });
    }

    fn push_frame(&mut self) {
        self.frames.push(HashMap::new());
    }

    fn pop_frame(&mut self) {
        self.frames.pop().unwrap();
    }

    fn get_global(&self, name: &str) -> Option<Resolution> {
        self.globals.get(name).map(|SymbolInfo { idx, typ }| Resolution {
            reference: Reference::Global { idx: *idx },
            typ: typ.clone(),
        })
    }

    fn get_frame(&self, name: &str, depth: usize) -> Option<Resolution> {
        let i = self.frames.len() - depth - 1;
        self.frames[i].get(name).map(|SymbolInfo { idx, typ }| Resolution {
            reference: Reference::Stack { frame_depth: depth, frame_idx: *idx },
            typ: typ.clone(),
        })
    }

    fn get(&self, name: &str) -> Option<Resolution> {
        (0..self.frames.len())
            .find_map(|depth| self.get_frame(name, depth))
            .or_else(|| self.get_global(name))
    }

    fn insert<S: Into<String>>(&mut self, name: S, typ: Type) {
        let frame = self.frames.last_mut().unwrap();
        let idx = frame.len();
        frame.insert(name.into(), SymbolInfo { idx, typ });
    }
}

fn analyze_all(
    exprs: Vec<Expr>,
    ctx: &mut SymbolTable,
) -> Result<Vec<TypedExpr>> {
    let mut texprs = Vec::new();
    for expr in exprs {
        texprs.push(analyze_expr(expr, ctx)?);
    }
    Ok(texprs)
}

fn check_all(want: &[Type], got: &[TypedExpr]) -> Result<()> {
    if want.len() != got.len() {
        Err(Error::Arity { want: want.len(), got: got.len() })
    } else {
        let mut pairs = want.iter().zip(got.iter());
        Ok(pairs.try_for_each(|(want, got)| want.check(&got.typ()))?)
    }
}

fn analyze_func(f: Func, ctx: &mut SymbolTable) -> Result<TypedFunc> {
    // TODO: look for return statements when we handle return types
    ctx.push_frame();
    let mut params = Vec::new();
    for param in f.params {
        let typ = resolve_type(&param.typ, ctx)?;
        params.push(TypedParam {
            name: param.name.clone(),
            typ: param.typ,
            cargo: typ.clone(),
        });
        ctx.insert(param.name, typ);
    }
    let body = analyze_block(f.body, ctx)?;
    let cargo = FnType {
        params: params.iter().map(|param| param.cargo.clone()).collect(),
        ret: Box::new(resolve_type(&f.ret, ctx)?),
    };
    let func = TypedFunc { name: f.name, params, body, ret: f.ret, cargo };
    ctx.pop_frame();
    Ok(func)
}

fn analyze_expr(expr: Expr, ctx: &mut SymbolTable) -> Result<TypedExpr> {
    match expr {
        IntExpr(prim) => Ok(IntExpr(prim)),
        StrExpr(prim) => Ok(StrExpr(prim)),
        IdentExpr(prim) => {
            let name = prim.name;
            let cargo = ctx.get(&name).ok_or(Error::Undefined(name.clone()))?;
            Ok(IdentExpr(TypedIdent { name, cargo }))
        }
        CallExpr(call) => {
            let target = analyze_expr(*call.target, ctx)?;
            if let Type::Fn(f) = target.typ() {
                let target = Box::new(target);
                let args = analyze_all(call.args, ctx)?;
                check_all(&f.params, &args)?;
                Ok(CallExpr(TypedCall { target, args, cargo: *f.ret }))
            } else {
                Err(Error::InvalidCallable(target.typ()))
            }
        }
        FuncExpr(_) => todo!(),
    }
}

fn resolve_type(typ: &TypeSpec, ctx: &SymbolTable) -> Result<Type> {
    // TODO: handle more complex types
    match typ {
        TypeSpec::Void => Ok(Type::Void),
        TypeSpec::Simple(typ) if "int" == typ => Ok(Type::Int),
        TypeSpec::Simple(typ) if "str" == typ => Ok(Type::Str),
        TypeSpec::Simple(typ) => Err(Error::UnknownType(typ.into())),
    }
}

fn analyze_let(stmt: Binding, ctx: &mut SymbolTable) -> Result<TypedStmt> {
    let name = stmt.name;
    let typ = resolve_type(&stmt.typ, ctx)?;
    let expr = analyze_expr(stmt.expr, ctx)?;
    typ.check(&expr.typ())?;
    ctx.insert(name.clone(), typ.clone());
    Ok(TypedStmt::LetStmt(TypedBinding {
        name,
        typ: stmt.typ,
        expr,
        cargo: typ,
    }))
}

fn analyze_stmt(stmt: Stmt, ctx: &mut SymbolTable) -> Result<TypedStmt> {
    match stmt {
        Stmt::ExprStmt(expr) => Ok(Stmt::ExprStmt(analyze_expr(expr, ctx)?)),
        Stmt::LetStmt(stmt) => analyze_let(stmt, ctx),
        Stmt::BlockStmt(block) => {
            Ok(Stmt::BlockStmt(analyze_block(block, ctx)?))
        }
    }
}

fn analyze_block(block: Block, ctx: &mut SymbolTable) -> Result<TypedBlock> {
    ctx.push_frame();
    let mut stmts = Vec::new();
    for stmt in block.0 {
        stmts.push(analyze_stmt(stmt, ctx)?);
    }
    ctx.pop_frame();
    Ok(Block::<TypedAST>(stmts))
}

fn analyze_def(def: Def, ctx: &mut SymbolTable) -> Result<TypedDef> {
    todo!()
}

pub fn analyze_program(prog: Program) -> Result<TypedProgram> {
    let mut ctx = SymbolTable::new();
    let mut defs = Vec::new();
    for def in prog.defs {
        defs.push(analyze_def(def, &mut ctx)?);
    }
    Ok(TypedProgram { defs })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser;
    use crate::scanner;

    fn parse(input: &[u8]) -> parser::Parser<&[u8]> {
        let s = scanner::scan(input);
        parser::parse(s)
    }

    fn analyze_all<
        T,
        U: Typed,
        F: Fn(&mut parser::Parser<&[u8]>) -> parser::Result<T>,
        G: FnMut(T) -> Result<U>,
    >(
        inputs: &[&[u8]],
        parse: F,
        mut analyze: G,
    ) -> Vec<Result<Type>> {
        let mut v = Vec::new();
        for input in inputs {
            let s = scanner::scan(*input);
            let mut p = parser::parse(s);
            let node = parse(&mut p).unwrap();
            v.push(analyze(node).map(|nd| nd.typ().clone()))
        }
        v
    }

    #[test]
    fn test_func() {
        let mut ctx = SymbolTable::new();
        ctx.def_global(
            "itoa",
            Type::Fn(FnType {
                params: vec![Type::Int],
                ret: Box::new(Type::Str),
            }),
        );
        ctx.def_global(
            "join",
            Type::Fn(FnType {
                params: vec![Type::Str, Type::Str],
                ret: Box::new(Type::Str),
            }),
        );
        ctx.push_frame();
        ctx.insert(
            "println",
            Type::Fn(FnType {
                params: vec![Type::Str],
                ret: Box::new(Type::Void),
            }),
        );
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
                TypedParam {
                    name: String::from("name"),
                    typ: TypeSpec::simple("str"),
                    cargo: Type::Str,
                },
                TypedParam {
                    name: String::from("age"),
                    typ: TypeSpec::simple("int"),
                    cargo: Type::Int,
                },
            ],
            ret: TypeSpec::Void,
            body: Block(vec![
                Stmt::LetStmt(TypedBinding {
                    name: String::from("age_str"),
                    typ: TypeSpec::simple("str"),
                    expr: CallExpr(Call {
                        target: Box::new(IdentExpr(Ident {
                            name: String::from("itoa"),
                            cargo: Resolution {
                                reference: Reference::Global { idx: 0 },
                                typ: Type::Fn(FnType {
                                    params: vec![Type::Int],
                                    ret: Box::new(Type::Str),
                                }),
                            },
                        })),
                        args: vec![IdentExpr(Ident {
                            name: String::from("age"),
                            cargo: Resolution {
                                reference: Reference::Stack {
                                    frame_depth: 1,
                                    frame_idx: 1,
                                },
                                typ: Type::Int,
                            },
                        })],
                        cargo: Type::Str,
                    }),
                    cargo: Type::Str,
                }),
                Stmt::LetStmt(TypedBinding {
                    name: String::from("greeting"),
                    typ: TypeSpec::simple("str"),
                    expr: CallExpr(Call {
                        target: Box::new(IdentExpr(Ident {
                            name: String::from("join"),
                            cargo: Resolution {
                                reference: Reference::Global { idx: 1 },
                                typ: Type::Fn(FnType {
                                    params: vec![Type::Str, Type::Str],
                                    ret: Box::new(Type::Str),
                                }),
                            },
                        })),
                        args: vec![
                            IdentExpr(Ident {
                                name: String::from("name"),
                                cargo: Resolution {
                                    reference: Reference::Stack {
                                        frame_depth: 1,
                                        frame_idx: 0,
                                    },
                                    typ: Type::Str,
                                },
                            }),
                            IdentExpr(Ident {
                                name: String::from("age_str"),
                                cargo: Resolution {
                                    reference: Reference::Stack {
                                        frame_depth: 0,
                                        frame_idx: 0,
                                    },
                                    typ: Type::Str,
                                },
                            }),
                        ],
                        cargo: Type::Str,
                    }),
                    cargo: Type::Str,
                }),
                Stmt::ExprStmt(CallExpr(Call {
                    target: Box::new(IdentExpr(Ident {
                        name: String::from("println"),
                        cargo: Resolution {
                            reference: Reference::Stack {
                                frame_depth: 2,
                                frame_idx: 0,
                            },
                            typ: Type::Fn(FnType {
                                params: vec![Type::Str],
                                ret: Box::new(Type::Void),
                            }),
                        },
                    })),
                    args: vec![IdentExpr(Ident {
                        name: String::from("greeting"),
                        cargo: Resolution {
                            reference: Reference::Stack {
                                frame_depth: 0,
                                frame_idx: 1,
                            },
                            typ: Type::Str,
                        },
                    })],
                    cargo: Type::Void,
                })),
            ]),
            cargo: FnType {
                params: vec![Type::Str, Type::Int],
                ret: Box::new(Type::Void),
            },
        };
        let func = parse(input).fn_expr().unwrap();
        let actual = analyze_func(func, &mut ctx).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_block() {
        let mut ctx = SymbolTable::new();
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
            Stmt::LetStmt(Binding {
                name: String::from("x"),
                typ: TypeSpec::Simple(String::from("int")),
                expr: IntExpr(Literal { value: 7 }),
                cargo: Type::Int,
            }),
            Stmt::LetStmt(Binding {
                name: String::from("y"),
                typ: TypeSpec::Simple(String::from("int")),
                expr: IdentExpr(Ident {
                    name: String::from("x"),
                    cargo: Resolution {
                        typ: Type::Int,
                        reference: Reference::Stack {
                            frame_depth: 0,
                            frame_idx: 0,
                        },
                    },
                }),
                cargo: Type::Int,
            }),
            Stmt::BlockStmt(Block(vec![
                Stmt::LetStmt(Binding {
                    name: String::from("z"),
                    typ: TypeSpec::Simple(String::from("int")),
                    expr: IdentExpr(Ident {
                        name: String::from("y"),
                        cargo: Resolution {
                            typ: Type::Int,
                            reference: Reference::Stack {
                                frame_depth: 1,
                                frame_idx: 1,
                            },
                        },
                    }),
                    cargo: Type::Int,
                }),
                Stmt::LetStmt(Binding {
                    name: String::from("y"),
                    typ: TypeSpec::Simple(String::from("int")),
                    expr: IdentExpr(Ident {
                        name: String::from("x"),
                        cargo: Resolution {
                            typ: Type::Int,
                            reference: Reference::Stack {
                                frame_depth: 1,
                                frame_idx: 0,
                            },
                        },
                    }),
                    cargo: Type::Int,
                }),
                Stmt::LetStmt(Binding {
                    name: String::from("w"),
                    typ: TypeSpec::Simple(String::from("int")),
                    expr: IdentExpr(Ident {
                        name: String::from("y"),
                        cargo: Resolution {
                            typ: Type::Int,
                            reference: Reference::Stack {
                                frame_depth: 0,
                                frame_idx: 1,
                            },
                        },
                    }),
                    cargo: Type::Int,
                }),
                Stmt::BlockStmt(Block(vec![Stmt::LetStmt(Binding {
                    name: String::from("x"),
                    typ: TypeSpec::Simple(String::from("int")),
                    expr: IntExpr(Literal { value: 7 }),
                    cargo: Type::Int,
                })])),
                Stmt::ExprStmt(IdentExpr(Ident {
                    name: String::from("x"),
                    cargo: Resolution {
                        typ: Type::Int,
                        reference: Reference::Stack {
                            frame_depth: 1,
                            frame_idx: 0,
                        },
                    },
                })),
            ])),
            Stmt::ExprStmt(IdentExpr(Ident {
                name: String::from("y"),
                cargo: Resolution {
                    typ: Type::Int,
                    reference: Reference::Stack {
                        frame_depth: 0,
                        frame_idx: 1,
                    },
                },
            })),
        ]);
        let block = parse(input).block().unwrap();
        let actual = analyze_block(block, &mut ctx).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_call() {
        let mut ctx = SymbolTable::new();
        ctx.push_frame();
        ctx.insert(
            String::from("println"),
            Type::Fn(FnType {
                params: vec![Type::Int, Type::Str],
                ret: Box::new(Type::Void),
            }),
        );
        let inputs: &[&[u8]] = &[
            b"println(\"foo\")",
            b"println(27, 34)",
            b"println(27, \"foo\")",
        ];
        let expected = vec![
            Err(Error::Arity { want: 2, got: 1 }),
            Err(Error::TypeMismatch(types::Error {
                want: String::from("Str"),
                got: Type::Int,
            })),
            Ok(Type::Void),
        ];
        let actual =
            analyze_all(inputs, |p| p.expr(), |e| analyze_expr(e, &mut ctx));
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_ident() {
        let mut ctx = SymbolTable::new();
        ctx.push_frame();
        ctx.insert(String::from("foo"), Type::Int);
        ctx.insert(String::from("bar"), Type::Str);
        let inputs: &[&[u8]] = &[b"foo", b"bar", b"baz"];
        let expected = vec![
            Ok(Type::Int),
            Ok(Type::Str),
            Err(Error::Undefined(String::from("baz"))),
        ];
        let actual =
            analyze_all(inputs, |p| p.expr(), |e| analyze_expr(e, &mut ctx));
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_literal() {
        let inputs: &[&[u8]] = &[b"27", b"\"hello, world\""];
        let expected = vec![
            TypedExpr::IntExpr(Literal::new(27)),
            TypedExpr::StrExpr(Literal::new("hello, world")),
        ];
        let actual: Vec<TypedExpr> = inputs
            .iter()
            .map(|input| parse(*input).expr().unwrap())
            .map(|expr| analyze_expr(expr, &mut SymbolTable::new()).unwrap())
            .collect();
        assert_eq!(expected, actual);
    }
}
