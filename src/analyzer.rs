// TODO: write failure test cases
use crate::ast::{Def::*, Expr::*, Stmt::*, *};
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
}

type Result<T> = result::Result<T, Error>;

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

fn analyze_func(f: Func, ctx: &mut SymbolTable) -> Result<TypedFunc> {
    // TODO: look for return statements when we handle return types
    ctx.push_frame();
    let (mut param_types, mut params) = (Vec::new(), Vec::new());
    for param in f.params {
        let typ = resolve_type(&param.typ, ctx)?;
        ctx.insert(param.name.clone(), typ.clone());
        param_types.push(typ.clone());
        params.push(Param { name: param.name, typ: param.typ, cargo: typ });
    }
    let body = analyze_block(f.body, ctx)?;
    let ret = Box::new(resolve_type(&f.ret, ctx)?);
    let cargo = FnType { params: param_types, ret };
    let func = TypedFunc { name: f.name, params, body, ret: f.ret, cargo };
    ctx.pop_frame();
    Ok(func)
}

fn analyze_call(call: Call, ctx: &mut SymbolTable) -> Result<TypedCall> {
    let target = Box::new(analyze_expr(*call.target, ctx)?);
    if let Type::Fn(f) = target.typ() {
        let args = call
            .args
            .into_iter()
            .map(|e| analyze_expr(e, ctx))
            .collect::<Result<Vec<TypedExpr>>>()?;
        check_all(&f.params, &args)?;
        Ok(Call { target, args, cargo: *f.ret })
    } else {
        Err(Error::InvalidCallable(target.typ()))
    }
}

fn analyze_expr(expr: Expr, ctx: &mut SymbolTable) -> Result<TypedExpr> {
    match expr {
        CallExpr(call) => Ok(CallExpr(analyze_call(call, ctx)?)),
        IntExpr(prim) => Ok(IntExpr(prim)),
        StrExpr(prim) => Ok(StrExpr(prim)),
        IdentExpr(prim) => {
            let name = prim.name;
            let cargo = ctx.get(&name).ok_or(Error::Undefined(name.clone()))?;
            Ok(IdentExpr(Ident { name, cargo }))
        }
    }
}

fn resolve_type(typ: &TypeSpec, _ctx: &SymbolTable) -> Result<Type> {
    // TODO: handle more complex types
    match typ {
        TypeSpec::Void => Ok(Type::Void),
        TypeSpec::Simple(typ) if "int" == typ => Ok(Type::Int),
        TypeSpec::Simple(typ) if "str" == typ => Ok(Type::Str),
        TypeSpec::Simple(typ) => Err(Error::UnknownType(typ.into())),
    }
}

fn analyze_let(stmt: Binding, ctx: &mut SymbolTable) -> Result<TypedStmt> {
    let typ = resolve_type(&stmt.typ, ctx)?;
    let expr = analyze_expr(stmt.expr, ctx)?;
    check((&typ, &expr))?;
    ctx.insert(stmt.name.clone(), typ.clone());
    Ok(LetStmt(Binding::new(stmt.name, stmt.typ, expr, typ)))
}

fn analyze_stmt(stmt: Stmt, ctx: &mut SymbolTable) -> Result<TypedStmt> {
    match stmt {
        ExprStmt(expr) => Ok(ExprStmt(analyze_expr(expr, ctx)?)),
        LetStmt(stmt) => analyze_let(stmt, ctx),
        BlockStmt(block) => Ok(BlockStmt(analyze_block(block, ctx)?)),
    }
}

fn analyze_block(
    Block(stmts): Block,
    ctx: &mut SymbolTable,
) -> Result<TypedBlock> {
    ctx.push_frame();
    let stmts = stmts
        .into_iter()
        .map(|stmt| analyze_stmt(stmt, ctx))
        .collect::<Result<_>>()?;
    ctx.pop_frame();
    Ok(Block(stmts))
}

fn analyze_def(def: Def, ctx: &mut SymbolTable) -> Result<TypedDef> {
    match def {
        FnDef(f) => {
            let func = analyze_func(f, ctx)?;
            ctx.def_global(func.name.clone(), Type::Fn(func.cargo.clone()));
            Ok(FnDef(func))
        }
    }
}

fn analyze_program(
    Program { defs }: Program,
    ctx: &mut SymbolTable,
) -> Result<TypedProgram> {
    let defs = defs
        .into_iter()
        .map(|def| analyze_def(def, ctx))
        .collect::<Result<_>>()?;
    Ok(Program { defs })
}

pub fn analyze(prog: Program) -> Result<TypedProgram> {
    let mut ctx = SymbolTable::new();
    builtins::install(&mut ctx);
    analyze_program(prog, &mut ctx)
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
                        cargo: Type::Str,
                    }],
                    ret: TypeSpec::Void,
                    body: Block(vec![ExprStmt(CallExpr(Call {
                        target: Box::new(IdentExpr(Ident {
                            name: String::from("println"),
                            cargo: Resolution {
                                reference: Reference::Global { idx: 0 },
                                typ: Type::Fn(FnType {
                                    params: vec![Type::Str],
                                    ret: Box::new(Type::Void),
                                }),
                            },
                        })),
                        args: vec![IdentExpr(Ident {
                            name: String::from("name"),
                            cargo: Resolution {
                                reference: Reference::Stack {
                                    frame_depth: 1,
                                    frame_idx: 0,
                                },
                                typ: Type::Str,
                            },
                        })],
                        cargo: Type::Void,
                    }))]),
                    cargo: FnType {
                        params: vec![Type::Str],
                        ret: Box::new(Type::Void),
                    },
                }),
                Def::FnDef(Func {
                    name: String::from("main"),
                    params: vec![],
                    ret: TypeSpec::Void,
                    body: Block(vec![ExprStmt(CallExpr(Call {
                        target: Box::new(IdentExpr(Ident {
                            name: String::from("greet"),
                            cargo: Resolution {
                                reference: Reference::Global { idx: 1 },
                                typ: Type::Fn(FnType {
                                    params: vec![Type::Str],
                                    ret: Box::new(Type::Void),
                                }),
                            },
                        })),
                        args: vec![StrExpr(Literal::new("the pope"))],
                        cargo: Type::Void,
                    }))]),
                    cargo: FnType { params: vec![], ret: Box::new(Type::Void) },
                }),
            ],
        };
        let mut ctx = SymbolTable::new();
        ctx.def_global(
            "println",
            Type::Fn(FnType {
                params: vec![Type::Str],
                ret: Box::new(Type::Void),
            }),
        );
        let actual = analyze_program(program, &mut ctx).unwrap();
        assert_eq!(expected, actual);
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
                Param {
                    name: String::from("name"),
                    typ: TypeSpec::simple("str"),
                    cargo: Type::Str,
                },
                Param {
                    name: String::from("age"),
                    typ: TypeSpec::simple("int"),
                    cargo: Type::Int,
                },
            ],
            ret: TypeSpec::Void,
            body: Block(vec![
                LetStmt(Binding {
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
                LetStmt(Binding {
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
                ExprStmt(CallExpr(Call {
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
            LetStmt(Binding {
                name: String::from("x"),
                typ: TypeSpec::Simple(String::from("int")),
                expr: IntExpr(Literal { value: 7 }),
                cargo: Type::Int,
            }),
            LetStmt(Binding {
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
            BlockStmt(Block(vec![
                LetStmt(Binding {
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
                LetStmt(Binding {
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
                LetStmt(Binding {
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
                BlockStmt(Block(vec![LetStmt(Binding {
                    name: String::from("x"),
                    typ: TypeSpec::Simple(String::from("int")),
                    expr: IntExpr(Literal { value: 7 }),
                    cargo: Type::Int,
                })])),
                ExprStmt(IdentExpr(Ident {
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
            ExprStmt(IdentExpr(Ident {
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

    fn analyze_exprs(
        inputs: &[&[u8]],
        mut ctx: SymbolTable,
    ) -> Vec<Result<Type>> {
        inputs
            .iter()
            .map(|b| parse(b).expr().unwrap())
            .map(|e| analyze_expr(e, &mut ctx).map(|e| e.typ()))
            .collect()
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
            Err(Error::TypeMismatch { want: Type::Str, got: Type::Int }),
            Ok(Type::Void),
        ];
        let actual: Vec<Result<Type>> = analyze_exprs(inputs, ctx.clone());
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
        let actual: Vec<Result<Type>> = analyze_exprs(inputs, ctx.clone());
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_literal() {
        let ctx = SymbolTable::new();
        let inputs: &[&[u8]] = &[b"27", b"\"hello, world\""];
        let expected = vec![Ok(Type::Int), Ok(Type::Str)];
        let actual = analyze_exprs(inputs, ctx.clone());
        assert_eq!(expected, actual);
    }
}
