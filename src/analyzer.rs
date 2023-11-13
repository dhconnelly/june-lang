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
    Type(#[from] types::Error),
    #[error("not callable: {0:?}")]
    InvalidCallable(Type),
}

type Result<T> = result::Result<T, Error>;

// TODO: avoid copying the expression
type SymbolTable = HashMap<String, TypedExpr>;

fn analyze_all(exprs: Vec<Expr>, ctx: &SymbolTable) -> Result<Vec<TypedExpr>> {
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
        Ok(pairs.try_for_each(|(want, got)| want.check(got.typ()))?)
    }
}

pub fn analyze_expr(expr: Expr, ctx: &SymbolTable) -> Result<TypedExpr> {
    match expr {
        IntExpr(prim) => Ok(IntExpr(TypedPrimary {
            cargo: prim.cargo,
            assoc: Type::Int,
        })),
        StrExpr(prim) => Ok(StrExpr(TypedPrimary {
            cargo: prim.cargo,
            assoc: Type::Str,
        })),
        IdentExpr(prim) => {
            let name = prim.cargo;
            let expr = ctx.get(&name).ok_or(Error::Undefined(name.clone()))?;
            Ok(expr.clone())
        }
        CallExpr(call) => match analyze_expr(*call.target, ctx)? {
            FuncExpr(f) => {
                let fn_typ = f.typ().as_fn().unwrap();
                let args = analyze_all(call.args, ctx)?;
                check_all(&fn_typ.params, &args)?;
                Ok(CallExpr(TypedCall {
                    target: Box::new(FuncExpr(f)),
                    args,
                    assoc: *fn_typ.ret,
                }))
            }
            typ => Err(Error::InvalidCallable(typ.typ().clone())),
        },
        _ => todo!(),
    }
}

pub fn analyze_program(_prog: Program) -> Result<TypedProgram> {
    todo!()
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
        F: Fn(T) -> Result<U>,
        G: Fn(&mut parser::Parser<&[u8]>) -> parser::Result<T>,
    >(
        inputs: &[&[u8]],
        parse: G,
        analyze: F,
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
    fn test_calls() {
        let mut ctx = HashMap::new();
        ctx.insert(
            String::from("println"),
            TypedExpr::FuncExpr(TypedFunc {
                name: String::from("println"),
                params: vec![
                    Param {
                        name: String::from("left"),
                        typ: TypeSpec::Simple(String::from("int")),
                    },
                    Param {
                        name: String::from("right"),
                        typ: TypeSpec::Simple(String::from("str")),
                    },
                ],
                body: Block(vec![]),
                ret: TypeSpec::Void,
                assoc: Type::Fn(FnType {
                    params: vec![Type::Int, Type::Str],
                    ret: Box::new(Type::Void),
                }),
            }),
        );
        let inputs: &[&[u8]] = &[
            b"println(\"foo\")",
            b"println(27, 34)",
            b"println(27, \"foo\")",
        ];
        let expected = vec![
            Err(Error::Arity { want: 2, got: 1 }),
            Err(Error::Type(types::Error {
                want: String::from("Str"),
                got: Type::Int,
            })),
            Ok(Type::Void),
        ];
        let actual =
            analyze_all(inputs, |p| p.expr(), |e| analyze_expr(e, &ctx));
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_idents() {
        let mut ctx = HashMap::new();
        ctx.insert(
            String::from("foo"),
            TypedExpr::IntExpr(TypedPrimary { cargo: 27, assoc: Type::Int }),
        );
        ctx.insert(
            String::from("bar"),
            TypedExpr::CallExpr(TypedCall {
                target: Box::new(TypedExpr::FuncExpr(TypedFunc {
                    name: String::from("test"),
                    params: vec![],
                    body: Block(vec![]),
                    ret: TypeSpec::Simple(String::from("str")),
                    assoc: Type::Fn(FnType {
                        params: vec![],
                        ret: Box::new(Type::Void),
                    }),
                })),
                args: vec![],
                assoc: Type::Str,
            }),
        );
        let inputs: &[&[u8]] = &[b"foo", b"bar", b"baz"];
        let expected = vec![
            Ok(Type::Int),
            Ok(Type::Str),
            Err(Error::Undefined(String::from("baz"))),
        ];
        let actual =
            analyze_all(inputs, |p| p.expr(), |e| analyze_expr(e, &ctx));
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_literals() {
        let inputs: &[&[u8]] = &[b"27", b"\"hello, world\""];
        let expected = vec![
            TypedExpr::IntExpr(Primary { cargo: 27, assoc: Type::Int }),
            TypedExpr::StrExpr(Primary {
                cargo: String::from("hello, world"),
                assoc: Type::Str,
            }),
        ];
        let actual: Vec<TypedExpr> = inputs
            .iter()
            .map(|input| parse(*input).expr().unwrap())
            .map(|expr| analyze_expr(expr, &HashMap::new()).unwrap())
            .collect();
        assert_eq!(expected, actual);
    }
}
