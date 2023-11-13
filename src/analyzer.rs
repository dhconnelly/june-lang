use crate::ast::{Expr::*, *};
use crate::types::*;
use std::collections::HashMap;
use std::result;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum Error {
    #[error("undefined: {0}")]
    Undefined(String),
}

type Result<T> = result::Result<T, Error>;

pub fn analyze_expr(
    expr: Expr,
    ctx: &HashMap<String, Type>,
) -> Result<TypedExpr> {
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
            let typ = ctx.get(&name).ok_or(Error::Undefined(name.clone()))?;
            Ok(IdentExpr(TypedPrimary { cargo: name, assoc: typ.clone() }))
        }
        _ => todo!(),
    }
}

pub fn analyze_program(prog: Program) -> Result<TypedProgram> {
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
            Type::Fn(FnType {
                params: vec![Type::Int, Type::Str],
                ret: Box::new(Type::Int),
            }),
        );
    }

    #[test]
    fn test_idents() {
        let mut ctx = HashMap::new();
        ctx.insert(String::from("foo"), Type::Int);
        ctx.insert(String::from("bar"), Type::Str);
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
