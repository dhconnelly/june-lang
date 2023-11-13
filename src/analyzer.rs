use crate::ast::{Expr::*, *};
use crate::types::Type::*;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {}

type Result<T> = result::Result<T, Error>;

pub fn analyze_expr(expr: Expr) -> Result<TypedExpr> {
    match expr {
        IntExpr(prim) => {
            Ok(IntExpr(TypedPrimary { cargo: prim.cargo, assoc: IntType }))
        }
        StrExpr(prim) => {
            Ok(StrExpr(TypedPrimary { cargo: prim.cargo, assoc: StrType }))
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

    #[test]
    fn test_literals() {
        let inputs: &[&[u8]] = &[b"27", b"\"hello, world\""];
        let expected = vec![
            TypedExpr::IntExpr(Primary { cargo: 27, assoc: IntType }),
            TypedExpr::StrExpr(Primary {
                cargo: String::from("hello, world"),
                assoc: StrType,
            }),
        ];
        let actual: Vec<TypedExpr> = inputs
            .iter()
            .map(|input| parse(*input).expr().unwrap())
            .map(|expr| analyze_expr(expr).unwrap())
            .collect();
        assert_eq!(expected, actual);
    }
}
