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

pub type Result<T> = result::Result<T, Error>;

struct SymbolInfo {
    idx: usize,
    typ: Type,
}

type StackFrame = HashMap<String, SymbolInfo>;

struct SymbolTable {
    // TODO: supporting forward references will require supporting empty values
    // in the globals table
    globals: HashMap<String, SymbolInfo>,
    frames: Vec<StackFrame>,
}

impl SymbolTable {
    fn new() -> Self {
        Self { globals: HashMap::new(), frames: Vec::new() }
    }

    fn def_global(&mut self, name: String, typ: Type) {
        let idx = self.globals.len();
        self.globals.insert(name, SymbolInfo { idx, typ });
    }

    fn push_frame(&mut self) {
        self.frames.push(HashMap::new());
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

    fn insert(&mut self, name: String, typ: Type) {
        let frame = self.frames.last_mut().unwrap();
        let idx = frame.len();
        frame.insert(name, SymbolInfo { idx, typ });
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

fn analyze_stmt(stmt: Stmt, ctx: &mut SymbolTable) -> Result<()> {
    match stmt {
        Stmt::ExprStmt(expr) => analyze_expr(expr, ctx).map(|_| ()),
        Stmt::LetStmt(_expr) => todo!(),
        Stmt::BlockStmt(_block) => todo!(),
    }
}

fn analyze_block(block: Block, ctx: &mut SymbolTable) -> Result<()> {
    let Block(stmts) = block;
    stmts.into_iter().try_for_each(|stmt| analyze_stmt(stmt, ctx))
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
    fn test_calls() {
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
            Err(Error::Type(types::Error {
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
    fn test_idents() {
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
    fn test_literals() {
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
