use crate::types::{FnType, Resolution, Type, Typed};
use std::fmt;

// We have two AST variants: Typed and Untyped. To avoid defining the AST twice
// and keeping them in sync, we add an extra field, `cargo`, that is defined
// via generics. The Untyped AST carries Unit () as its cargo and the Typed AST
// holds type information. Since each AST node might need different type info,
// we define traits for each of the AST variants and specify the per-node cargo
// type via associated objects, a pattern similar to C++ policy-based design.
//
// This is overkill. But it's a fun exercise :)

pub trait ASTSpec
where
    <Self as ASTSpec>::CallCargo: fmt::Debug + Clone + PartialEq + Eq,
    <Self as ASTSpec>::IdentCargo: fmt::Debug + Clone + PartialEq + Eq,
    <Self as ASTSpec>::ExprCargo: fmt::Debug + Clone + PartialEq + Eq,
    <Self as ASTSpec>::ParamCargo: fmt::Debug + Clone + PartialEq + Eq,
    <Self as ASTSpec>::FuncCargo: fmt::Debug + Clone + PartialEq + Eq,
{
    type CallCargo;
    type IdentCargo;
    type ExprCargo;
    type ParamCargo;
    type FuncCargo;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UntypedAST;

impl ASTSpec for UntypedAST {
    type CallCargo = ();
    type IdentCargo = ();
    type ExprCargo = ();
    type ParamCargo = ();
    type FuncCargo = ();
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedAST;

impl ASTSpec for TypedAST {
    type CallCargo = Type;
    type IdentCargo = Resolution;
    type ExprCargo = Type;
    type ParamCargo = Type;
    type FuncCargo = FnType;
}

// =============================================================================
// Call

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call<AST: ASTSpec = UntypedAST> {
    // TODO: Rc?
    pub target: Box<Expr<AST>>,
    pub args: Vec<Expr<AST>>,
    pub cargo: AST::CallCargo,
}

impl Call<UntypedAST> {
    pub fn untyped(target: Expr, args: Vec<Expr>) -> Call<UntypedAST> {
        Call { target: Box::new(target), args, cargo: () }
    }
}

pub type TypedCall = Call<TypedAST>;

impl Typed for TypedCall {
    fn typ(&self) -> Type {
        self.cargo.clone()
    }
}

// =============================================================================
// Literal

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Literal<T: PartialEq + Eq + Clone> {
    pub value: T,
}

impl<T: fmt::Debug + PartialEq + Eq + Clone> Literal<T> {
    pub fn new<U: Into<T>>(value: U) -> Literal<T> {
        Literal { value: value.into() }
    }
}

impl Typed for Literal<String> {
    fn typ(&self) -> Type {
        Type::Str
    }
}

impl Typed for Literal<i64> {
    fn typ(&self) -> Type {
        Type::Int
    }
}

// =============================================================================
// Ident

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ident<AST: ASTSpec = UntypedAST> {
    pub name: String,
    pub cargo: AST::IdentCargo,
}

impl Ident<UntypedAST> {
    pub fn untyped<S: Into<String>>(name: S) -> Ident<UntypedAST> {
        Ident { name: name.into(), cargo: () }
    }
}

pub type TypedIdent = Ident<TypedAST>;

impl Typed for TypedIdent {
    fn typ(&self) -> Type {
        self.cargo.typ.clone()
    }
}

// =============================================================================
// Expr

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr<AST: ASTSpec = UntypedAST> {
    IdentExpr(Ident<AST>),
    StrExpr(Literal<String>),
    IntExpr(Literal<i64>),
    CallExpr(Call<AST>),
    FuncExpr(Func<AST>),
}

pub type TypedExpr = Expr<TypedAST>;

impl Typed for TypedExpr {
    fn typ(&self) -> Type {
        use Expr::*;
        match self {
            IdentExpr(expr) => expr.typ(),
            StrExpr(expr) => expr.typ(),
            IntExpr(expr) => expr.typ(),
            CallExpr(expr) => expr.typ(),
            FuncExpr(expr) => expr.typ(),
        }
    }
}

// =============================================================================
// TypeSpec

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeSpec {
    Void,
    Simple(String),
}

// =============================================================================
// Param

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Param<AST: ASTSpec = UntypedAST> {
    pub name: String,
    pub typ: TypeSpec,
    pub cargo: AST::ParamCargo,
}

impl Param<UntypedAST> {
    pub fn untyped<S: Into<String>>(
        name: S,
        typ: TypeSpec,
    ) -> Param<UntypedAST> {
        Param { name: name.into(), typ, cargo: () }
    }
}

pub type TypedParam = Param<TypedAST>;

impl Typed for TypedParam {
    fn typ(&self) -> Type {
        self.cargo.clone()
    }
}

// =============================================================================
// Stmt

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt<AST: ASTSpec = UntypedAST> {
    ExprStmt(Expr<AST>),
}

pub type TypedStmt = Stmt<TypedAST>;

// =============================================================================
// Block

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block<AST: ASTSpec = UntypedAST>(pub Vec<Stmt<AST>>);

pub type TypedBlock = Block<TypedAST>;

// =============================================================================
// Func

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Func<AST: ASTSpec = UntypedAST> {
    pub name: String,
    pub params: Vec<Param<AST>>,
    pub body: Block<AST>,
    pub ret: TypeSpec,
    pub cargo: AST::FuncCargo,
}

impl Func<UntypedAST> {
    pub fn untyped<S: Into<String>>(
        name: S,
        params: Vec<Param>,
        body: Block,
        ret: TypeSpec,
    ) -> Func<UntypedAST> {
        Func { name: name.into(), params, body, ret, cargo: () }
    }
}

pub type TypedFunc = Func<TypedAST>;

impl Typed for TypedFunc {
    fn typ(&self) -> Type {
        Type::Fn(self.cargo.clone())
    }
}

// =============================================================================
// Def

#[derive(Debug, PartialEq, Clone)]
pub enum Def<AST: ASTSpec = UntypedAST> {
    FnDef(Func<AST>),
}

pub type TypedDef = Def<TypedAST>;

// =============================================================================
// Program

#[derive(Debug, PartialEq, Clone)]
pub struct Program<AST: ASTSpec = UntypedAST> {
    pub defs: Vec<Def<AST>>,
}

pub type TypedProgram = Program<TypedAST>;
