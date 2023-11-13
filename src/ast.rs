use crate::types::{Type, Typed};

#[derive(Debug, PartialEq, Eq)]
pub struct Call<Assoc = ()> {
    pub target: Box<Expr<Assoc>>,
    pub args: Vec<Expr<Assoc>>,
    pub assoc: Assoc,
}

pub type TypedCall = Call<Type>;

impl Typed for TypedCall {
    fn typ(&self) -> &Type {
        &self.assoc
    }
}

impl Call<()> {
    pub fn new(target: Expr<()>, args: Vec<Expr<()>>) -> Call {
        Call { target: Box::new(target), args, assoc: () }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Primary<T, Assoc = ()> {
    pub cargo: T,
    pub assoc: Assoc,
}

pub type TypedPrimary<T> = Primary<T, Type>;

impl<T> Typed for TypedPrimary<T> {
    fn typ(&self) -> &Type {
        &self.assoc
    }
}

impl<T> Primary<T, ()> {
    pub fn new<U: Into<T>>(cargo: U) -> Primary<T> {
        Primary { cargo: cargo.into(), assoc: () }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr<Assoc = ()> {
    IdentExpr(Primary<String, Assoc>),
    StrExpr(Primary<String, Assoc>),
    IntExpr(Primary<i64, Assoc>),
    CallExpr(Call<Assoc>),
}

pub type TypedExpr = Expr<Type>;

impl Typed for TypedExpr {
    fn typ(&self) -> &Type {
        match self {
            Self::IdentExpr(expr) => expr.typ(),
            Self::StrExpr(expr) => expr.typ(),
            Self::IntExpr(expr) => expr.typ(),
            Self::CallExpr(expr) => expr.typ(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Param {
    pub name: String,
    pub typ: String,
}

#[derive(Debug, PartialEq)]
pub enum Stmt<Assoc = ()> {
    ExprStmt(Expr<Assoc>),
}

#[derive(Debug, PartialEq)]
pub struct Block<Assoc = ()>(pub Vec<Stmt<Assoc>>);

#[derive(Debug, PartialEq)]
pub struct FnExpr<Assoc = ()> {
    pub name: String,
    pub params: Vec<Param>,
    pub body: Block<Assoc>,
}

#[derive(Debug, PartialEq)]
pub enum Def<Assoc = ()> {
    FnDef(FnExpr<Assoc>),
}

#[derive(Debug, PartialEq)]
pub struct Program<Assoc = ()> {
    pub defs: Vec<Def<Assoc>>,
}

pub type TypedProgram = Program<Type>;
