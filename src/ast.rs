use crate::types::Type;

#[derive(Debug, PartialEq, Eq)]
pub struct Call<Assoc = ()> {
    pub target: Box<Expr<Assoc>>,
    pub args: Vec<Expr<Assoc>>,
    pub assoc: Assoc,
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

#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub typ: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt<Assoc = ()> {
    ExprStmt(Expr<Assoc>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block<Assoc = ()>(pub Vec<Stmt<Assoc>>);

#[derive(Debug, PartialEq, Eq)]
pub struct FnExpr<Assoc = ()> {
    pub name: String,
    pub params: Vec<Param>,
    pub body: Block<Assoc>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Def<Assoc = ()> {
    FnDef(FnExpr<Assoc>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program<Assoc = ()> {
    pub defs: Vec<Def<Assoc>>,
}

pub type TypedProgram = Program<Type>;
