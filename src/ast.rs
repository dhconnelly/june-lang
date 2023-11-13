use crate::types::{Type, Typed};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call<Assoc = ()> {
    // TODO: Rc?
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr<Assoc = ()> {
    IdentExpr(Primary<String, Assoc>),
    StrExpr(Primary<String, Assoc>),
    IntExpr(Primary<i64, Assoc>),
    CallExpr(Call<Assoc>),
    FuncExpr(Func<Assoc>),
}

pub type TypedExpr = Expr<Type>;

impl Typed for TypedExpr {
    fn typ(&self) -> &Type {
        match self {
            Self::IdentExpr(expr) => expr.typ(),
            Self::StrExpr(expr) => expr.typ(),
            Self::IntExpr(expr) => expr.typ(),
            Self::CallExpr(expr) => expr.typ(),
            Self::FuncExpr(expr) => expr.typ(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeSpec {
    Void,
    Simple(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Param {
    pub name: String,
    pub typ: TypeSpec,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt<Assoc = ()> {
    ExprStmt(Expr<Assoc>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block<Assoc = ()>(pub Vec<Stmt<Assoc>>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Func<Assoc = ()> {
    pub name: String,
    pub params: Vec<Param>,
    pub body: Block<Assoc>,
    pub ret: TypeSpec,
    // TODO: figure out how to make this statically FnType
    pub assoc: Assoc,
}

pub type TypedFunc = Func<Type>;

impl Typed for TypedFunc {
    fn typ(&self) -> &Type {
        &self.assoc
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Def<Assoc = ()> {
    FnDef(Func<Assoc>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program<Assoc = ()> {
    pub defs: Vec<Def<Assoc>>,
}

pub type TypedProgram = Program<Type>;
