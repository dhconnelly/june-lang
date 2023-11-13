#[derive(Debug, PartialEq, Eq)]
pub struct Call {
    pub target: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Primary<T> {
    pub cargo: T,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    IdentExpr(Primary<String>),
    StrExpr(Primary<String>),
    IntExpr(Primary<i64>),
    CallExpr(Call),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub typ: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    ExprStmt(Expr),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block(pub Vec<Stmt>);

#[derive(Debug, PartialEq, Eq)]
pub struct FnExpr {
    pub name: String,
    pub params: Vec<Param>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Def {
    FnDef(FnExpr),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub defs: Vec<Def>,
}
