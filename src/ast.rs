#[derive(Debug, PartialEq, Eq)]
pub struct Call {
    pub line: usize,
    pub col: usize,
    pub target: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Primary<T> {
    pub line: usize,
    pub col: usize,
    pub cargo: T,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    IdentExpr(Primary<String>),
    StrExpr(Primary<String>),
    CallExpr(Call),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    pub line: usize,
    pub col: usize,
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
pub struct Fn {
    pub line: usize,
    pub col: usize,
    pub name: String,
    pub params: Vec<Param>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Def {
    Fn(Fn),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub defs: Vec<Def>,
}
