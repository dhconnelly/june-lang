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
//
// More ideas:
//
// - http://lambda-the-ultimate.org/node/4170
// - http://blog.ezyang.com/2013/05/the-ast-typing-problem/
// - https://news.ycombinator.com/item?id=37114976
// - https://www.reddit.com/r/ProgrammingLanguages/comments/b7fvlv/ast_and_tast/
// - https://www.reddit.com/r/Compilers/comments/x3d3r6/type_information_in_the_ast/

pub trait ASTSpec {
    type CallCargo: fmt::Debug + PartialEq + Eq + Clone;
    type IdentCargo: fmt::Debug + PartialEq + Eq + Clone;
    type ExprCargo: fmt::Debug + PartialEq + Eq + Clone;
    type ParamCargo: fmt::Debug + PartialEq + Eq + Clone;
    type FuncCargo: fmt::Debug + PartialEq + Eq + Clone;
    type LetCargo: fmt::Debug + PartialEq + Eq + Clone;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UntypedAST;

impl ASTSpec for UntypedAST {
    type CallCargo = ();
    type IdentCargo = ();
    type ExprCargo = ();
    type ParamCargo = ();
    type FuncCargo = ();
    type LetCargo = ();
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedAST;

impl ASTSpec for TypedAST {
    type CallCargo = Type;
    type IdentCargo = Resolution;
    type ExprCargo = Type;
    type ParamCargo = Type;
    type FuncCargo = FnType;
    type LetCargo = Type;
}

// =============================================================================
// Call

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call<AST: ASTSpec = UntypedAST> {
    pub target: Box<Expr<AST>>,
    pub args: Vec<Expr<AST>>,
    pub resolved_type: AST::CallCargo,
}

impl Call<UntypedAST> {
    pub fn untyped(target: Expr, args: Vec<Expr>) -> Call<UntypedAST> {
        Call { target: Box::new(target), args, resolved_type: () }
    }
}

pub type TypedCall = Call<TypedAST>;

impl Typed for TypedCall {
    fn typ(&self) -> Type {
        self.resolved_type.clone()
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
    pub resolution: AST::IdentCargo,
}

impl Ident<UntypedAST> {
    pub fn untyped<S: Into<String>>(name: S) -> Ident<UntypedAST> {
        Ident { name: name.into(), resolution: () }
    }
}

pub type TypedIdent = Ident<TypedAST>;

impl Typed for TypedIdent {
    fn typ(&self) -> Type {
        self.resolution.typ.clone()
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

impl TypeSpec {
    pub fn simple<S: Into<String>>(s: S) -> TypeSpec {
        TypeSpec::Simple(s.into())
    }
}

// =============================================================================
// Param

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Param<AST: ASTSpec = UntypedAST> {
    pub name: String,
    pub typ: TypeSpec,
    pub resolved_type: AST::ParamCargo,
}

impl Param<UntypedAST> {
    pub fn untyped<S: Into<String>>(
        name: S,
        typ: TypeSpec,
    ) -> Param<UntypedAST> {
        Param { name: name.into(), typ, resolved_type: () }
    }
}

pub type TypedParam = Param<TypedAST>;

impl Typed for TypedParam {
    fn typ(&self) -> Type {
        self.resolved_type.clone()
    }
}

// =============================================================================
// Binding

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Binding<AST: ASTSpec = UntypedAST> {
    pub name: String,
    pub typ: TypeSpec,
    pub expr: Expr<AST>,
    pub resolved_type: AST::LetCargo,
}

pub type TypedBinding = Binding<TypedAST>;

impl<AST: ASTSpec> Binding<AST> {
    pub fn new(
        name: String,
        typ: TypeSpec,
        expr: Expr<AST>,
        resolved_type: AST::LetCargo,
    ) -> Binding<AST> {
        Binding { name, typ, expr, resolved_type }
    }
}

// =============================================================================
// Stmt

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt<AST: ASTSpec = UntypedAST> {
    ExprStmt(Expr<AST>),
    BlockStmt(Block<AST>),
    LetStmt(Binding<AST>),
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
    pub resolved_type: AST::FuncCargo,
}

impl Func<UntypedAST> {
    pub fn untyped<S: Into<String>>(
        name: S,
        params: Vec<Param>,
        body: Block,
        ret: TypeSpec,
    ) -> Func<UntypedAST> {
        Func { name: name.into(), params, body, ret, resolved_type: () }
    }
}

pub type TypedFunc = Func<TypedAST>;

impl Typed for TypedFunc {
    fn typ(&self) -> Type {
        Type::Fn(self.resolved_type.clone())
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
