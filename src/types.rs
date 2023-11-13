#[derive(Debug, PartialEq, Clone)]
pub struct FnType {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Void,
    Int,
    Str,
    Fn(FnType),
}

pub trait Typed {
    fn typ(&self) -> &Type;
}
