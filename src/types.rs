#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnType {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Void,
    Int,
    Str,
    Fn(FnType),
}

pub trait Typed {
    fn typ(&self) -> Type;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Reference {
    Global { idx: usize },
    Stack { frame_depth: usize, frame_idx: usize },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Resolution {
    // TODO: I don't think we need |typ| here
    pub typ: Type,
    pub reference: Reference,
}
