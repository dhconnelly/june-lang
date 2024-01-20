#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnType {
    pub index: usize,
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

impl Type {
    pub fn as_fn(self) -> Option<FnType> {
        if let Self::Fn(f) = self {
            Some(f)
        } else {
            None
        }
    }
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
