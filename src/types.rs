#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnType {
    pub index: usize,
    pub params: Vec<Type>,
    pub ret: Option<Box<Type>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnDef {
    pub typ: FnType,
    pub locals: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Void,
    Int,
    Str,
    Fn(FnType),
}

impl From<Option<Box<Type>>> for Type {
    fn from(value: Option<Box<Type>>) -> Self {
        match value {
            None => Type::Void,
            Some(typ) => *typ,
        }
    }
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
    External,
    Global { idx: usize },
    Stack { local_idx: usize },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LocalBinding {
    pub typ: Type,
    pub idx: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Resolution {
    // TODO: I don't think we need |typ| here
    pub typ: Type,
    pub reference: Reference,
}
