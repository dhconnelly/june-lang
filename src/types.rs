#[derive(Debug, PartialEq, Eq)]
pub struct FnType {
    params: Vec<Type>,
    ret: Box<Type>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    VoidType,
    IntType,
    StrType,
    FnType(FnType),
}
