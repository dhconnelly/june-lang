use std::result;
use thiserror::Error;

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

impl Type {
    pub fn check(&self, other: &Type) -> Result<()> {
        if self == other {
            Ok(())
        } else {
            Err(Error { want: format!("{:?}", self), got: other.clone() })
        }
    }
}

pub trait Typed {
    fn typ(&self) -> Type;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Resolution {
    pub typ: Type,
    pub frame_depth: usize,
    pub frame_offset: usize,
}

#[derive(Debug, Error, PartialEq)]
#[error("type mismatch: want {want}, got {got:?}")]
pub struct Error {
    pub want: String,
    pub got: Type,
}

pub type Result<T> = result::Result<T, Error>;
