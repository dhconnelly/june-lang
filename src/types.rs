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

#[derive(Debug, Error, PartialEq)]
#[error("type mismatch: want {want}, got {got:?}")]
pub struct Error {
    pub want: String,
    pub got: Type,
}

pub type Result<T> = result::Result<T, Error>;

impl Type {
    pub fn as_fn(&self) -> Result<FnType> {
        if let Type::Fn(f) = self {
            Ok(f.clone())
        } else {
            Err(Error { want: String::from("Fn"), got: self.clone() })
        }
    }

    pub fn check(&self, other: &Type) -> Result<()> {
        if self == other {
            Ok(())
        } else {
            Err(Error { want: format!("{:?}", self), got: other.clone() })
        }
    }
}

pub trait Typed {
    fn typ(&self) -> &Type;
}
