use crate::ast::*;
use crate::wasm::*;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {}

type Result<T> = result::Result<T, Error>;

pub fn compile(_program: TypedProgram) -> Result<Vec<Wasm>> {
    Ok(Vec::new())
}
