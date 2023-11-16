use crate::ast::*;
use crate::wasm::*;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {}

type Result<T> = result::Result<T, Error>;

#[derive(Default)]
struct Emitter {
    code: Vec<Wasm>,
}

impl Emitter {
    fn program(&mut self, _program: &TypedProgram) -> Result<()> {
        Ok(())
    }

    fn code(self) -> Vec<Wasm> {
        self.code
    }
}

pub fn emit(program: &TypedProgram) -> Result<Vec<Wasm>> {
    let mut emitter = Emitter::default();
    emitter.program(program)?;
    Ok(emitter.code())
}
