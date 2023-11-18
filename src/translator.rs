use crate::ast::*;
use crate::wasm::*;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {}

type Result<T> = result::Result<T, Error>;

#[derive(Default)]
struct Translator {
    code: Vec<Wasm>,
}

impl Translator {
    fn program(&mut self, _program: &TypedProgram) -> Result<()> {
        Ok(())
    }

    fn code(self) -> Vec<Wasm> {
        self.code
    }
}

pub fn translate(program: &TypedProgram) -> Result<Vec<Wasm>> {
    let mut translator = Translator::default();
    translator.program(program)?;
    Ok(translator.code())
}
