use crate::ast;
use crate::wasm;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {}

type Result<T> = result::Result<T, Error>;

#[derive(Default)]
struct Translator {}

impl Translator {
    fn program(
        &mut self,
        _program: &ast::TypedProgram,
    ) -> Result<wasm::Module> {
        Ok(wasm::Module {
            funcs: Some(wasm::FuncSection(vec![wasm::Func { typeidx: 0 }])),
            types: Some(wasm::TypeSection(vec![wasm::FuncType {
                params: vec![
                    wasm::ValType::NumType(wasm::NumType::I64),
                    wasm::ValType::NumType(wasm::NumType::I64),
                ],
                results: vec![wasm::ValType::NumType(wasm::NumType::I64)],
            }])),
            code: Some(wasm::CodeSection(vec![wasm::Code {
                body: vec![
                    wasm::Instr::GetLocal(0),
                    wasm::Instr::GetLocal(1),
                    wasm::Instr::Add(wasm::NumType::I64),
                    wasm::Instr::End,
                ],
                locals: vec![],
            }])),
            start: None,
            exports: Some(wasm::ExportSection(vec![wasm::Export {
                name: String::from("add"),
                desc: wasm::ExportDesc::Func(0),
            }])),
        })
    }
}

pub fn translate(program: &ast::TypedProgram) -> Result<wasm::Module> {
    let mut translator = Translator::default();
    Ok(translator.program(program)?)
}
