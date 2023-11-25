use crate::ast;
use crate::wasm;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {}

type Result<T> = result::Result<T, Error>;

#[derive(Default)]
struct Translator {
    module: wasm::Module,
}

impl Translator {
    fn program(&mut self, _program: &ast::TypedProgram) -> Result<()> {
        self.module.funcs.0.push(wasm::Func { typeidx: 0 });
        self.module.types.0.push(wasm::FuncType {
            params: vec![
                wasm::ValType::NumType(wasm::NumType::I64),
                wasm::ValType::NumType(wasm::NumType::I64),
            ],
            results: vec![wasm::ValType::NumType(wasm::NumType::I64)],
        });
        self.module.code.0.push(wasm::Code {
            body: vec![
                wasm::Instr::GetLocal(0),
                wasm::Instr::GetLocal(1),
                wasm::Instr::SubI64,
                wasm::Instr::End,
            ],
            locals: vec![],
        });
        self.module.exports.0.push(wasm::Export {
            name: String::from("add"),
            desc: wasm::ExportDesc::Func(0),
        });

        self.module.funcs.0.push(wasm::Func { typeidx: 1 });
        self.module
            .types
            .0
            .push(wasm::FuncType { params: vec![], results: vec![] });
        self.module.code.0.push(wasm::Code {
            body: vec![
                wasm::Instr::Const(wasm::Const::I64(7)),
                wasm::Instr::Const(wasm::Const::I64(14)),
                wasm::Instr::Call(0),
                wasm::Instr::Drop,
                wasm::Instr::End,
            ],
            locals: vec![],
        });
        self.module.exports.0.push(wasm::Export {
            name: String::from("main"),
            desc: wasm::ExportDesc::Func(1),
        });

        self.module.start.0 = 1;

        Ok(())
    }

    fn translate(
        mut self,
        program: &ast::TypedProgram,
    ) -> Result<wasm::Module> {
        self.program(program)?;
        Ok(self.module)
    }
}

pub fn translate(program: &ast::TypedProgram) -> Result<wasm::Module> {
    Translator::default().translate(program)
}
