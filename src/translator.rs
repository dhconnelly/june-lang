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
        // print
        self.module.types.0.push(wasm::FuncType {
            params: vec![wasm::ValType::NumType(wasm::NumType::I64)],
            results: vec![],
        });
        self.module.imports.0.push(wasm::Import {
            module: String::from("june"),
            name: String::from("print"),
            import: wasm::ImportedValue::Func(0),
        });

        // add
        self.module.types.0.push(wasm::FuncType {
            params: vec![
                wasm::ValType::NumType(wasm::NumType::I64),
                wasm::ValType::NumType(wasm::NumType::I64),
            ],
            results: vec![wasm::ValType::NumType(wasm::NumType::I64)],
        });
        self.module.funcs.0.push(wasm::Func { typeidx: 1 });
        self.module.code.0.push(wasm::Code {
            body: vec![
                wasm::Instr::GetLocal(0),
                wasm::Instr::GetLocal(1),
                wasm::Instr::AddI64,
                wasm::Instr::End,
            ],
            locals: vec![],
        });

        // main
        self.module
            .types
            .0
            .push(wasm::FuncType { params: vec![], results: vec![] });
        self.module.funcs.0.push(wasm::Func { typeidx: 2 });
        self.module.code.0.push(wasm::Code {
            body: vec![
                wasm::Instr::Const(wasm::Const::I64(7)),
                wasm::Instr::Const(wasm::Const::I64(14)),
                wasm::Instr::Call(1),
                wasm::Instr::Call(0),
                wasm::Instr::End,
            ],
            locals: vec![],
        });

        self.module.start.0 = 2;

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
