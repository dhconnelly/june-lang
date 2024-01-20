use crate::ast;
use crate::builtins;
use crate::wasm;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {}

type Result<T> = result::Result<T, Error>;

struct Translator {
    module: wasm::Module,
}

impl Default for Translator {
    fn default() -> Self {
        let mut module = wasm::Module::default();
        builtins::install_imports(&mut module);
        Self { module }
    }
}

impl Translator {
    fn empty() -> Self {
        Self { module: wasm::Module::default() }
    }

    fn int_literal(
        &self,
        lit: ast::IntLiteral,
        instrs: &mut Vec<wasm::Instr>,
    ) -> Result<()> {
        instrs.push(wasm::Instr::Const(wasm::Const::I64(lit.value)));
        Ok(())
    }

    fn binary(
        &self,
        binary: ast::TypedBinary,
        instrs: &mut Vec<wasm::Instr>,
    ) -> Result<()> {
        self.expr(*binary.lhs, instrs)?;
        self.expr(*binary.rhs, instrs)?;
        // TODO: handle generic operations
        match binary.op {
            ast::BinaryOp::Add => instrs.push(wasm::Instr::AddI64),
            ast::BinaryOp::Sub => instrs.push(wasm::Instr::SubI64),
            ast::BinaryOp::Mul => todo!(),
            ast::BinaryOp::Div => todo!(),
        }
        Ok(())
    }

    fn call(
        &self,
        call: ast::TypedCall,
        instrs: &mut Vec<wasm::Instr>,
    ) -> Result<()> {
        // TODO: encode these invariants in the call ast node
        let fn_type = call
            .target
            .as_ident()
            .expect("typed call target must be an ident")
            .resolution
            .typ
            .as_fn()
            .expect("typed call target must have fn type");
        assert_eq!(call.args.len(), fn_type.params.len());
        for arg in call.args {
            self.expr(arg, instrs)?;
        }
        instrs.push(wasm::Instr::Call(fn_type.index as u32));
        Ok(())
    }

    fn expr(
        &self,
        expr: ast::TypedExpr,
        instrs: &mut Vec<wasm::Instr>,
    ) -> Result<()> {
        match expr {
            ast::Expr::Int(int) => self.int_literal(int, instrs),
            ast::Expr::Binary(binary) => self.binary(binary, instrs),
            ast::Expr::Call(call) => self.call(call, instrs),
            _ => todo!(),
        }
    }

    fn func(&mut self, func: ast::TypedFunc) -> Result<()> {
        // type
        // func
        // code
        Ok(())
    }

    fn program(&mut self, program: ast::TypedProgram) -> Result<()> {
        for def in program.defs {
            match def {
                ast::Def::FnDef(func) => self.func(func)?,
            }
        }
        Ok(())
    }

    fn _foo(&mut self) {
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
        self.module.types.0.push(wasm::FuncType { params: vec![], results: vec![] });
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
    }

    fn translate(mut self, program: ast::TypedProgram) -> Result<wasm::Module> {
        self.program(program)?;
        Ok(self.module)
    }
}

pub fn translate(program: ast::TypedProgram) -> Result<wasm::Module> {
    Translator::default().translate(program)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::types;

    fn translate_expr(expr: ast::TypedExpr) -> Vec<wasm::Instr> {
        let mut instrs = Vec::new();
        Translator::empty().expr(expr, &mut instrs).unwrap();
        instrs
    }

    #[test]
    fn test_int_literal() {
        let instrs = translate_expr(ast::TypedExpr::Int(ast::Literal::new(247)));
        let expected = vec![wasm::Instr::Const(wasm::Const::I64(247))];
        assert_eq!(instrs, expected);
    }

    #[test]
    fn test_binary() {
        let instrs = translate_expr(ast::TypedExpr::Binary(ast::TypedBinary {
            op: ast::BinaryOp::Add,
            lhs: Box::new(ast::TypedExpr::Int(ast::Literal::new(3))),
            rhs: Box::new(ast::TypedExpr::Int(ast::Literal::new(16))),
            cargo: types::Type::Int,
        }));
        let expected = vec![
            wasm::Instr::Const(wasm::Const::I64(3)),
            wasm::Instr::Const(wasm::Const::I64(16)),
            wasm::Instr::AddI64,
        ];
        assert_eq!(instrs, expected);
    }

    #[test]
    fn test_call() {
        let instrs = translate_expr(ast::TypedExpr::Call(ast::TypedCall {
            target: Box::new(ast::TypedExpr::Ident(ast::TypedIdent {
                name: String::from("add"),
                resolution: types::Resolution {
                    typ: types::Type::Fn(types::FnType {
                        index: 3,
                        params: vec![types::Type::Int, types::Type::Int],
                        ret: Box::new(types::Type::Int),
                    }),
                    reference: types::Reference::Global { idx: 17 },
                },
            })),
            args: vec![
                ast::TypedExpr::Int(ast::Literal::new(3)),
                ast::TypedExpr::Int(ast::Literal::new(4)),
            ],
            resolved_type: types::Type::Int,
        }));
        let expected = vec![
            wasm::Instr::Const(wasm::Const::I64(3)),
            wasm::Instr::Const(wasm::Const::I64(4)),
            wasm::Instr::Call(3),
        ];
        assert_eq!(instrs, expected);
    }
}
