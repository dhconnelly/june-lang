// TODO: get rid of glob imports
use crate::symbol_table::*;
use crate::types::*;
use crate::wasm::*;

pub fn install_symbols(ctx: &mut SymbolTable) {
    // TODO: generic println
    ctx.def_global(
        "println",
        Type::Fn(FnType {
            index: 0,
            params: vec![Type::Str],
            ret: Box::new(Type::Void),
        }),
    );
}

pub fn install_imports(module: &mut Module) {
    module.types.0.push(FuncType {
        params: vec![ValType::NumType(NumType::I64)],
        results: vec![],
    });
    module.imports.0.push(Import {
        module: String::from("june"),
        name: String::from("print"),
        import: ImportedValue::Func(0),
    });
}
