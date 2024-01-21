// TODO: get rid of glob imports
use crate::symbol_table::*;
use crate::types::*;
use crate::wasm::*;

fn install_function(name: impl ToString, typ: FnType, ctx: &mut SymbolTable) {
    ctx.def_global(name, Type::Fn(typ.clone()));
    ctx.def_fn(typ.params, typ.ret.map(|ret| *ret));
}

pub fn install_symbols(ctx: &mut SymbolTable) {
    // TODO: generic println
    install_function(
        "println",
        FnType { index: 0, params: vec![Type::Int], ret: None },
        ctx,
    );
}

pub fn install_imports(module: &mut Module) {
    module.types.0.push(FuncType {
        params: vec![ValType::NumType(NumType::I64)],
        results: vec![],
    });
    module.imports.0.push(Import {
        module: String::from("june"),
        name: String::from("println"),
        import: ImportedValue::Func(0),
    });
}

pub fn make_imports<T>(
    mut store: impl wasmtime::AsContextMut<Data = T>,
) -> Vec<wasmtime::Extern> {
    // TODO: make |print| generic
    let print = wasmtime::Func::wrap(&mut store, |x: i64| println!("{}", x));
    vec![wasmtime::Extern::Func(print)]
}
