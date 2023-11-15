use crate::symbol_table::*;
use crate::types::*;

pub fn install(ctx: &mut SymbolTable) {
    ctx.def_global(
        "println",
        Type::Fn(FnType { params: vec![Type::Str], ret: Box::new(Type::Void) }),
    );
    ctx.def_global(
        "concat",
        Type::Fn(FnType {
            params: vec![Type::Str, Type::Str],
            ret: Box::new(Type::Str),
        }),
    );
    ctx.def_global(
        "itoa",
        Type::Fn(FnType { params: vec![Type::Int], ret: Box::new(Type::Str) }),
    );
}
