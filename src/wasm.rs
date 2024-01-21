use crate::types;

#[derive(Debug, PartialEq, Clone)]
pub enum NumType {
    I64,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ValType {
    NumType(NumType),
}

// TODO: get these out of here
impl From<types::Type> for ValType {
    fn from(typ: types::Type) -> Self {
        match typ {
            types::Type::Int => Self::NumType(NumType::I64),
            _ => todo!(),
        }
    }
}

// TODO: get these out of here
impl<T: Into<ValType>> From<Box<T>> for ValType {
    fn from(value: Box<T>) -> Self {
        let unboxed: T = *value;
        unboxed.into()
    }
}

#[derive(Debug, PartialEq)]
pub enum Const {
    I64(i64),
}

#[derive(Debug, PartialEq)]
pub struct FuncType {
    pub params: Vec<ValType>,
    pub results: Vec<ValType>,
}

// TODO: get these out of here
impl From<types::FnDef> for FuncType {
    fn from(func: types::FnDef) -> Self {
        let typ = func.typ;
        let params = typ.params.into_iter().map(|p| p.into()).collect();
        let results = typ.ret.map(|typ| typ.into()).into_iter().collect();
        Self { params, results }
    }
}

#[derive(Debug, PartialEq)]
pub struct Func {
    pub typeidx: u32,
}

#[derive(Debug, PartialEq)]
pub enum Instr {
    Const(Const),
    GetLocal(u32),
    SetLocal(u32),
    Call(u32),
    Drop,
    AddI64,
    SubI64,
    End,
}

#[derive(Debug, PartialEq)]
pub struct Code {
    pub locals: Vec<ValType>,
    pub body: Vec<Instr>,
}

#[derive(Debug)]
pub enum ExportDesc {
    Func(u32),
}

#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub desc: ExportDesc,
}

#[derive(Debug)]
pub enum ImportedValue {
    Func(u32),
}

#[derive(Debug)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub import: ImportedValue,
}

#[derive(Debug, Default)]
pub struct TypeSection(pub Vec<FuncType>);

#[derive(Debug, Default)]
pub struct ImportSection(pub Vec<Import>);

#[derive(Debug, Default)]
pub struct FuncSection(pub Vec<Func>);

#[derive(Debug, Default)]
pub struct ExportSection(pub Vec<Export>);

#[derive(Debug, Default)]
pub struct CodeSection(pub Vec<Code>);

#[derive(Debug, Default)]
pub struct StartSection(pub u32);

#[derive(Debug, Default)]
pub struct Module {
    pub types: TypeSection,
    pub imports: ImportSection,
    pub funcs: FuncSection,
    pub exports: ExportSection,
    pub start: StartSection,
    pub code: CodeSection,
}
