#[derive(Debug)]
pub enum NumType {
    I64,
}

#[derive(Debug)]
pub enum ValType {
    NumType(NumType),
}

#[derive(Debug)]
pub struct FuncType {
    pub params: Vec<ValType>,
    pub results: Vec<ValType>,
}

#[derive(Debug)]
pub struct Func {
    pub typeidx: u32,
}

#[derive(Debug)]
pub enum Instr {
    GetLocal(u32),
    Add(NumType),
    End,
}

#[derive(Debug)]
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
pub struct TypeSection(pub Vec<FuncType>);

#[derive(Debug)]
pub struct FuncSection(pub Vec<Func>);

#[derive(Debug)]
pub struct ExportSection(pub Vec<Export>);

#[derive(Debug)]
pub struct CodeSection(pub Vec<Code>);

#[derive(Debug)]
pub struct Module {
    pub types: Option<TypeSection>,
    pub funcs: Option<FuncSection>,
    pub exports: Option<ExportSection>,
    pub start: Option<u32>,
    pub code: Option<CodeSection>,
}
