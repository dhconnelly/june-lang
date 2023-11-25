#[derive(Debug)]
pub enum NumType {
    I64,
}

#[derive(Debug)]
pub enum ValType {
    NumType(NumType),
}

#[derive(Debug)]
pub enum Const {
    I64(i64),
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
    Const(Const),
    GetLocal(u32),
    Call(u32),
    Drop,
    AddI64,
    SubI64,
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

#[derive(Debug, Default)]
pub struct TypeSection(pub Vec<FuncType>);

#[derive(Debug, Default)]
pub struct FuncSection(pub Vec<Func>);

#[derive(Debug, Default)]
pub struct ExportSection(pub Vec<Export>);

#[derive(Debug, Default)]
pub struct CodeSection(pub Vec<Code>);

#[derive(Debug, Default)]
pub struct StartSection(pub u32);

#[derive(Debug)]
pub enum Section {
    Type(TypeSection),
    Func(FuncSection),
    Export(ExportSection),
    Code(CodeSection),
    Start(StartSection),
}

#[derive(Debug, Default)]
pub struct Module {
    pub types: TypeSection,
    pub funcs: FuncSection,
    pub exports: ExportSection,
    pub start: StartSection,
    pub code: CodeSection,
}
