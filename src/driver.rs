use crate::analyzer;
use crate::emitter;
use crate::imports;
use crate::parser;
use crate::scanner;
use crate::translator;
use std::fs;
use std::io;
use std::path;
use std::result;
use thiserror::Error;
use wasmtime;

#[derive(Error, Debug)]
pub enum Error {
    #[error("can't create output path: {0}")]
    OutputPathError(path::PathBuf),
    #[error("io: {path}: {err}")]
    IOError { path: path::PathBuf, err: io::Error },
    #[error("scanner: {0}")]
    ScannerError(#[from] scanner::Error),
    #[error("parser: {0}")]
    ParserError(#[from] parser::Error),
    #[error("analyzer: {0}")]
    AnalyzerError(#[from] analyzer::Error),
    #[error("translator: {0}")]
    TranslatorError(#[from] translator::Error),
    #[error("emitter: {0}")]
    EmitterError(#[from] emitter::Error),
    #[error("wasmtime: {0}")]
    Wasmtime(#[from] wasmtime::Error),
}

pub type Result<T> = result::Result<T, Error>;

fn make_output_path(p: &path::Path) -> Result<path::PathBuf> {
    let mut output_path = p.to_owned();
    if output_path.set_extension("wasm") {
        Ok(output_path)
    } else {
        Err(Error::OutputPathError(output_path))
    }
}

pub fn compile<W: io::Write, R: io::Read>(mut w: W, r: R) -> Result<()> {
    let toks = scanner::scan(io::BufReader::new(r));
    let ast = parser::parse(toks)?;
    let typed_ast = analyzer::analyze(ast)?;
    let wasm = translator::translate(typed_ast)?;
    println!("{:#?}", wasm);
    Ok(emitter::emit(&mut w, wasm)?)
}

pub fn compile_file<P: Into<path::PathBuf>>(input_path: P) -> Result<()> {
    let input_path = input_path.into();
    let output_path = make_output_path(&input_path)?;
    let f = fs::File::open(&input_path)
        .map_err(|err| Error::IOError { path: input_path, err })?;
    let r = io::BufReader::new(f);
    let w = fs::File::create(&output_path)
        .map_err(|err| Error::IOError { path: output_path, err })?;
    compile(w, r)
}

pub fn execute<P: AsRef<path::Path>>(input_path: P) -> Result<()> {
    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::from_file(&engine, input_path)?;
    let mut store = wasmtime::Store::new(&engine, ());
    let imports = imports::make_imports(&mut store);
    wasmtime::Instance::new(&mut store, &module, &imports)?;
    Ok(())
}
