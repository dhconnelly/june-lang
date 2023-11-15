use crate::analyzer;
use crate::parser;
use crate::scanner;
use std::fs;
use std::io;
use std::path;
use std::result;
use thiserror::Error;

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
}

pub type Result<T> = result::Result<T, Error>;

pub fn compile<R: io::Read, W: io::Write>(r: R, _w: W) -> Result<()> {
    let s = scanner::scan(io::BufReader::new(r));
    let mut p = parser::parse(s);
    let ast = p.program()?;
    let typed_ast = analyzer::analyze(ast)?;
    println!("{:#?}", typed_ast);
    Ok(())
}

fn make_output_path(p: &path::Path) -> Result<path::PathBuf> {
    let mut output_path = p.to_owned();
    if output_path.set_extension("wasm") {
        Ok(output_path)
    } else {
        Err(Error::OutputPathError(output_path))
    }
}

pub fn compile_file<P: Into<path::PathBuf>>(input_path: P) -> Result<()> {
    let input_path = input_path.into();
    let output_path = make_output_path(&input_path)?;
    let f = fs::File::open(&input_path)
        .map_err(|err| Error::IOError { path: input_path, err })?;
    let r = io::BufReader::new(f);
    let w = fs::File::create(&output_path)
        .map_err(|err| Error::IOError { path: output_path, err })?;
    compile(r, w)
}