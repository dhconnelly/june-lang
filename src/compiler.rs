use crate::parser;
use crate::scanner;
use std::fs::File;
use std::io;
use std::io::BufReader;
use std::path::Path;
use std::result;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("io: {0}")]
    IOError(#[from] io::Error),

    #[error("scanner: {0}")]
    ScannerError(#[from] scanner::ScannerError),

    #[error("parser: {0}")]
    ParserError(#[from] parser::ParserError),
}

type Result<T> = result::Result<T, Error>;

pub struct Compiler;

pub fn compile(p: impl AsRef<Path>) -> Result<()> {
    let f = File::open(p)?;
    let scan = scanner::scan(BufReader::new(f));
    let ast = parser::parse(scan)?;
    println!("{:?}", ast);
    Ok(())
}
