use crate::wasm::*;
use std::io;
use std::io::Write;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("io: {0}")]
    IOError(#[from] io::Error),
}

pub type Result<T> = result::Result<T, Error>;

// https://webassembly.github.io/spec/core/binary/modules.html#binary-module
const HEADER: &'static [u8] = &[0x00, 0x61, 0x73, 0x6d];
const VERSION: &'static [u8] = &[0x01, 0x00, 0x00, 0x00];

pub fn emit<W: Write>(mut w: W, _wasm: &[Wasm]) -> Result<()> {
    w.write_all(HEADER)?;
    w.write_all(VERSION)?;
    Ok(())
}
