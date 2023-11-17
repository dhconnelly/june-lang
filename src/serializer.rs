use crate::wasm::*;
use std::io::Write;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {}

pub type Result<T> = result::Result<T, Error>;

pub fn serialize<W: Write>(_w: W, _wasm: &[Wasm]) -> Result<()> {
    Ok(())
}
