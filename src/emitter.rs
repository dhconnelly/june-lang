use crate::wasm::*;
use leb128;
use std::io;
use std::io::Write;
use std::result;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("io: {0}")]
    IOError(#[from] io::Error),
    #[error("vector too long: max len 2^32, found {0}")]
    LengthError(usize),
}

pub type Result<T> = result::Result<T, Error>;

const HEADER: &'static [u8] = &[0x00, 0x61, 0x73, 0x6d];
const VERSION: &'static [u8] = &[0x01, 0x00, 0x00, 0x00];

static TYPE_SECTION: u8 = 1;
static FUNCTION_SECTION: u8 = 3;
static EXPORT_SECTION: u8 = 7;
static CODE_SECTION: u8 = 10;

trait Writable<W: Write> {
    fn write(&self, w: &mut W) -> Result<()>;
}

// https://webassembly.github.io/spec/core/binary/index.html

fn write_len<W: Write>(w: &mut W, int: usize) -> Result<()> {
    let int: u32 = int.try_into().or(Err(Error::LengthError(int)))?;
    leb128::write::unsigned(w, int as u64)?;
    Ok(())
}

fn vec<W: Write, T: Writable<W>>(w: &mut W, v: &[T]) -> Result<()> {
    // https://webassembly.github.io/spec/core/binary/conventions.html#vectors
    write_len(w, v.len())?;
    for item in v {
        item.write(w)?;
    }
    Ok(())
}

impl<W: Write> Writable<W> for ValType {
    fn write(&self, w: &mut W) -> Result<()> {
        match self {
            ValType::NumType(NumType::I64) => w.write_all(&[0x7E])?,
        }
        Ok(())
    }
}

impl<W: Write> Writable<W> for FuncType {
    fn write(&self, w: &mut W) -> Result<()> {
        w.write_all(&[0x60])?;
        vec(w, &self.params)?;
        vec(w, &self.results)?;
        Ok(())
    }
}

impl<W: Write> Writable<W> for TypeSection {
    fn write(&self, w: &mut W) -> Result<()> {
        w.write_all(&[TYPE_SECTION])?;
        let mut buf = Vec::new();
        vec(&mut buf, &self.0)?;
        write_len(w, buf.len())?;
        w.write_all(&buf)?;
        Ok(())
    }
}

impl<W: Write> Writable<W> for Func {
    fn write(&self, w: &mut W) -> Result<()> {
        leb128::write::unsigned(w, self.typeidx as u64)?;
        Ok(())
    }
}

impl<W: Write> Writable<W> for FuncSection {
    fn write(&self, w: &mut W) -> Result<()> {
        w.write_all(&[FUNCTION_SECTION])?;
        let mut buf = Vec::new();
        vec(&mut buf, &self.0)?;
        write_len(w, buf.len())?;
        w.write_all(&buf)?;
        Ok(())
    }
}

impl<W: Write> Writable<W> for Instr {
    fn write(&self, w: &mut W) -> Result<()> {
        match self {
            Instr::End => w.write_all(&[0x0B])?,
            _ => todo!(),
        }
        Ok(())
    }
}

impl<W: Write> Writable<W> for Code {
    fn write(&self, w: &mut W) -> Result<()> {
        let mut buf = Vec::new();
        vec(&mut buf, &self.locals)?;
        vec(&mut buf, &self.body)?;
        write_len(w, buf.len())?;
        w.write_all(&buf)?;
        Ok(())
    }
}

impl<W: Write> Writable<W> for CodeSection {
    fn write(&self, w: &mut W) -> Result<()> {
        w.write_all(&[CODE_SECTION])?;
        let mut buf = Vec::new();
        vec(&mut buf, &self.0)?;
        write_len(w, buf.len())?;
        w.write_all(&buf)?;
        Ok(())
    }
}

impl<W: Write> Writable<W> for Export {
    fn write(&self, w: &mut W) -> Result<()> {
        write_len(w, self.name.len())?;
        w.write_all(self.name.as_bytes())?;
        match self.desc {
            ExportDesc::Func(idx) => {
                w.write_all(&[0x00])?;
                leb128::write::unsigned(w, idx as u64)?;
            }
        }
        Ok(())
    }
}

impl<W: Write> Writable<W> for ExportSection {
    fn write(&self, w: &mut W) -> Result<()> {
        w.write_all(&[EXPORT_SECTION])?;
        let mut buf = Vec::new();
        vec(&mut buf, &self.0)?;
        write_len(w, buf.len())?;
        w.write_all(&buf)?;
        Ok(())
    }
}

fn write_section<W: Write, Section: Writable<W>>(
    w: &mut W,
    section: Option<&Section>,
) -> Result<()> {
    if let Some(section) = section {
        section.write(w)
    } else {
        Ok(())
    }
}

pub fn emit<W: Write>(w: &mut W, module: &Module) -> Result<()> {
    w.write_all(HEADER)?;
    w.write_all(VERSION)?;
    write_section(w, module.types.as_ref())?;
    write_section(w, module.funcs.as_ref())?;
    write_section(w, module.exports.as_ref())?;
    write_section(w, module.code.as_ref())?;
    Ok(())
}
