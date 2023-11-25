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

const HEADER: &[u8] = &[0x00, 0x61, 0x73, 0x6d];
const VERSION: &[u8] = &[0x01, 0x00, 0x00, 0x00];

trait Writable<W: Write> {
    fn write(&self, w: &mut W) -> Result<()>;
}

// https://webassembly.github.io/spec/core/binary/index.html

fn write_unsigned<W: Write>(w: &mut W, int: u32) -> Result<()> {
    leb128::write::unsigned(w, int as u64)?;
    Ok(())
}

fn write_len<W: Write>(w: &mut W, len: usize) -> Result<()> {
    let len = len.try_into().or(Err(Error::LengthError(len)))?;
    write_unsigned(w, len)?;
    Ok(())
}

fn vec<W: Write, T: Writable<W>>(w: &mut W, v: &[T]) -> Result<()> {
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

impl<W: Write> Writable<W> for Func {
    fn write(&self, w: &mut W) -> Result<()> {
        write_unsigned(w, self.typeidx)?;
        Ok(())
    }
}

impl<W: Write> Writable<W> for Const {
    fn write(&self, w: &mut W) -> Result<()> {
        match self {
            Const::I64(x) => {
                w.write_all(&[0x42])?;
                leb128::write::signed(w, *x)?;
            }
        }
        Ok(())
    }
}

impl<W: Write> Writable<W> for Instr {
    fn write(&self, w: &mut W) -> Result<()> {
        match self {
            Instr::End => w.write_all(&[0x0B])?,
            Instr::Drop => w.write_all(&[0x1A])?,
            Instr::AddI64 => w.write_all(&[0x7C])?,
            Instr::SubI64 => w.write_all(&[0x7D])?,
            Instr::Const(val) => val.write(w)?,
            Instr::Call(idx) => {
                w.write_all(&[0x10])?;
                write_unsigned(w, *idx)?;
            }
            Instr::GetLocal(idx) => {
                w.write_all(&[0x20])?;
                write_unsigned(w, *idx)?;
            }
        }
        Ok(())
    }
}

impl<W: Write> Writable<W> for Code {
    fn write(&self, w: &mut W) -> Result<()> {
        let mut buf = Vec::new();
        vec(&mut buf, &self.locals)?;
        for instr in &self.body {
            instr.write(&mut buf)?;
        }
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
                write_unsigned(w, idx)?;
            }
        }
        Ok(())
    }
}

fn section_index(section: &Section) -> u8 {
    match section {
        Section::Type(_) => 1,
        Section::Func(_) => 3,
        Section::Export(_) => 7,
        Section::Start(_) => 8,
        Section::Code(_) => 10,
    }
}

impl<W: Write> Writable<W> for Section {
    fn write(&self, w: &mut W) -> Result<()> {
        w.write_all(&[section_index(self)])?;
        let mut buf = Vec::new();
        match self {
            Section::Type(types) => vec(&mut buf, &types.0)?,
            Section::Func(funcs) => vec(&mut buf, &funcs.0)?,
            Section::Export(exports) => vec(&mut buf, &exports.0)?,
            Section::Start(start) => write_unsigned(&mut buf, start.0)?,
            Section::Code(code) => vec(&mut buf, &code.0)?,
        }
        write_len(w, buf.len())?;
        w.write_all(&buf)?;
        Ok(())
    }
}

pub fn emit<W: Write>(w: &mut W, module: Module) -> Result<()> {
    w.write_all(HEADER)?;
    w.write_all(VERSION)?;
    Section::Type(module.types).write(w)?;
    Section::Func(module.funcs).write(w)?;
    Section::Export(module.exports).write(w)?;
    Section::Start(module.start).write(w)?;
    Section::Code(module.code).write(w)?;
    Ok(())
}
