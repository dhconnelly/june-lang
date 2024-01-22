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

fn write_len<W: Write>(w: &mut W, len: usize) -> Result<()> {
    let len: u32 = len.try_into().or(Err(Error::LengthError(len)))?;
    len.write(w)?;
    Ok(())
}

fn vec<W: Write, T: Writable<W>>(w: &mut W, v: &[T]) -> Result<()> {
    write_len(w, v.len())?;
    for item in v {
        item.write(w)?;
    }
    Ok(())
}

impl<W: Write> Writable<W> for u32 {
    fn write(&self, w: &mut W) -> Result<()> {
        leb128::write::unsigned(w, *self as u64)?;
        Ok(())
    }
}

impl<W: Write> Writable<W> for i64 {
    fn write(&self, w: &mut W) -> Result<()> {
        leb128::write::signed(w, *self)?;
        Ok(())
    }
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
        self.typeidx.write(w)?;
        Ok(())
    }
}

impl<W: Write> Writable<W> for Const {
    fn write(&self, w: &mut W) -> Result<()> {
        match self {
            Const::I64(x) => {
                w.write_all(&[0x42])?;
                x.write(w)?;
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
                idx.write(w)?;
            }
            Instr::SetLocal(idx) => {
                w.write_all(&[0x21])?;
                idx.write(w)?;
            }
            Instr::GetLocal(idx) => {
                w.write_all(&[0x20])?;
                idx.write(w)?;
            }
        }
        Ok(())
    }
}

impl<W: Write> Writable<W> for Code {
    fn write(&self, w: &mut W) -> Result<()> {
        let mut buf = Vec::new();
        // the locals are actually vec<vec<local>> in order to optimize
        // for having many variables of the same type :eyeroll:
        // https://webassembly.github.io/spec/core/binary/modules.html#code-section
        // here i don't care about this, we'll just make it n vecs of length 1
        write_len(&mut buf, self.locals.len())?;
        for local in &self.locals {
            write_len(&mut buf, 1)?;
            local.write(&mut buf)?;
        }
        for instr in &self.body {
            instr.write(&mut buf)?;
        }
        write_len(w, buf.len())?;
        w.write_all(&buf)?;
        Ok(())
    }
}

impl<W: Write> Writable<W> for String {
    fn write(&self, w: &mut W) -> Result<()> {
        write_len(w, self.len())?;
        w.write_all(self.as_bytes())?;
        Ok(())
    }
}

impl<W: Write> Writable<W> for Export {
    fn write(&self, w: &mut W) -> Result<()> {
        self.name.write(w)?;
        match self.desc {
            ExportDesc::Func(idx) => {
                w.write_all(&[0x00])?;
                idx.write(w)?;
            }
        }
        Ok(())
    }
}

impl<W: Write> Writable<W> for Import {
    fn write(&self, w: &mut W) -> Result<()> {
        self.module.write(w)?;
        self.name.write(w)?;
        match self.import {
            ImportedValue::Func(idx) => {
                w.write_all(&[0x00])?;
                idx.write(w)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Section {
    Type(TypeSection),
    Import(ImportSection),
    Func(FuncSection),
    Export(ExportSection),
    Code(CodeSection),
    Start(StartSection),
}

fn section_index(section: &Section) -> u8 {
    match section {
        Section::Type(_) => 1,
        Section::Import(_) => 2,
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
            Section::Import(imports) => vec(&mut buf, &imports.0)?,
            Section::Func(funcs) => vec(&mut buf, &funcs.0)?,
            Section::Export(exports) => vec(&mut buf, &exports.0)?,
            Section::Start(start) => start.0.write(&mut buf)?,
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
    Section::Import(module.imports).write(w)?;
    Section::Func(module.funcs).write(w)?;
    Section::Export(module.exports).write(w)?;
    Section::Start(module.start).write(w)?;
    Section::Code(module.code).write(w)?;
    Ok(())
}
