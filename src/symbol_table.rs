use crate::types::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct SymbolInfo {
    idx: usize,
    typ: Type,
}

#[derive(Debug, Default, Clone)]
pub struct SymbolTable {
    // TODO: supporting forward references will require supporting empty values
    // in the globals table
    globals: HashMap<String, SymbolInfo>,
    frames: Vec<HashMap<String, SymbolInfo>>,
}

impl SymbolTable {
    pub fn def_global<S: Into<String>>(&mut self, name: S, typ: Type) {
        let idx = self.globals.len();
        let name = name.into();
        self.globals.insert(name, SymbolInfo { idx, typ });
    }

    fn get_global(&self, name: &str) -> Option<Resolution> {
        self.globals.get(name).map(|SymbolInfo { idx, typ }| Resolution {
            reference: Reference::Global { idx: *idx },
            typ: typ.clone(),
        })
    }

    pub fn push_frame(&mut self) {
        self.frames.push(HashMap::new());
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop().unwrap();
    }

    pub fn def_local<S: Into<String>>(&mut self, name: S, typ: Type) {
        let frame = self.frames.last_mut().unwrap();
        let idx = frame.len();
        frame.insert(name.into(), SymbolInfo { idx, typ });
    }

    fn get_frame(&self, name: &str, depth: usize) -> Option<Resolution> {
        let i = self.frames.len() - depth - 1;
        self.frames[i].get(name).map(|SymbolInfo { idx, typ }| Resolution {
            reference: Reference::Stack { frame_depth: depth, frame_idx: *idx },
            typ: typ.clone(),
        })
    }

    pub fn get(&self, name: &str) -> Option<Resolution> {
        (0..self.frames.len())
            .find_map(|depth| self.get_frame(name, depth))
            .or_else(|| self.get_global(name))
    }
}
