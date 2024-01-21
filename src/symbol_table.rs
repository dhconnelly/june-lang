use crate::types::*;
use std::collections::HashMap;

struct SymbolInfo {
    idx: usize,
    typ: Type,
}

pub struct CurrentFunction {
    locals: usize,
    frames: Vec<HashMap<String, SymbolInfo>>,
}

#[derive(Default)]
pub struct SymbolTable {
    // TODO: supporting forward references will require supporting empty values
    // in the globals table
    globals: HashMap<String, SymbolInfo>,
    functions: Vec<FnType>,
    current: Option<CurrentFunction>,
}

impl SymbolTable {
    pub fn enter_function(&mut self) {
        assert!(self.current.is_none());
        self.current = Some(CurrentFunction { locals: 0, frames: vec![] });
    }

    pub fn exit_function(&mut self) {
        assert!(self.current.as_ref().unwrap().frames.is_empty());
        self.current.take().unwrap();
    }

    pub fn num_locals(&self) -> Option<usize> {
        self.current.as_ref().map(|f| f.locals)
    }

    pub fn def_global(&mut self, name: impl ToString, typ: Type) {
        let idx = self.globals.len();
        let name = name.to_string();
        self.globals.insert(name, SymbolInfo { idx, typ });
    }

    fn get_global(&self, name: &str) -> Option<Resolution> {
        self.globals.get(name).map(|SymbolInfo { idx, typ }| Resolution {
            reference: Reference::Global { idx: *idx },
            typ: typ.clone(),
        })
    }

    pub fn def_fn(&mut self, params: Vec<Type>, ret: Option<Type>) -> FnType {
        let ret = ret.map(|typ| Box::new(typ));
        let typ = FnType { index: self.functions.len(), params, ret };
        self.functions.push(typ.clone());
        typ
    }

    pub fn push_frame(&mut self) {
        self.current.as_mut().unwrap().frames.push(HashMap::new());
    }

    pub fn pop_frame(&mut self) {
        self.current.as_mut().unwrap().frames.pop().unwrap();
    }

    pub fn def_local(&mut self, name: impl ToString, typ: Type) -> usize {
        let func = &mut self.current.as_mut().unwrap();
        let frame = func.frames.last_mut().unwrap();
        frame.insert(name.to_string(), SymbolInfo { idx: func.locals, typ });
        func.locals += 1;
        func.locals - 1
    }

    fn get_frame(&self, name: &str, depth: usize) -> Option<Resolution> {
        let func = self.current.as_ref().unwrap();
        let i = func.frames.len() - depth - 1;
        func.frames[i].get(name).map(|SymbolInfo { idx, typ }| Resolution {
            reference: Reference::Stack { local_idx: *idx },
            typ: typ.clone(),
        })
    }

    pub fn get(&self, name: &str) -> Option<Resolution> {
        let func = self.current.as_ref().unwrap();
        (0..func.frames.len())
            .find_map(|depth| self.get_frame(name, depth))
            .or_else(|| self.get_global(name))
    }
}
