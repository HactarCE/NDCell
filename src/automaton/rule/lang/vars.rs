use std::collections::HashMap;
use std::fmt;
use std::ops::Index;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Int,
    CellState,
}
impl Default for Type {
    fn default() -> Self {
        Self::Void
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Void => "void",
                Self::Int => "integer",
                Self::CellState => "cell state",
            }
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Value {
    Null,
    Void,
    Int(i64),
    CellState(i64),
}

#[derive(Debug, Clone, Default)]
pub struct VarMapping {
    vec: Vec<(String, Type)>,
    ids: HashMap<String, usize>,
}
impl Index<&str> for VarMapping {
    type Output = usize;
    fn index(&self, name: &str) -> &usize {
        self.ids
            .get(name)
            .expect("Attempt to get ID of unknown variable")
    }
}
impl VarMapping {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn len(&self) -> usize {
        self.vec.len()
    }
    pub fn get_name(&self, id: usize) -> &str {
        match self.vec.get(id) {
            Some((name, _ty)) => name,
            None => panic!("Attempt to get name of unknown variable"),
        }
    }
    pub fn get_type(&self, id: usize) -> Type {
        match self.vec.get(id) {
            Some((_name, ty)) => *ty,
            None => panic!("Attempt to get type of unknown variable"),
        }
    }
    pub fn is_registered(&self, s: &str) -> bool {
        self.ids.contains_key(s)
    }
    pub fn register(&mut self, s: &str, ty: Type) {
        if !self.is_registered(s) {
            self.ids.insert(s.to_owned(), self.len());
            self.vec.push((s.to_owned(), ty));
        }
    }
}
