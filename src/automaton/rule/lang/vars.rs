use std::collections::HashMap;
use std::ops::Index;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
}

#[derive(Debug, Clone, Default)]
pub struct VarMapping {
    vec: Vec<String>,
    ids: HashMap<String, usize>,
}
impl Index<usize> for VarMapping {
    type Output = str;
    fn index(&self, id: usize) -> &str {
        &self.vec[id]
    }
}
impl Index<&str> for VarMapping {
    type Output = usize;
    fn index(&self, name: &str) -> &usize {
        &self.ids[name]
    }
}
impl VarMapping {
    pub fn register(&mut self, s: &str) {
        if !self.ids.contains_key(s) {
            self.ids.insert(s.to_owned(), self.vec.len());
            self.vec.push(s.to_owned());
        }
    }
}
