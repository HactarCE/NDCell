use std::hash::{Hash, Hasher};
use std::sync::Arc;

use super::VectorSet;

#[derive(Debug, Clone)]
pub struct Pattern {
    len: usize,
}
impl PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        true
    }
}
impl Eq for Pattern {}
impl Hash for Pattern {
    fn hash<H: Hasher>(&self, _: &mut H) {
        todo!()
    }
}
impl Pattern {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn never_match(len: usize) -> Self {
        Self { len }
    }
}
