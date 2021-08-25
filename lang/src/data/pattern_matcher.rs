use std::hash::{Hash, Hasher};
use std::sync::Arc;

use super::VectorSet;

#[derive(Debug, Clone)]
pub struct PatternMatcher {
    len: usize,
}
impl PartialEq for PatternMatcher {
    fn eq(&self, other: &Self) -> bool {
        true
    }
}
impl Eq for PatternMatcher {}
impl Hash for PatternMatcher {
    fn hash<H: Hasher>(&self, _: &mut H) {
        todo!()
    }
}
impl PatternMatcher {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn never_match(len: usize) -> Self {
        Self { len }
    }
}
