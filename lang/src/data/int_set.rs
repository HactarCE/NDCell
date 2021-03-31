use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Debug, Default, Clone)]
pub struct IntegerSet {}
impl PartialEq for IntegerSet {
    fn eq(&self, other: &Self) -> bool {
        true
    }
}
impl Eq for IntegerSet {}
impl fmt::Display for IntegerSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TODO")
    }
}
impl Hash for IntegerSet {
    fn hash<H: Hasher>(&self, state: &mut H) {
        todo!()
    }
}
impl IntegerSet {
    pub fn empty() -> Self {
        Self::default()
    }
}
