use std::fmt;

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
