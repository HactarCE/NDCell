use std::fmt;

#[derive(Debug, Clone)]
pub struct VectorSet {
    vec_len: usize,
}
impl PartialEq for VectorSet {
    fn eq(&self, other: &Self) -> bool {
        true
    }
}
impl Eq for VectorSet {}
impl fmt::Display for VectorSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl VectorSet {
    pub fn empty(vec_len: usize) -> Self {
        Self { vec_len }
    }

    pub fn vec_len(&self) -> usize {
        self.vec_len
    }
}
