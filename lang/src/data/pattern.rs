use super::VectorSet;

#[derive(Debug, Clone)]
pub struct Pattern {
    shape: VectorSet,
}
impl PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        true
    }
}
impl Eq for Pattern {}

impl Pattern {
    pub fn shape(&self) -> &VectorSet {
        &self.shape
    }

    pub fn never_match(shape: VectorSet) -> Self {
        Self { shape }
    }
}
