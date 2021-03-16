#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CellSet(Vec<bool>);

impl CellSet {
    pub fn empty(state_count: usize) -> Self {
        Self(vec![false; state_count])
    }
    pub fn state_count(&self) -> usize {
        self.0.len()
    }
}
