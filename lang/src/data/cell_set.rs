use super::LangCell;

/// 256-bit array, where each bit corresponds to a cell state.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct CellSet([u64; 4]);

impl CellSet {
    pub fn empty() -> Self {
        Self::default()
    }
    pub fn single_cell(cell_state: LangCell) -> Self {
        todo!()
    }
    pub fn state_count(&self) -> usize {
        self.0.len()
    }
}
