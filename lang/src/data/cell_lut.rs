use std::ops::Index;

use super::LangCellState;

/// Cell state translation table.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CellLut(Vec<LangCellState>);
impl CellLut {
    pub fn default(cell_state_count: usize) -> Self {
        Self((0..cell_state_count).map(|x| x as LangCellState).collect())
    }
}
impl Index<LangCellState> for CellLut {
    type Output = LangCellState;

    fn index(&self, index: LangCellState) -> &Self::Output {
        &self.0[index as usize]
    }
}
