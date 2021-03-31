use std::ops::Index;

use super::LangCell;

/// Cell state translation table.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CellLut(Vec<LangCell>);
impl CellLut {
    pub fn default(cell_state_count: usize) -> Self {
        Self((0..cell_state_count).map(|x| x as LangCell).collect())
    }
}
impl Index<LangCell> for CellLut {
    type Output = LangCell;

    fn index(&self, index: LangCell) -> &Self::Output {
        &self.0[index as usize]
    }
}
