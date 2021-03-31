use std::sync::Arc;

use super::VectorSet;

/// Masked N-dimensional array of cells.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Array {
    shape: Arc<VectorSet>,
    cells: (),
}
impl Array {
    pub fn zeros(shape: Arc<VectorSet>) -> Self {
        Self { shape, cells: () }
    }

    pub fn shape(&self) -> Arc<VectorSet> {
        Arc::clone(&self.shape)
    }

    pub fn any_nonzero(&self) -> bool {
        false
    }
}
