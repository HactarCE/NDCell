use super::VectorSet;

/// Masked N-dimensional array of cells.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Array {
    shape: VectorSet,
    cells: (),
}

impl Array {
    pub fn zeros(shape: VectorSet) -> Self {
        Self { shape, cells: () }
    }

    pub fn shape(&self) -> &VectorSet {
        &self.shape
    }

    pub fn any_nonzero(&self) -> bool {
        false
    }
}
