use ndarray::{Array, Dimension, NdIndex};

use super::Grid;

struct ArrayGrid<C, D: Dimension> {
    array: Array<C, D>,
}

impl<C, D: Dimension, I: NdIndex<D>> Grid<C, D, I> for ArrayGrid<C, D> {
    fn get_cell(&self, index: I) -> Option<&C> {
        self.array.get(index)
    }
}
