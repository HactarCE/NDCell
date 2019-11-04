use ndarray::{Array, Dimension, NdIndex, Shape};

use super::{Cell, Grid};

struct ArrayGrid<C: Cell, D: Dimension> {
    array: Array<C, D>,
}

impl<C: Cell, D: Dimension> ArrayGrid<C, D> {
    fn new(shape: Shape<D>) -> Self {
        Self {
            array: Array::default(shape),
        }
    }
}

impl<C: Cell, D: Dimension, I: NdIndex<D>> Grid<C, D, I> for ArrayGrid<C, D> {
    fn get_cell(&self, index: I) -> Option<&C> {
        self.array.get(index)
    }
}
