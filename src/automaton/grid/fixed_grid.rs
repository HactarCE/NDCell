use ndarray::{Array, Shape};

use super::{Cell, Dimension, Grid};

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

impl<C: Cell, D: Dimension> Grid<C, D> for ArrayGrid<C, D> {
    fn get_cell(&self, index: D) -> Option<&C> {
        self.array.get(index)
    }
}
