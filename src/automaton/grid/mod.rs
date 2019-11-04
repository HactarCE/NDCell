use ndarray::{Dimension, NdIndex};

mod chunked_grid;
mod fixed_grid;

pub trait Grid<C, D: Dimension, I: NdIndex<D>> {
    fn get_cell(&self, index: I) -> Option<&C>;
}
