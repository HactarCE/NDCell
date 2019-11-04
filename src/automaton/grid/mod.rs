use ndarray::{Dimension, NdIndex};
use std::default::Default;

mod chunked_grid;
mod fixed_grid;

/// A "trait alias" for a cell type that has a "default" value.
pub trait Cell: Default {}
impl<T: Default> Cell for T {}

pub trait Grid<C: Cell, D: Dimension, I: NdIndex<D>> {
    fn get_cell(&self, index: I) -> Option<&C>;
}
