use std::clone::Clone;
use std::default::Default;

mod chunked_grid;
mod fixed_grid;

/// A "trait alias" for ndarray::Dimension + std::cmp::Eq + std::hash::Hash so
/// that it can be used in HashMaps.
pub trait Dimension: ndarray::Dimension + std::cmp::Eq + std::hash::Hash {}
impl<T: ndarray::Dimension + std::cmp::Eq + std::hash::Hash> Dimension for T {}

/// A "trait alias" for a cell type that has a "default" value.
pub trait Cell: Clone + Default {}
impl<T: Clone + Default> Cell for T {}

pub trait Grid<C: Cell, D: Dimension> {
    fn ndim() -> usize {
        D::NDIM.unwrap()
    }
    fn origin() -> D {
        D::zeros(Self::ndim())
    }
    fn get_cell(&self, index: D) -> Option<&C>;
}
