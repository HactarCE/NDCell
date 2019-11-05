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
    /// Returns the number of dimensions in this grid.
    fn ndim() -> usize {
        D::NDIM.unwrap()
    }

    /// Returns the coordinates of the origin (0 on each axis).
    fn origin() -> D {
        D::zeros(Self::ndim())
    }

    /// Returns the cell at the given position.
    fn get_cell(&self, index: D) -> Option<&C>;
}
