use std::cmp::Eq;
use std::default::Default;
use std::hash::Hash;

mod chunked_grid;

/// A "trait alias" for ndarray::Dimension + std::cmp::Eq + std::hash::Hash so
/// that it can be used in HashMaps.
pub trait Dimension: ndarray::Dimension + Eq + Hash {}
impl<T: ndarray::Dimension + Eq + Hash> Dimension for T {}

/// A "trait alias" for a cell type that has a "default" value.
pub trait Cell: Copy + Default + Eq {}
impl<T: Copy + Default + Eq> Cell for T {}

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
    fn get_cell(&self, index: D) -> C;

    /// Sets the cell at the given position and returns the previous value.
    fn set_cell(&mut self, index: D, cell_value: C) -> C;

    // fn get_chunk_napkin(&mut self, index: D, cell_value: C);
}
