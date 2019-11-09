use ndarray::ArcArray;
use std::ops::{Index, IndexMut};

use super::*;

/// A hypercubic chunk of a Grid holding ~4k cells.
#[derive(Clone)]
pub struct Chunk<T: CellType, D: Dim> {
    /// The ndarray storing the cell contents of this chunk.
    pub array: ArcArray<T, D::D>,
}

impl<T: CellType, D: Dim> Default for Chunk<T, D> {
    fn default() -> Self {
        Self {
            array: ArcArray::<T, D::D>::default(LocalCoords::<D>::get_chunk_shape().ndindex()),
        }
    }
}

impl<T: CellType, D: Dim> Index<LocalCoords<D>> for Chunk<T, D> {
    type Output = T;
    fn index(&self, local_coords: LocalCoords<D>) -> &T {
        &self.array[local_coords.ndindex()]
    }
}

impl<T: CellType, D: Dim> IndexMut<LocalCoords<D>> for Chunk<T, D> {
    fn index_mut(&mut self, local_coords: LocalCoords<D>) -> &mut T {
        &mut self.array[local_coords.ndindex()]
    }
}
