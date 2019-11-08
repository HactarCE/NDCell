use ndarray::prelude::*;
use std::ops::{Index, IndexMut};

use super::*;

/// A hypercubic chunk of a Grid holding ~4k cells.
#[derive(Clone)]
pub struct Chunk<T: CellType, C: Coords> {
    /// The ndarray storing the cell contents of this chunk.
    pub array: ArcArray<T, C::D>,
}

impl<T: CellType, C: Coords> Default for Chunk<T, C> {
    fn default() -> Self {
        Self {
            array: ArcArray::<T, C::D>::default(LocalCoords::<C>::get_chunk_shape().ndindex()),
        }
    }
}

impl<T: CellType, C: Coords> Index<LocalCoords<C>> for Chunk<T, C> {
    type Output = T;
    fn index(&self, local_coords: LocalCoords<C>) -> &T {
        &self.array[local_coords.ndindex()]
    }
}

impl<T: CellType, C: Coords> IndexMut<LocalCoords<C>> for Chunk<T, C> {
    fn index_mut(&mut self, local_coords: LocalCoords<C>) -> &mut T {
        &mut self.array[local_coords.ndindex()]
    }
}
