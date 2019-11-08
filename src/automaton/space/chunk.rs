use ndarray::prelude::*;
use std::ops::{Index, IndexMut};

use super::*;

#[derive(Clone)]
pub struct Chunk<T: CellType, C: Coords> {
    pub array: ArcArray<T, C::D>,
}

impl<T: CellType, C: Coords> Default for Chunk<T, C> {
    fn default() -> Self {
        Self {
            array: ArcArray::<T, C::D>::default(LocalCoords::<C>::CHUNK_SHAPE.ndindex()),
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
