use ndarray::ArcArray;
use std::fmt;
use std::fmt::Write;
use std::ops::{Index, IndexMut};

use super::*;

/// A hypercubic chunk of a Grid holding ~4k cells.
#[derive(Clone)]
pub struct Chunk<T: CellType, D: Dim> {
    /// The ndarray storing the cell contents of this chunk.
    pub array: ArcArray<T, D::NdarrayDim>,
}

impl fmt::Display for Chunk<bool, Coords2D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for ((_, x), &cell_value) in self.array.indexed_iter() {
            f.write_char(if cell_value { '#' } else { '.' })?;
            if x == Coords2D::CHUNK_SIZE - 1 {
                f.write_char('\n')?;
            } else {
                f.write_char(' ')?;
            }
        }
        Ok(())
    }
}

impl<T: CellType, D: Dim> Chunk<T, D> {
    /// Returns whether every cell in the chunk is in the default state.
    pub fn is_empty(&self) -> bool {
        let default_cell = T::default();
        self.array.iter().all(|&cell| cell == default_cell)
    }
}

impl<T: CellType, D: Dim> Default for Chunk<T, D> {
    fn default() -> Self {
        Self {
            array: ArcArray::<T, D::NdarrayDim>::default(
                LocalCoords::<D>::get_chunk_shape().ndindex(),
            ),
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
