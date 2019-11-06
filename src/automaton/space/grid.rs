//! The infinite grid used to simulate automata.

use ndarray::ArcArray;
use ndarray::Dimension;
use std::collections::HashMap;

use super::{Cell, CellVector, ChunkVector, LocalVector, Vector};

/// An inifnite Grid, stored in chunks of ~4k cells.
#[derive(Clone)]
pub struct Grid<C: Cell, V: Vector> {
    chunks: HashMap<ChunkVector<V>, ArcArray<C, V::D>>,
    default_chunk: ArcArray<C, V::D>,
}

/// A generic Grid consisting of a sparse ndarray of hypercubes, stored
/// internally using a HashMap.
impl<C: Cell, V: Vector> Grid<C, V> {
    const NDIM: usize = V::NDIM;

    /// Constructs an empty Grid with the default chunk size.
    pub fn new() -> Self {
        // I don't know how else to generate an array shape from a Dimension
        // type, but this works: Generate a zero vector like [0, 0].
        let mut chunk_shape = V::D::zeros(Self::NDIM);
        // Turn it into a mutable 1D array.
        let mut chunk_shape_array = chunk_shape.as_array_view_mut();
        // Increment each member of the array (i.e. the size along each axis) by the chunk size.
        chunk_shape_array += V::CHUNK_SIZE;
        Self {
            chunks: HashMap::new(),
            // chunk_size: chunk_size,
            default_chunk: ArcArray::default(chunk_shape),
        }
    }

    /// Returns the coordinates of the origin (0 on each axis).
    fn origin() -> CellVector<V> {
        V::origin().into()
    }

    /// Returns the cell at the given position.
    fn get_cell(&self, cell_vector: CellVector<V>) -> C {
        if let Some(chunk) = self.get_chunk(&(&cell_vector).into()) {
            let local_index: LocalVector<V> = (&cell_vector).into();
            chunk[local_index.ndindex()]
        } else {
            C::default()
        }
    }

    /// Sets the cell at the given position and returns the previous value.
    fn set_cell(&mut self, cell_vector: &CellVector<V>, cell_value: C) -> C {
        let local_index: LocalVector<V> = cell_vector.into();
        let chunk = self.infer_chunk_mut(&cell_vector.into());
        std::mem::replace(&mut chunk[local_index.ndindex()], cell_value)
    }

    /// Returns whether there is a chunk at the given chunk coordinates.
    fn has_chunk(&self, chunk_index: &ChunkVector<V>) -> bool {
        self.chunks.contains_key(chunk_index)
    }

    /// Returns whether the chunk at the given chunk coordinates is empty.
    ///
    /// Returns true if the chunk does not exist.
    fn is_chunk_empty(&self, chunk_index: &ChunkVector<V>) -> bool {
        match self.get_chunk(chunk_index) {
            None => true,
            Some(chunk) => chunk.iter().any(|&cell| cell == C::default()),
        }
    }

    /// Returns a reference to the chunk with the given chunk coordinates.
    ///
    /// If the chunk does not exist.
    fn get_chunk(&self, chunk_index: &ChunkVector<V>) -> Option<&ArcArray<C, V::D>> {
        self.chunks.get(chunk_index)
    }

    /// Returns a mutable reference to the chunk with the given chunk
    /// coordinates.
    ///
    /// If the chunk does not exist, return None.
    fn get_chunk_mut(&mut self, chunk_index: &ChunkVector<V>) -> Option<&mut ArcArray<C, V::D>> {
        self.chunks.get_mut(chunk_index)
    }

    /// Returns a reference to the chunk with the given chunk coordinates, or an
    /// empty chunk if it does not exist.
    ///
    /// If the chunk does not exist, return a reference to a blank chunk.
    fn infer_chunk(&self, chunk_index: &ChunkVector<V>) -> &ArcArray<C, V::D> {
        self.get_chunk(chunk_index).unwrap_or(&self.default_chunk)
    }

    /// Returns a mutable reference to the chunk with the given chunk
    /// coordinates, creating it if it does not exist.
    ///
    /// If the chunk does not exist, create a new chunk at those coordinates and
    /// return a mutable reference to it.
    fn infer_chunk_mut(&mut self, chunk_index: &ChunkVector<V>) -> &mut ArcArray<C, V::D> {
        self.make_chunk(chunk_index);
        self.get_chunk_mut(chunk_index)
            .expect("Just created chunk, but not present")
    }

    /// Creates a chunk at the given chunk coordinates if there is none.
    ///
    /// If there is already a chunk there, this method does nothing.
    fn make_chunk(&mut self, chunk_index: &ChunkVector<V>) {
        if !self.has_chunk(chunk_index) {
            self.chunks
                .insert(chunk_index.clone(), self.default_chunk.clone());
        }
    }

    /// Removes the chunk at the given chunk coordinates and return it.
    ///
    /// If the chunk does not exist, this method does nothing and returns None.
    fn remove_chunk(&mut self, chunk_index: &ChunkVector<V>) -> Option<ArcArray<C, V::D>> {
        self.chunks.remove(chunk_index)
    }

    /// Removes the chunk at the given coordinates if it exists and is empty.
    /// Returns true if the chunk was removed and false otherwise.
    fn remove_chunk_if_empty(&mut self, chunk_index: &ChunkVector<V>) -> bool {
        if self.has_chunk(chunk_index) && self.is_chunk_empty(chunk_index) {
            self.remove_chunk(chunk_index);
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn test_get_and_set_cell(position in [-50..=50isize, -50..=50isize, -50..=50isize], cell_value: u8) {
            let mut grid = Grid::<u8, [isize; 3]>::new();
            grid.set_cell(&position.into(), cell_value);
            assert_eq!(cell_value, grid.get_cell(position.into()));
        }
    }
}
