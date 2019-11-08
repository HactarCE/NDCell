//! The infinite grid used to simulate automata.

use std::collections::HashMap;
use std::ops::{Index, IndexMut};

use super::*;

/// An infinite Grid, stored in chunks of ~4k cells.
#[derive(Clone)]
pub struct Grid<T: CellType, C: Coords> {
    chunks: HashMap<ChunkCoords<C>, Chunk<T, C>>,
    default_chunk: Chunk<T, C>,
    default_cell: T,
}

/// A generic Grid consisting of a sparse ndarray of hypercubes, stored
/// internally using a HashMap.
impl<T: CellType, C: Coords> Grid<T, C> {
    const NDIM: usize = C::NDIM;

    /// Constructs an empty Grid with the default chunk size.
    pub fn new() -> Self {
        Self {
            chunks: HashMap::new(),
            default_chunk: Chunk::<T, C>::default(),
            default_cell: T::default(),
        }
    }

    pub fn get_chunks(&self) -> &HashMap<ChunkCoords<C>, Chunk<T, C>> {
        &self.chunks
    }

    /// Returns whether the entire grid is empty.
    ///
    /// If there is a single non-default cell, this method returns false;
    /// otherwise it returns true.
    pub fn is_empty(&self) -> bool {
        self.chunks
            .keys()
            .all(|&chunk_coords| self.is_chunk_empty(chunk_coords))
    }

    /// Returns the coordinates of the origin (0 on each axis).
    pub fn origin() -> CellCoords<C> {
        C::origin().into()
    }

    /// Returns whether there is a chunk at the given chunk coordinates.
    fn has_chunk(&self, chunk_index: ChunkCoords<C>) -> bool {
        self.chunks.contains_key(&chunk_index)
    }

    /// Returns whether the chunk at the given chunk coordinates is empty.
    ///
    /// Returns true if the chunk does not exist.
    fn is_chunk_empty(&self, chunk_index: ChunkCoords<C>) -> bool {
        match self.get_chunk(chunk_index) {
            None => true,
            Some(chunk) => chunk.array.iter().all(|&cell| cell == T::default()),
        }
    }

    /// Returns a reference to the chunk with the given chunk coordinates.
    ///
    /// If the chunk does not exist.
    fn get_chunk(&self, chunk_index: ChunkCoords<C>) -> Option<&Chunk<T, C>> {
        self.chunks.get(&chunk_index)
    }

    /// Returns a mutable reference to the chunk with the given chunk
    /// coordinates.
    ///
    /// If the chunk does not exist, return None.
    fn get_chunk_mut(&mut self, chunk_index: ChunkCoords<C>) -> Option<&mut Chunk<T, C>> {
        self.chunks.get_mut(&chunk_index)
    }

    /// Returns a reference to the chunk with the given chunk coordinates, or an
    /// empty chunk if it does not exist.
    ///
    /// If the chunk does not exist, return a reference to a blank chunk.
    fn infer_chunk(&self, chunk_index: ChunkCoords<C>) -> &Chunk<T, C> {
        self.get_chunk(chunk_index).unwrap_or(&self.default_chunk)
    }

    /// Removes the chunk at the given chunk coordinates and return it.
    ///
    /// If the chunk does not exist, this method does nothing and returns None.
    fn remove_chunk(&mut self, chunk_index: ChunkCoords<C>) -> Option<Chunk<T, C>> {
        self.chunks.remove(&chunk_index)
    }

    /// Removes the chunk at the given coordinates if it exists and is empty.
    /// Returns true if the chunk was removed and false otherwise.
    fn remove_chunk_if_empty(&mut self, chunk_index: ChunkCoords<C>) -> bool {
        if self.has_chunk(chunk_index) && self.is_chunk_empty(chunk_index) {
            self.remove_chunk(chunk_index);
            true
        } else {
            false
        }
    }
}

impl<T: CellType, C: Coords> Index<ChunkCoords<C>> for Grid<T, C> {
    type Output = Chunk<T, C>;
    fn index(&self, chunk_coords: ChunkCoords<C>) -> &Chunk<T, C> {
        self.chunks
            .get(&chunk_coords)
            .unwrap_or(&self.default_chunk)
    }
}
impl<T: CellType, C: Coords> IndexMut<ChunkCoords<C>> for Grid<T, C> {
    fn index_mut(&mut self, chunk_coords: ChunkCoords<C>) -> &mut Chunk<T, C> {
        if !self.has_chunk(chunk_coords) {
            self.chunks.insert(chunk_coords, self.default_chunk.clone());
        }
        self.chunks
            .get_mut(&chunk_coords)
            .expect("Just created chunk, but not present")
    }
}

impl<T: CellType, C: Coords> Index<CellCoords<C>> for Grid<T, C> {
    type Output = T;
    fn index(&self, cell_coords: CellCoords<C>) -> &T {
        &self[cell_coords.chunk()][cell_coords.local()]
    }
}
impl<T: CellType, C: Coords> IndexMut<CellCoords<C>> for Grid<T, C> {
    fn index_mut(&mut self, cell_coords: CellCoords<C>) -> &mut T {
        &mut self[cell_coords.chunk()][cell_coords.local()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::automaton::space::coords_container::{cell_coords_strategy, ChunkCoords3D};
    use proptest::prelude::*;

    proptest! {
        /// Tests setting and getting a single cell on a grid.
        #[test]
        fn test_grid_set_get(
            pos in cell_coords_strategy(-50..=50isize),
            cell_value: u8
        ) {
            let mut grid = Grid::<u8, [isize; 3]>::new();
            grid[pos] = cell_value;
            assert_eq!(cell_value, grid[pos]);
        }

        /// Tests setting and getting two cells on a grid, where the second may
        /// overwrite the first.
        #[test]
        fn test_grid_multi_set(
            pos1 in cell_coords_strategy(-50..=50isize),
            offset in cell_coords_strategy(-2..=2isize),
            cell_value1: u8,
            cell_value2: u8,
        ) {
            let mut grid = Grid::<u8, [isize; 3]>::new();
            let pos2 = pos1 + offset;
            grid[pos1] = cell_value1;
            grid[pos2] = cell_value2;
            assert_eq!(if offset.is_zero() {cell_value2} else {cell_value1}, grid[pos1], "First cell is wrong");
            assert_eq!(cell_value2, grid[pos2], "Second cell is wrong");
        }

        /// Tests removing a grid chunk if it is empty.
        #[test]
        fn test_grid_remove_chunk_if_empty(
            pos in cell_coords_strategy(-50..=50isize),
            cell_value: u8
        ) {
            let mut grid = Grid::<u8, [isize; 3]>::new();
            grid[pos] = cell_value;
            let chunk_coords: ChunkCoords3D = pos.into();
            let value_is_zero = cell_value == 0;
            let value_is_nonzero = cell_value != 0;
            assert!(grid.has_chunk(chunk_coords));
            assert_eq!(value_is_zero, grid.is_chunk_empty(chunk_coords));
            assert_eq!(value_is_zero, grid.is_empty());
            grid.remove_chunk_if_empty(chunk_coords);
            assert_eq!(value_is_nonzero, grid.has_chunk(chunk_coords));
            assert_eq!(value_is_zero, grid.is_chunk_empty(chunk_coords));
            assert_eq!(value_is_zero, grid.is_empty());
            grid[pos] = 0;
            grid.remove_chunk_if_empty(chunk_coords);
            assert!(! grid.has_chunk(chunk_coords));
        }
    }
}
