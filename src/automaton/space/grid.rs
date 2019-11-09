//! The infinite grid used to simulate automata.

use std::collections::HashMap;
use std::ops::{Index, IndexMut};

use super::*;

/// An infinite Grid, stored in chunks of ~4k cells.
#[derive(Clone)]
pub struct Grid<T: CellType, D: Dim> {
    chunks: HashMap<ChunkCoords<D>, Chunk<T, D>>,
    default_chunk: Chunk<T, D>,
    default_cell: T,
}

/// A generic Grid consisting of a sparse ndarray of hypercubes, stored
/// internally using a HashMap.
impl<T: CellType, D: Dim> Grid<T, D> {
    const NDIM: usize = D::NDIM;

    /// Constructs an empty Grid with the default chunk size.
    pub fn new() -> Self {
        Self {
            chunks: HashMap::new(),
            default_chunk: Chunk::<T, D>::default(),
            default_cell: T::default(),
        }
    }

    /// Returns the hashmap mapping chunk coordinates to chunks.
    pub fn get_chunks(&self) -> &HashMap<ChunkCoords<D>, Chunk<T, D>> {
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
    pub fn origin() -> CellCoords<D> {
        D::origin().into()
    }

    /// Returns whether there is a chunk at the given chunk coordinates.
    pub fn has_chunk(&self, chunk_index: ChunkCoords<D>) -> bool {
        self.chunks.contains_key(&chunk_index)
    }

    /// Returns whether the chunk at the given chunk coordinates is empty.
    ///
    /// Returns true if the chunk does not exist.
    pub fn is_chunk_empty(&self, chunk_index: ChunkCoords<D>) -> bool {
        match self.get_chunk(chunk_index) {
            None => true,
            Some(chunk) => chunk.is_empty(),
        }
    }

    /// Returns a reference to the chunk with the given chunk coordinates.
    ///
    /// If the chunk does not exist.
    pub fn get_chunk(&self, chunk_index: ChunkCoords<D>) -> Option<&Chunk<T, D>> {
        self.chunks.get(&chunk_index)
    }

    /// Returns a mutable reference to the chunk with the given chunk
    /// coordinates.
    ///
    /// If the chunk does not exist, return None.
    pub fn get_chunk_mut(&mut self, chunk_index: ChunkCoords<D>) -> Option<&mut Chunk<T, D>> {
        self.chunks.get_mut(&chunk_index)
    }

    /// Removes the chunk at the given chunk coordinates and return it.
    ///
    /// If the chunk does not exist, this method does nothing and returns None.
    pub fn remove_chunk(&mut self, chunk_index: ChunkCoords<D>) -> Option<Chunk<T, D>> {
        self.chunks.remove(&chunk_index)
    }

    /// Removes the chunk at the given coordinates if it exists and is empty.
    /// Returns true if the chunk was removed and false otherwise.
    pub fn remove_chunk_if_empty(&mut self, chunk_index: ChunkCoords<D>) -> bool {
        if self.has_chunk(chunk_index) && self.is_chunk_empty(chunk_index) {
            self.remove_chunk(chunk_index);
            true
        } else {
            false
        }
    }
}

impl<T: CellType, D: Dim> Index<ChunkCoords<D>> for Grid<T, D> {
    type Output = Chunk<T, D>;
    fn index(&self, chunk_coords: ChunkCoords<D>) -> &Chunk<T, D> {
        self.chunks
            .get(&chunk_coords)
            .unwrap_or(&self.default_chunk)
    }
}
impl<T: CellType, D: Dim> IndexMut<ChunkCoords<D>> for Grid<T, D> {
    fn index_mut(&mut self, chunk_coords: ChunkCoords<D>) -> &mut Chunk<T, D> {
        if !self.has_chunk(chunk_coords) {
            self.chunks.insert(chunk_coords, self.default_chunk.clone());
        }
        self.chunks
            .get_mut(&chunk_coords)
            .expect("Just created chunk, but not present")
    }
}

impl<T: CellType, D: Dim> Index<CellCoords<D>> for Grid<T, D> {
    type Output = T;
    fn index(&self, cell_coords: CellCoords<D>) -> &T {
        &self[cell_coords.chunk()][cell_coords.local()]
    }
}
impl<T: CellType, D: Dim> IndexMut<CellCoords<D>> for Grid<T, D> {
    fn index_mut(&mut self, cell_coords: CellCoords<D>) -> &mut T {
        &mut self[cell_coords.chunk()][cell_coords.local()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        /// Tests setting and getting a single cell on a grid.
        #[test]
        fn test_grid_set_get(
            pos in cell_coords_strategy(-50..=50isize),
            cell_value: u8
        ) {
            let mut grid = Grid::<u8, Coords3D>::new();
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
            let mut grid = Grid::<u8, Coords3D>::new();
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
            let mut grid = Grid::<u8, Coords3D>::new();
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
