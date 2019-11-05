use ndarray::ArcArray;
use std::collections::HashMap;

use super::{Cell, Dimension, Grid};

struct ChunkedGrid<C: Cell, D: Dimension> {
    chunks: HashMap<D, ArcArray<C, D>>,
    chunk_size: usize,
    default_chunk: ArcArray<C, D>,
}

/// Computes the "recommended" chunk size for a given dimension count.
///
/// This is based on trying to keep the chunk size big, but still reasonable
/// (such that a full chunk is at most 4k) and always a power of 2.
///
/// Using a flat chunk size would either result in stupidly small chunks at
/// lower dimensions (16 is silly for 1D CA that often densely span thousands of
/// cells) or stupidly huge chunks at higher dimensions (even a 32^4 chunk in 4D would be 1 MiB, which is rather large to be copying around constantly)
///
/// Here are the values that this function outputs:
///
/// - 1D => 4096 = 4k
/// - 2D => 64^2 = 4k
/// - 3D => 16^3 = 4k
/// - 4D =>  8^4 = 4k
/// - 5D =>  4^5 = 1k (8^5 would be 32k)
/// - 6D =>  4^6 = 4k
fn get_recommended_chunk_size(ndim: usize) -> usize {
    let max_power = 12; // 2^12 = 4096
    1 << max_power / ndim
}

/// A generic Grid consisting of a sparse ndarray of hypercubes, stored
/// internally using a HashMap.
impl<C: Cell, D: Dimension> ChunkedGrid<C, D> {
    /// Constructs an empty ChunkedGrid with the default chunk size.
    fn new() -> Self {
        let chunk_size = get_recommended_chunk_size(Self::ndim());
        // I don't know how else to generate an array shape from a Dimension
        // type, but this works: Generate a zero vector like [0, 0].
        let mut chunk_shape = Self::origin();
        // Turn it into a mutable 1D array.
        let mut chunk_shape_array = chunk_shape.as_array_view_mut();
        // Increment each member of the array (i.e. the size along each axis) by the chunk size.
        chunk_shape_array += chunk_size;
        Self {
            chunks: HashMap::new(),
            chunk_size: chunk_size,
            default_chunk: ArcArray::default(chunk_shape),
        }
    }

    /// Returns a reference to the chunk with the given chunk coordinates.
    ///
    /// If the chunk does not exist.
    fn get_chunk(&self, chunk_index: &D) -> Option<&ArcArray<C, D>> {
        self.chunks.get(chunk_index)
    }

    /// Returns a mutable reference to the chunk with the given chunk
    /// coordinates.
    ///
    /// If the chunk does not exist, return None.
    fn get_chunk_mut(&mut self, chunk_index: &D) -> Option<&mut ArcArray<C, D>> {
        self.chunks.get_mut(chunk_index)
    }

    /// Returns a reference to the chunk with the given chunk coordinates, or an
    /// empty chunk if it does not exist.
    ///
    /// If the chunk does not exist, return a reference to a blank chunk.
    fn infer_chunk(&self, chunk_index: &D) -> &ArcArray<C, D> {
        self.get_chunk(chunk_index).unwrap_or(&self.default_chunk)
    }

    /// Returns a mutable reference to the chunk with the given chunk
    /// coordinates, creating it if it does not exist.
    ///
    /// If the chunk does not exist, create a new chunk at those coordinates and
    /// return a mutable reference to it.
    fn infer_chunk_mut(&mut self, chunk_index: &D) -> &mut ArcArray<C, D> {
        self.make_chunk(chunk_index);
        self.get_chunk_mut(chunk_index)
            .expect("Just created chunk, but not present")
    }

    /// Returns whether there is a chunk at the given chunk coordinates.
    fn has_chunk(&self, chunk_index: &D) -> bool {
        self.chunks.contains_key(chunk_index)
    }

    /// Returns whether the chunk at the given chunk coordinates is empty.
    ///
    /// Returns true if the chunk does not exist.
    fn is_chunk_empty(&self, chunk_index: &D) -> bool {
        match self.get_chunk(chunk_index) {
            None => true,
            Some(chunk) => chunk.iter().any(|&cell| cell == C::default()),
        }
    }

    /// Creates a chunk at the given chunk coordinates does not exist if there
    /// is none.
    ///
    /// If there is already a chunk there, this method does nothing.
    pub fn make_chunk(&mut self, chunk_index: &D) {
        if !self.has_chunk(chunk_index) {
            self.chunks
                .insert(chunk_index.clone(), self.default_chunk.clone());
        }
    }

    /// Returns the coordinates of the chunk containing the cell at the given
    /// position.
    fn cell_to_chunk_index(&self, mut cell_index: D) -> D {
        for axis in 0..cell_index.ndim() {
            cell_index[axis] /= self.chunk_size;
        }
        cell_index
    }

    /// Returns the local (intra-chunk) coordinates of the cell at the given global coordinates.
    fn cell_local_index(&self, mut cell_index: D) -> D {
        for axis in 0..cell_index.ndim() {
            cell_index[axis] %= self.chunk_size;
        }
        cell_index
    }
}

impl<C: Cell, D: Dimension> Grid<C, D> for ChunkedGrid<C, D> {
    fn get_cell(&self, index: D) -> C {
        let chunk_index = self.cell_to_chunk_index(index.clone());
        if let Some(chunk) = self.get_chunk(&chunk_index) {
            let local_index = self.cell_local_index(index);
            chunk[local_index]
        } else {
            C::default()
        }
    }

    fn set_cell(&mut self, index: D, cell_value: C) -> C {
        let chunk_index = self.cell_to_chunk_index(index.clone());
        let local_index = self.cell_local_index(index);
        let chunk = self.infer_chunk_mut(&chunk_index);
        std::mem::replace(&mut chunk[local_index], cell_value)
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_grid_construct() {
//         // ChunkedGrid::<usize, ndarray::Dim<[ndarray::Ix; 2]>, ndarray::Ix2>::new();
//         // let mut chunks = HashMap::new();
//         // chunks.insert((0, 0), ndarray::Array2::<u8>::zeros((16, 16)));
//         // let grid = ChunkedGrid { chunks: chunks };
//     }
// }
