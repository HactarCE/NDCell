use ndarray::{Array, NdIndex};
use std::collections::HashMap;

use super::{Cell, Dimension, Grid};

struct ChunkedGrid<C: Cell, D: Dimension> {
    chunks: HashMap<D, Array<C, D>>,
    chunk_size: usize,
    default_chunk: Array<C, D>,
}

impl<C: Cell, D: Dimension> ChunkedGrid<C, D> {
    fn new() -> Self {
        let chunk_size = 16;
        // I don't know how else to generate an array shape from a Dimension
        // type, but this works: Generate a zero vector like [0, 0].
        let mut chunk_shape = D::zeros(D::NDIM.unwrap());
        // Turn it into a mutable 1D array.
        let mut mut_chunk_shape = chunk_shape.as_array_view_mut();
        // Increment each member of the array (i.e. the size along each axis) by the chunk size.
        mut_chunk_shape += chunk_size;
        Self {
            chunks: HashMap::new(),
            chunk_size: chunk_size,
            default_chunk: Array::default(chunk_shape),
        }
    }
    pub fn get_chunk(&self, chunk_index: &D) -> Option<&Array<C, D>> {
        self.chunks.get(chunk_index)
    }
    pub fn get_chunk_mut(&mut self, chunk_index: &D) -> Option<&mut Array<C, D>> {
        self.chunks.get_mut(chunk_index)
    }
    pub fn make_chunk(&mut self, chunk_index: D) {
        if !self.chunks.contains_key(&chunk_index) {
            self.chunks.insert(chunk_index, self.default_chunk.clone());
        }
    }
    pub fn cell_to_chunk_index(&self, cell_index: D) -> D {
        for axis in 0..cell_index.ndim() {
            cell_index[axis] /= self.chunk_size;
        }
        cell_index
    }
}

impl<C: Cell, D: Dimension> Grid<C, D> for ChunkedGrid<C, D> {
    fn get_cell(&self, index: D) -> Option<&C> {
        self.chunks.get(&index)?.get(index)
        // panic!("Not yet implemented");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grid_construct() {
        // ChunkedGrid::<usize, ndarray::Dim<[ndarray::Ix; 2]>, ndarray::Ix2>::new();
        // let mut chunks = HashMap::new();
        // chunks.insert((0, 0), ndarray::Array2::<u8>::zeros((16, 16)));
        // let grid = ChunkedGrid { chunks: chunks };
    }
}
