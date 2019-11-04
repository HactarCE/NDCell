use ndarray::{Array, Dimension, NdIndex};
use std::collections::HashMap;

use super::{Cell, Grid};

/// A "trait alias" for something that is an NdIndex and can also be used as a
/// key in a HashMap.
trait HashableIndex<D>: NdIndex<D> + std::cmp::Eq + std::hash::Hash {}
impl<T, D> HashableIndex<D> for T where T: NdIndex<D> + std::cmp::Eq + std::hash::Hash {}

struct ChunkedGrid<C: Cell, D: Dimension, I: HashableIndex<D>> {
    chunks: HashMap<I, Array<C, D>>,
    chunk_size: usize,
    default_chunk: Array<C, D>,
}

impl<C: Cell, D: Dimension, I: HashableIndex<D>> ChunkedGrid<C, D, I> {
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
    fn get_chunk(&self, chunk_index: &I) -> Option<&Array<C, D>> {
        self.chunks.get(chunk_index)
    }
    fn get_chunk_mut(&mut self, chunk_index: &I) -> Option<&mut Array<C, D>> {
        self.chunks.get_mut(chunk_index)
    }
}

impl<C: Cell, D: Dimension, I: HashableIndex<D>> Grid<C, D, I> for ChunkedGrid<C, D, I> {
    fn get_cell(&self, index: I) -> Option<&C> {
        self.chunks.get(&index)?.get(index)
        // panic!("Not yet implemented");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grid_construct() {
        ChunkedGrid::<usize, ndarray::Dim<[ndarray::Ix; 2]>, ndarray::Ix2>::new();
        // let mut chunks = HashMap::new();
        // chunks.insert((0, 0), ndarray::Array2::<u8>::zeros((16, 16)));
        // let grid = ChunkedGrid { chunks: chunks };
    }
}
