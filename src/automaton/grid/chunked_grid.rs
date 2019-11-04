use ndarray::{Array, Dimension, NdIndex};
use std::collections::HashMap;

use super::Grid;

/// A "trait alias" for something that is an NdIndex and can also be used as a
/// key in a HashMap.
trait HashableIndex<D>: NdIndex<D> + std::cmp::Eq + std::hash::Hash {}
impl<T, D> HashableIndex<D> for T where T: NdIndex<D> + std::cmp::Eq + std::hash::Hash {}

struct ChunkedGrid<C, D: Dimension, I: HashableIndex<D>> {
    chunks: HashMap<I, Array<C, D>>,
    chunk_size: usize,
}

impl<C, D: Dimension, I: HashableIndex<D>> ChunkedGrid<C, D, I> {
    fn empty() -> Self {
        ChunkedGrid {
            chunks: HashMap::new(),
            chunk_size: 16,
        }
    }
}

impl<C, D: Dimension, I: HashableIndex<D>> Grid<C, D, I> for ChunkedGrid<C, D, I> {
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
        // let mut chunks = HashMap::new();
        // chunks.insert((0, 0), ndarray::Array2::<u8>::zeros((16, 16)));
        // let grid = ChunkedGrid { chunks: chunks };
    }
}
