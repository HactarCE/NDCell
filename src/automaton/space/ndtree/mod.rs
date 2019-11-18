mod cache;
mod slice;
mod subtree;

use super::*;
use cache::NdTreeCache;
pub use slice::*;
use subtree::{NdSubTree, NdTreeNode};

/// An N-dimensional generalization of a quadtree.
#[derive(Debug, Clone)]
pub struct NdTree<T: CellType, D: Dim> {
    cache: NdTreeCache<T, D>,
    slice: NdTreeSlice<T, D>,
}

/// A 1D grid represented as a bintree.
pub type NdTree1D<T> = NdTree<T, Dim1D>;
/// A 2D grid represented as a quadtree.
pub type NdTree2D<T> = NdTree<T, Dim2D>;
/// A 3D grid represented as an octree.
pub type NdTree3D<T> = NdTree<T, Dim3D>;
/// A 4D grid represented as a tree with nodes of degree 16.
pub type NdTree4D<T> = NdTree<T, Dim4D>;
/// A 5D grid represented as a tree with nodes of degree 32.
pub type NdTree5D<T> = NdTree<T, Dim5D>;
/// A 6D grid represented as a tree with nodes of degree 64.
pub type NdTree6D<T> = NdTree<T, Dim6D>;

impl<T: CellType, D: Dim> Default for NdTree<T, D> {
    fn default() -> Self {
        Self::new()
    }
}
impl<T: CellType, D: Dim> NdTree<T, D> {
    /// Constructs a new empty NdTree with an empty node cache centered on the
    /// origin.
    pub fn new() -> Self {
        let mut cache = Default::default();
        let root = NdTreeNode::empty(&mut cache, 1);
        let offset = NdVec::origin();
        Self {
            cache,
            slice: NdTreeSlice::new(root, offset),
        }
    }

    /// Returns the hyperrectangle that this NdTree spans.
    pub fn rect(&self) -> NdRect<D> {
        self.slice.rect()
    }
    /// Returns the minimum position in this NdTree.
    pub fn min(&self) -> NdVec<D> {
        self.rect().min()
    }
    /// Returns the maximum position in this NdTree.
    pub fn max(&self) -> NdVec<D> {
        self.rect().max()
    }

    /// "Zooms out" of the current tree by a factor of 2.
    fn expand_centered(&mut self) {
        let new_root = self.slice.root().expand_centered(&mut self.cache);
        let new_offset = self.slice.offset()
            - match new_root.layer() {
                1 => 1,
                _ => new_root.len() as isize / 4,
            };
        self.slice = NdTreeSlice::new(new_root, new_offset);
    }
    /// Offsets the entire grid so that the given position is the new origin.
    pub fn recenter(&mut self, pos: NdVec<D>) {
        let new_offset = self.slice.offset() - pos;
        self.slice = NdTreeSlice::new(self.slice.root().clone(), new_offset);
    }

    /// Returns the state of the cell at the given position.
    pub fn get_cell(&self, pos: NdVec<D>) -> T {
        self.slice.get_cell(pos).unwrap_or_default()
    }
    /// Sets the state of the cell at the given position.
    pub fn set_cell(&mut self, pos: NdVec<D>, cell_state: T) {
        while !self.rect().contains(pos) {
            self.expand_centered();
        }
        self.slice = NdTreeSlice::new(
            self.slice
                .root()
                .set_cell(&mut self.cache, pos - self.slice.offset(), cell_state),
            self.slice.offset(),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use std::collections::HashMap;

    // Proptest gives crummy tracebacks due to macro use, so uncomment this test
    // and change the input values for debugging.

    // #[test]
    // fn test_ndtree_set_get_tmp() {
    //     let cells_to_set = vec![([0, 0, 0].into(), 1), ([0, 0, -17].into(), 0)];
    //     let cells_to_get = vec![[-1, -1, -1].into()];
    //     let mut ndtree = NdTree::new();
    //     let mut hashmap = HashMap::new();
    //     for (pos, state) in cells_to_set {
    //         hashmap.insert(pos, state);
    //         ndtree.set_cell(pos, state);
    //     }
    //     println!("{:?}", ndtree);
    //     for pos in cells_to_get {
    //         assert_eq!(hashmap.get(&pos).unwrap_or(&0), &ndtree.get_cell(pos));
    //     }
    // }

    proptest! {
        #![proptest_config(ProptestConfig {
            max_shrink_iters: 4096,
            ..Default::default()
        })]

        /// Tests setting and getting arbitrary grid cells by comparing against
        /// a HashMap.
        #[test]
        fn test_ndtree_set_get(
            cells_to_set: Vec<(Vec3D, u8)>,
            cells_to_get: Vec<Vec3D>
        ) {
            let mut ndtree = NdTree::new();
            let mut hashmap = HashMap::new();
            for (pos, state) in cells_to_set {
                hashmap.insert(pos, state);
                ndtree.set_cell(pos, state);
            }
            for pos in cells_to_get {
                assert_eq!(hashmap.get(&pos).unwrap_or(&0), &ndtree.get_cell(pos));
            }
        }
    }
}
