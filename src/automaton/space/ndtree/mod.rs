mod cache;
mod subtree;

use super::*;
use cache::NdTreeCache;
use subtree::{NdSubTree, NdTreeNode};

/// An N-dimensional generalization of a quadtree.
#[derive(Debug, Clone)]
pub struct NdTree<T: CellType, D: Dim> {
    cache: NdTreeCache<T, D>,
    root: NdSubTree<T, D>,
    offset: NdVec<D>,
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
        let root = NdTreeNode::empty(0).intern(&mut cache);
        let offset = NdVec::origin();
        Self {
            cache,
            root,
            offset,
        }
    }

    /// Returns the minimum position in this NdTree.
    pub fn min(&self) -> NdVec<D> {
        self.offset
    }
    /// Returns the maximum position in this NdTree.
    pub fn max(&self) -> NdVec<D> {
        self.offset + (self.root.len() - 1) as isize
    }
    /// Returns the hyperrectangle that this NdTree spans.
    pub fn rect(&self) -> NdRect<D> {
        NdRect {
            a: self.min(),
            b: self.max(),
        }
    }

    fn expand_centered(&mut self) {
        self.root = self.root.expand_centered(&mut self.cache);
        self.offset -= self.root.len() as isize / 4;
    }

    /// Returns the state of the cell at the given position.
    pub fn get_cell(&self, pos: NdVec<D>) -> T {
        if self.rect().contains(pos) {
            self.root.get_cell(pos - self.offset)
        } else {
            T::default()
        }
    }

    /// Sets the state of the cell at the given position.
    pub fn set_cell(&mut self, pos: NdVec<D>, cell_state: T) {
        while !self.rect().contains(pos) {
            self.expand_centered();
        }
        println!("contains done");
        self.root
            .set_cell(&mut self.cache, pos - self.offset, cell_state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use std::collections::HashMap;

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
