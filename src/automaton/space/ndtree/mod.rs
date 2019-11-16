mod cache;
mod index;
mod subtree;

use super::*;
use cache::NdTreeCache;
use subtree::{NdSubTree, NdTreeNode};

/// An N-dimensional generalization of a quadtree.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NdTree<T: CellType, D: Dim> {
    root: NdSubTree<T, D>,
    offset: NdVec<D>,
}

/// A 1D grid represented as a bintree.
pub type NdTree1D<T> = NdTree<T, Vec1D>;
/// A 2D grid represented as a quadtree.
pub type NdTree2D<T> = NdTree<T, Vec2D>;
/// A 3D grid represented as an octree.
pub type NdTree3D<T> = NdTree<T, Vec3D>;
/// A 4D grid represented as a tree with nodes of degree 16.
pub type NdTree4D<T> = NdTree<T, Vec4D>;
/// A 5D grid represented as a tree with nodes of degree 32.
pub type NdTree5D<T> = NdTree<T, Vec5D>;
/// A 6D grid represented as a tree with nodes of degree 64.
pub type NdTree6D<T> = NdTree<T, Vec6D>;

impl<T: CellType, D: Dim> NdTree<T, D> {
    /// Constructs a new empty NdTree with an empty node cache centered on the
    /// origin.
    pub fn new() -> Self {
        let root = NdSubTree::default();
        let offset = NdVec::origin() - (root.node().len() as isize / 2);
        Self { root, offset }
    }

    /// Returns the minimum position in this NdTree.
    pub fn min(&self) -> NdVec<D> {
        self.offset
    }
    /// Returns the maximum position in this NdTree.
    pub fn max(&self) -> NdVec<D> {
        self.offset + (self.root.node().len() / 2 - 1) as isize
    }
    /// Returns the hyperrectangle that this NdTree spans.
    pub fn rect(&self) -> NdRect<D> {
        NdRect {
            a: self.min(),
            b: self.max(),
        }
    }
}
