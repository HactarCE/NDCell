mod cache;
mod index;
mod subtree;

use super::*;
use cache::NdTreeCache;
use subtree::{NdSubTree, NdTreeChild, NdTreeNode};

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

impl<T: CellType, D: Dim> Default for NdTree<T, D> {
    fn default() -> Self {
        Self::new()
    }
}
impl<T: CellType, D: Dim> NdTree<T, D> {
    /// Constructs a new empty NdTree with an empty node cache centered on the
    /// origin.
    pub fn new() -> Self {
        let root = NdTreeNode::empty(Default::default(), 0).intern();
        let offset = NdVec::origin() - (root.len() as isize / 2);
        Self { root, offset }
    }

    /// Returns the minimum position in this NdTree.
    pub fn min(&self) -> NdVec<D> {
        self.offset
    }
    /// Returns the maximum position in this NdTree.
    pub fn max(&self) -> NdVec<D> {
        self.offset + (self.root.len() / 2 - 1) as isize
    }
    /// Returns the hyperrectangle that this NdTree spans.
    pub fn rect(&self) -> NdRect<D> {
        NdRect {
            a: self.min(),
            b: self.max(),
        }
    }

    // pub fn get_cell(&self, pos: NdVec<D>) -> T {
    //     self.root.get_cell(pos - self.offset)
    // }

    fn expand_layer(&mut self) {
        self.root = NdTreeNode::with_child(
            self.root.cache.clone(),
            self.root.layer + 1,
            NdTreeChild::Branch({
                let mut new_branches = Vec::with_capacity(NdVec::<D>::BRANCHES);
                for branch_idx in 0..NdVec::<D>::BRANCHES {
                    new_branches[branch_idx] = match &self.root.child {
                        NdTreeChild::Leaf(cell_state) => NdTreeNode::with_child(
                            self.root.cache.clone(),
                            self.root.layer,
                            NdTreeChild::Leaf(*cell_state),
                        )
                        .intern(),
                        NdTreeChild::Branch(old_branches) => {
                            old_branches[branch_idx ^ NdVec::<D>::BRANCH_IDX_BITMASK].clone()
                        }
                    }
                }
                new_branches
            }),
        )
        .intern()
    }

    // pub fn set_cell(&self, pos: NdVec<D>, cell_state: T) {
    //     while !self.rect().contains(pos) {
    //         self.expand_layer();
    //     }
    //     // self.root.expand_to(pos.)
    //     self.root.set_cell(pos - self.offset, cell_state);
    // }
}
