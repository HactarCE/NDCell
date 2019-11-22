use std::ops::Index;
use std::rc::Rc;

use super::*;

/// An immutable view into an NdTree.
#[derive(Debug, Clone)]
pub struct NdTreeSlice<T: CellType, D: Dim> {
    /// The root NdTreeNode of this slice.
    pub root: Rc<NdTreeNode<T, D>>,
    /// The position of the lower bound of the root node.
    pub offset: NdVec<D>,
}

/// A 1D grid represented as a bintree.
pub type NdTreeSlice1D<T> = NdTreeSlice<T, Dim1D>;
/// A 2D grid represented as a quadtree.
pub type NdTreeSlice2D<T> = NdTreeSlice<T, Dim2D>;
/// A 3D grid represented as an octree.
pub type NdTreeSlice3D<T> = NdTreeSlice<T, Dim3D>;
/// A 4D grid represented as a tree with nodes of degree 16.
pub type NdTreeSlice4D<T> = NdTreeSlice<T, Dim4D>;
/// A 5D grid represented as a tree with nodes of degree 32.
pub type NdTreeSlice5D<T> = NdTreeSlice<T, Dim5D>;
/// A 6D grid represented as a tree with nodes of degree 64.
pub type NdTreeSlice6D<T> = NdTreeSlice<T, Dim6D>;

impl<T: CellType, D: Dim> Index<NdVec<D>> for NdTreeSlice<T, D> {
    type Output = T;
    fn index(&self, pos: NdVec<D>) -> &T {
        &self.root[pos]
    }
}

impl<T: CellType, D: Dim> NdTreeSlice<T, D> {
    /// Constructs a new NdTreeSlice centered on a given node.
    pub fn centered(root: Rc<NdTreeNode<T, D>>) -> Self {
        Self {
            offset: NdVec::origin() - root.len() as isize / 2,
            root,
        }
    }

    /// Returns the NdRect bounding this slice.
    pub fn rect(&self) -> NdRect<D> {
        self.root.rect() + self.offset
    }

    /// Returns the cell at the given position, if it is within the bounds of the slice.
    pub fn get_cell(&self, pos: NdVec<D>) -> Option<T> {
        if self.rect().contains(pos) {
            Some(self.root.get_cell(pos - self.offset))
        } else {
            None
        }
    }

    // /// Returns an NdTreeSlice containing the root node's branch with the given
    // /// branch index.
    // pub fn get_branch_slice(&self, branch_idx: usize) -> Self {
    //     Self {
    //         root: match &self.root.branches[branch_idx] {
    //             NdTreeBranch::Leaf(_) => panic!("Cannot take slice of node at layer 1"),
    //             NdTreeBranch::Node(node) => node.clone(),
    //         },
    //         offset: self.offset + self.root.branch_offset(branch_idx),
    //     }
    // }
}
