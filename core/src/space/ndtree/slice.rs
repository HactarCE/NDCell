use num::ToPrimitive;
use std::ops::Index;

use super::*;

/// An immutable view into an NdTree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NdTreeSlice<D: Dim> {
    /// The root NdTreeNode of this slice.
    pub root: NdCachedNode<D>,
    /// The position of the lower bound of the root node.
    pub offset: BigVec<D>,
}

/// The same as NdTreeBranch, but using NdTreeSlice instead of NdCachedNode (so
/// it retains global location information).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NdTreeSliceBranch<D: Dim> {
    /// A "layer 0" node; i.e. a single cell.
    Leaf(u8, BigVec<D>),
    /// A tree slice whose layer is >= 1.
    Node(NdTreeSlice<D>),
}

/// A 1D grid represented as a bintree.
pub type NdTreeSlice1D = NdTreeSlice<Dim1D>;
/// A 2D grid represented as a quadtree.
pub type NdTreeSlice2D = NdTreeSlice<Dim2D>;
/// A 3D grid represented as an octree.
pub type NdTreeSlice3D = NdTreeSlice<Dim3D>;
/// A 4D grid represented as a tree with nodes of degree 16.
pub type NdTreeSlice4D = NdTreeSlice<Dim4D>;
/// A 5D grid represented as a tree with nodes of degree 32.
pub type NdTreeSlice5D = NdTreeSlice<Dim5D>;
/// A 6D grid represented as a tree with nodes of degree 64.
pub type NdTreeSlice6D = NdTreeSlice<Dim6D>;

impl fmt::Display for NdTreeSlice<Dim2D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.root.layer > 8 {
            panic!("Cannot display node larger than 256x256");
        }
        let rect = self.rect();
        let mut line = String::with_capacity(self.root.len().to_usize().unwrap() * 2);
        for y in rect.axis_range(Y).rev() {
            line.clear();
            for x in rect.axis_range(X) {
                let state = self[NdVec([x, y.clone()])];
                line.push(".#23456789".chars().nth(state as usize).unwrap_or('?'));
                line.push(' ');
            }
            line.pop();
            writeln!(f, "{}", line)?;
        }
        Ok(())
    }
}

impl<D: Dim> Index<BigVec<D>> for NdTreeSlice<D> {
    type Output = u8;
    fn index(&self, pos: BigVec<D>) -> &u8 {
        &self.root[&(pos - &self.offset)]
    }
}

impl<D: Dim> NdTreeSlice<D> {
    /// Constructs a new NdTreeSlice of a given node centered on the origin.
    pub fn centered(root: NdCachedNode<D>) -> Self {
        Self {
            offset: NdVec::origin() - &(root.len() / 2),
            root,
        }
    }

    /// Returns the NdRect bounding this slice.
    pub fn rect(&self) -> BigRect<D> {
        self.root.rect() + &self.offset
    }
    /// Returns the minimum position in this NdTree.
    pub fn min(&self) -> BigVec<D> {
        self.rect().min()
    }
    /// Returns the maximum position in this NdTree.
    pub fn max(&self) -> BigVec<D> {
        self.rect().max()
    }
    /// Returns the vector size of this NdTree.
    pub fn size(&self) -> BigVec<D> {
        self.rect().size()
    }

    /// Returns a reference to the cell value at the given position, if it is
    /// within the bounds of the slice.
    pub fn get_cell_ref(&self, pos: &BigVec<D>) -> Option<&u8> {
        if self.rect().contains(pos) {
            Some(self.root.get_cell_ref(&(pos - &self.offset)))
        } else {
            None
        }
    }
    /// Returns the cell value at the given position, if it is within the bounds
    /// of the slice.
    pub fn get_cell(&self, pos: &BigVec<D>) -> Option<u8> {
        self.get_cell_ref(pos).map(|cell| cell.clone())
    }

    /// Returns an NdTreeSlice of the root node's branch with the given branch
    /// index.
    pub fn get_branch(&self, branch_idx: ByteVec<D>) -> NdTreeSliceBranch<D> {
        match &self.root[branch_idx.clone()] {
            NdTreeBranch::Leaf(cell_state) => NdTreeSliceBranch::Leaf(
                *cell_state,
                self.rect().min() + &branch_idx.branch_offset(1),
            ),
            NdTreeBranch::Node(node) => NdTreeSliceBranch::Node(Self {
                root: node.clone(),
                offset: &self.offset + &branch_idx.branch_offset(self.root.layer),
            }),
        }
    }
}