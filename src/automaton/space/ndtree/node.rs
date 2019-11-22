use seahash::SeaHasher;
use std::borrow::Borrow;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::ops::Index;

use crate::automaton::*;

/// A tree node with only branches and a hash code; it does not contain
/// information about its layer, population, etc. While an NdTreeNode instance
/// is guaranteed to have 2**d branches with the same layer, NdBaseTreeNode is
/// not.
#[derive(Debug, Clone)]
pub struct NdBaseTreeNode<C: CellType, D: Dim> {
    /// The branches of this node, stored as a flattened 2^d hypercube of nodes
    /// one layer lower.
    ///
    /// If layer == 1, then all of these must be `NdTreeBranch::Leaf`s. If layer
    /// > 1, then all of these must be `NdTreeBranch::Branch`es.
    ///
    /// Until rust-lang #44580 (RFC 2000) is resolved, there's no way to use
    /// D::NDIM as the array size. It might be worth implementing a custom
    /// unsafe type for this, but at the time of writing such an optimization
    /// would be entirely premature.
    pub branches: Vec<NdTreeBranch<C, D>>,

    /// This node's hash, based solely on the hashes of its branches.
    pub hash_code: u64,

    /// Phantom because we don't technically own anything of type D, but we
    /// still definitely need to know it.
    phantom: PhantomData<D>,
}
impl<C: CellType, D: Dim> From<Vec<NdTreeBranch<C, D>>> for NdBaseTreeNode<C, D> {
    fn from(branches: Vec<NdTreeBranch<C, D>>) -> Self {
        let mut hasher = SeaHasher::new();
        branches.hash(&mut hasher);
        Self {
            branches,
            hash_code: hasher.finish(),
            phantom: PhantomData,
        }
    }
}
impl<C: CellType, D: Dim> Hash for NdBaseTreeNode<C, D> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        // We already cached our own hash, so just rehash that if you want to.
        self.hash_code.hash(hasher);
    }
}
impl<C: CellType, D: Dim> Eq for NdBaseTreeNode<C, D> {}
impl<C: CellType, D: Dim> PartialEq for NdBaseTreeNode<C, D> {
    fn eq(&self, rhs: &Self) -> bool {
        // Check for pointer equality (very fast; guarantees true).
        std::ptr::eq(self, rhs)
            // If that fails, check hash codes (very fast; guarantees false).
            || (self.hash_code == rhs.hash_code
                // If neither of those worked, we have to check the hard way.
                && self.branches == rhs.branches)
    }
}

/// A single node in the NdTree, which contains information about its layer
/// (base-2 logarithm of hypercube side length) and its children.
#[derive(Clone)]
pub struct NdTreeNode<C: CellType, D: Dim> {
    /// The member containing the branches and hash code.
    pub base: NdBaseTreeNode<C, D>,

    /// The "layer" of this node (base-2 logarithm of hypercube side length).
    pub layer: usize,

    /// The population of this node.
    pub population: usize,
}

// Implement Borrow so that NdBaseTreeNode can be used for HashSet lookups.
impl<C: CellType, D: Dim> Borrow<NdBaseTreeNode<C, D>> for NdTreeNode<C, D> {
    fn borrow(&self) -> &NdBaseTreeNode<C, D> {
        &self.base
    }
}

// Allow use of NdTreeNode as if it's an NdBaseTreeNode.
impl<C: CellType, D: Dim> Deref for NdTreeNode<C, D> {
    type Target = NdBaseTreeNode<C, D>;
    fn deref(&self) -> &NdBaseTreeNode<C, D> {
        &self.base
    }
}

impl<C: CellType, D: Dim> From<NdBaseTreeNode<C, D>> for NdTreeNode<C, D> {
    fn from(base: NdBaseTreeNode<C, D>) -> Self {
        let branches = &base.branches;
        // Compute the layer based on the layer of the node's branches, and check
        // that all branches are at the same layer.
        // Check that there are the correct number of branches.
        assert_eq!(
            Self::BRANCHES,
            branches.len(),
            "Node with {} dimensions must have {} branches; got {} instead.",
            D::NDIM,
            Self::BRANCHES,
            branches.len()
        );
        // Check that all branches are at the same layer.
        let mut branch_iter = branches.iter();
        let branch_layer = branch_iter.next().unwrap().get_layer();
        assert!(
            branch_iter.all(|branch| branch.get_layer() == branch_layer),
            "Cannot construct node using branches with differing layers: {:?}",
            branches
        );
        // This node is at the layer above all of its branches.
        let layer = branch_layer + 1;
        // Compute the population based on the population of each of the node's
        // branches.
        let population = branches.iter().map(|branch| branch.population()).sum();
        Self {
            base,
            layer,
            population,
        }
    }
}

impl<C: CellType, D: Dim> fmt::Debug for NdTreeNode<C, D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NdTreeNode {{ branches: {:?} }}", self.branches)
    }
}

impl<C: CellType, D: Dim> Eq for NdTreeNode<C, D> {}
impl<C: CellType, D: Dim> PartialEq for NdTreeNode<C, D> {
    fn eq(&self, rhs: &Self) -> bool {
        // Delegate to NdBaseTreeNode.
        self.base == rhs.base
    }
}
impl<C: CellType, D: Dim> Hash for NdTreeNode<C, D> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        // We already cached our own hash; just rehash that if you want to.
        self.hash_code.hash(hasher);
    }
}

impl<C: CellType, D: Dim> Index<NdVec<D>> for NdTreeNode<C, D> {
    type Output = C;
    fn index(&self, pos: NdVec<D>) -> &C {
        match &self.branches[self.branch_idx(pos)] {
            NdTreeBranch::Leaf(cell_state) => cell_state,
            NdTreeBranch::Node(node) => &node[pos],
        }
    }
}

impl<C: CellType, D: Dim> NdTreeNode<C, D> {
    /// The number of branches for this many dimensions (2 ** d).
    pub const BRANCHES: usize = 1 << D::NDIM;
    /// The bitmask for branch indices.
    pub const BRANCH_IDX_BITMASK: usize = Self::BRANCHES - 1;

    /// Returns false if this node contains at least one non-default cell, or
    /// true if it contains only default cells.
    pub fn is_empty(&self) -> bool {
        self.population == 0
    }

    /// Returns the side length of the rectangle encompassing this node.
    pub fn len(&self) -> usize {
        Self::len_at_layer(self.layer)
    }
    /// Returns the side length of the rectangle encompassing a node at the
    /// given layer.
    pub fn len_at_layer(layer: usize) -> usize {
        // layer = 1 => len = 2
        // layer = 2 => len = 4
        // layer = 3 => len = 8
        // etc.
        1 << layer
    }
    /// Returns a hyperrectangle the size of this node with the origin as the
    /// lower bound.
    pub fn rect(&self) -> NdRect<D> {
        Self::rect_at_layer(self.layer)
    }
    /// Returns a hyperrectangle the size of a node at the given layer with the
    /// origin as the lower bound.
    pub fn rect_at_layer(layer: usize) -> NdRect<D> {
        NdRect::span(
            NdVec::origin(),
            NdVec::origin() + (Self::len_at_layer(layer) as isize - 1),
        )
    }

    /// Computes the "branch index" corresponding to the child of this node
    /// containing the given position.
    ///
    /// See NdTreeNode::branch_idx_at_layer() for more info.
    pub fn branch_idx(&self, pos: NdVec<D>) -> usize {
        Self::branch_idx_at_layer(self.layer, pos)
    }
    /// Computes the "branch index" corresponding to the child of a node the
    /// given layer containing the given position.
    ///
    /// The nth node layer corresponds to the (n - 1)th bit of each axis, which
    /// can either be 0 or 1. The "branch index" is a number in 0..(2 ** d)
    /// composed from these bits; each bit in the branch index is taken from a
    /// different axis. It's like a bitwise NdVec that selects one
    /// half/quadrant/etc. from the node's rectangle.
    pub fn branch_idx_at_layer(layer: usize, pos: NdVec<D>) -> usize {
        let mut ret = 0;
        for ax in D::axes() {
            ret <<= 1;
            ret |= (pos[ax] as usize >> (layer - 1)) & 1;
        }
        ret
    }

    /// Computes the vector offset for the given branch of this node.
    pub fn branch_offset(&self, branch_idx: usize) -> NdVec<D> {
        Self::branch_offset_at_layer(self.layer, branch_idx)
    }
    /// Computes the vector offset for the given branch of a node at the given
    /// layer.
    pub fn branch_offset_at_layer(layer: usize, branch_idx: usize) -> NdVec<D> {
        let mut ret = NdVec::origin();
        let halfway = 1 << (layer - 1);
        for ax in D::axes() {
            // If the current bit of the branch index is 1, add half of the
            // length of this node to the corresponding axis in the result.
            let axis_bit_idx = D::NDIM - 1 - ax as usize;
            let axis_bit = (branch_idx as isize >> axis_bit_idx) & 1;
            ret[ax] += halfway * axis_bit;
        }
        ret
    }

    /// Returns the cell value at the given position, modulo the node size.
    pub fn get_cell(&self, pos: NdVec<D>) -> C {
        match &self.branches[self.branch_idx(pos)] {
            NdTreeBranch::Leaf(cell_state) => *cell_state,
            NdTreeBranch::Node(node) => node.get_cell(pos),
        }
    }
    /// Returns a node with the cell at the given position, modulo the node
    /// size, having the given cell state.
    pub fn set_cell(
        &self,
        cache: &mut NdTreeCache<C, D>,
        pos: NdVec<D>,
        cell_state: C,
    ) -> NdCachedNode<C, D> {
        let mut new_branches = self.branches.clone();
        // Get the branch containing the given cell.
        let branch = &mut new_branches[self.branch_idx(pos)];
        match branch {
            // The branch is a single cell, so set that cell.
            NdTreeBranch::Leaf(old_cell_state) => *old_cell_state = cell_state,
            // The branch is a node, so recurse on that node.
            NdTreeBranch::Node(node) => *node = node.set_cell(cache, pos, cell_state),
        }
        cache.get_node(new_branches)
    }
}

/// A single branch of an NdNode; an NdNode's child.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NdTreeBranch<C: CellType, D: Dim> {
    /// All cells within this branch are the same cell state.
    Leaf(C),

    /// An interned subnode.
    Node(NdCachedNode<C, D>),
}

impl<C: CellType, D: Dim> NdTreeBranch<C, D> {
    /// Returns the layer of this tree branch, which is the same as its
    /// contained node (if it is a node) or 0 if it is a leaf.
    pub fn get_layer(&self) -> usize {
        match self {
            NdTreeBranch::Leaf(_) => 0,
            NdTreeBranch::Node(node) => node.layer,
        }
    }

    /// Returns the number of non-default cells in this branch, which is the
    /// same as its contained node (if it is a node) or 0 if it is a leaf with
    /// the default cell state or 1 if it is a leaf with a non-default cell
    /// state.
    pub fn population(&self) -> usize {
        match self {
            NdTreeBranch::Leaf(cell_state) => {
                if *cell_state == C::default() {
                    0
                } else {
                    1
                }
            }
            NdTreeBranch::Node(node) => node.population,
        }
    }
    /// Returns false if this branch contains at least one non-default cell, or
    /// true if it contains only default cells.
    pub fn is_empty(&self) -> bool {
        self.population() == 0
    }
}
