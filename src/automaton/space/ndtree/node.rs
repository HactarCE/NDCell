use num::{BigInt, One, ToPrimitive, Zero};
use seahash::SeaHasher;
use std::borrow::Borrow;
use std::convert::From;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, Index};

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
#[derive(Clone, Eq)]
pub struct NdTreeNode<C: CellType, D: Dim> {
    /// The member containing the branches and hash code.
    pub base: NdBaseTreeNode<C, D>,

    /// The "layer" of this node (base-2 logarithm of hypercube side length).
    pub layer: usize,

    /// The population of this node.
    pub population: BigInt,
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
            D::TREE_BRANCHES,
            branches.len(),
            "Node with {} dimensions must have {} branches; got {} instead",
            D::NDIM,
            D::TREE_BRANCHES,
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
        let big_zero = BigInt::zero();
        let big_one = BigInt::one();
        let population = branches
            .iter()
            .map(|branch| match branch {
                NdTreeBranch::Leaf(cell_state) => {
                    if *cell_state == C::default() {
                        &big_zero
                    } else {
                        &big_one
                    }
                }
                NdTreeBranch::Node(node) => &node.population,
            })
            .sum();
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

// Implement indexing a cell of an NdTreeNode.
impl<C: CellType, D: Dim> Index<&BigVec<D>> for NdTreeNode<C, D> {
    type Output = C;
    fn index(&self, pos: &BigVec<D>) -> &C {
        self.get_cell_ref(pos)
    }
}
impl<C: CellType, D: Dim> Index<IVec<D>> for NdTreeNode<C, D> {
    type Output = C;
    fn index(&self, pos: IVec<D>) -> &C {
        self.get_cell_ref(&pos)
    }
}
impl<C: CellType, D: Dim> Index<UVec<D>> for NdTreeNode<C, D> {
    type Output = C;
    fn index(&self, pos: UVec<D>) -> &C {
        self.get_cell_ref(&pos)
    }
}

// Implement indexing a branch of an NdTreeNode.
impl<C: CellType, D: Dim> Index<ByteVec<D>> for NdTreeNode<C, D> {
    type Output = NdTreeBranch<C, D>;
    fn index(&self, branch_idx: ByteVec<D>) -> &NdTreeBranch<C, D> {
        &self.branches[branch_idx.to_array_idx()]
    }
}

impl<C: CellType, D: Dim> NdTreeNode<C, D> {
    /// Returns false if this node contains at least one non-default cell, or
    /// true if it contains only default cells.
    pub fn is_empty(&self) -> bool {
        self.population.is_zero()
    }

    /// Returns the side length of the rectangle encompassing this node.
    pub fn len(&self) -> BigInt {
        Self::len_at_layer(self.layer)
    }
    /// Returns the side length of the rectangle encompassing a node at the
    /// given layer.
    pub fn len_at_layer(layer: usize) -> BigInt {
        // layer = 1 => len = 2
        // layer = 2 => len = 4
        // layer = 3 => len = 8
        // etc.
        BigInt::one() << layer
    }
    /// Returns a hyperrectangle the size of this node with the origin as the
    /// lower bound.
    pub fn rect(&self) -> BigRect<D> {
        Self::rect_at_layer(self.layer)
    }
    /// Returns a hyperrectangle the size of a node at the given layer with the
    /// origin as the lower bound.
    pub fn rect_at_layer(layer: usize) -> BigRect<D> {
        NdRect::new(NdVec::origin(), NdVec::repeat(Self::len_at_layer(layer)))
    }

    /// Returns the bounding rectangle of this node's inner node.
    pub fn inner_rect(&self) -> BigRect<D> {
        self.rect() + &(self.len() / 4)
    }
    /// Returns the inner node of this one.
    ///
    /// A node's inner node is the node one layer down, centered on the original
    /// node. For example, the inner node of a 16x16 node (layer 4) is the 8x8
    /// node (layer 3) centered on it.
    pub fn get_inner_node(&self, cache: &mut NdTreeCache<C, D>) -> NdCachedNode<C, D> {
        assert_ne!(1, self.layer, "Cannot take inner node of node at layer 1");
        let new_branches = self
            .branches
            .iter()
            .enumerate()
            .map(|(branch_idx, branch)| {
                branch.node().unwrap()[ByteVec::from_array_idx(branch_idx).opposite()].clone()
            })
            .collect();
        cache.get_node(new_branches)
    }

    /// Returns the NdTreeBranchIndex of the branch of this node containing the
    /// given position.
    pub fn branch_idx<I: NdTreeIndex<D>>(&self, pos: &I) -> ByteVec<D> {
        pos.branch_idx(self.layer)
    }

    /// Computes the vector offset for the given branch of this node.
    pub fn branch_offset<I: NdTreeBranchIndex<D>>(&self, branch_idx: I) -> BigVec<D> {
        branch_idx.branch_offset(self.layer)
    }

    /// Returns the specified sub-branch which is two layers below the given
    /// node, given a ByteVec with values in the range 0..=3.
    pub fn get_sub_branch(&self, sub_branch_idx: ByteVec<D>) -> &NdTreeBranch<C, D> {
        // Get the more significant bit.
        let outer_branch_idx = sub_branch_idx.clone() >> 1;
        // Get the less significant bit.
        let inner_branch_idx = sub_branch_idx & 1;
        // Now use those to index into the node.
        &self[outer_branch_idx]
            .node()
            .expect("Cannot get sub-branch of node at layer 1")[inner_branch_idx]
    }

    /// Returns a reference to the cell value at the given position, modulo the
    /// node size. This is the same as indexing the node using the vector.
    pub fn get_cell_ref<I: NdTreeIndex<D>>(&self, pos: &I) -> &C {
        match &self[self.branch_idx(pos)] {
            NdTreeBranch::Leaf(cell_state) => cell_state,
            NdTreeBranch::Node(node) => node.get_cell_ref(pos),
        }
    }
    /// Returns the cell value at the given position, modulo the node size.
    pub fn get_cell<I: NdTreeIndex<D>>(&self, pos: &I) -> C {
        self.get_cell_ref(pos).clone()
    }
    /// Returns a node with the cell at the given position, modulo the node
    /// size, having the given cell state.
    #[must_use]
    pub fn set_cell<I: NdTreeIndex<D>>(
        &self,
        cache: &mut NdTreeCache<C, D>,
        pos: &I,
        cell_state: C,
    ) -> NdCachedNode<C, D> {
        let mut new_branches = self.branches.clone();
        // Get the branch containing the given cell.
        let branch = &mut new_branches[self.branch_idx(pos).to_array_idx()];
        match branch {
            // The branch is a single cell, so set that cell.
            NdTreeBranch::Leaf(old_cell_state) => *old_cell_state = cell_state,
            // The branch is a node, so recurse on that node.
            NdTreeBranch::Node(node) => *node = node.set_cell(cache, pos, cell_state),
        }
        cache.get_node(new_branches)
    }
    /// Returns an iterator over the branches of this node.
    pub fn branch_iter(&self) -> impl Iterator<Item = (ByteVec<D>, &NdTreeBranch<C, D>)> {
        self.branches
            .iter()
            .enumerate()
            .map(|(array_idx, branch)| (ByteVec::from_array_idx(array_idx), branch))
    }
}

/// A single branch of an NdNode; an NdNode's child.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NdTreeBranch<C: CellType, D: Dim> {
    /// A "layer 0" node; i.e. a single cell.
    Leaf(C),
    /// A cached node whose layer is >= 1.
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

    /// Returns the inner cell state if this is a leaf, or None if it is a node.
    pub fn leaf(&self) -> Option<C> {
        match self {
            NdTreeBranch::Leaf(state) => Some(*state),
            _ => None,
        }
    }

    /// Returns the inner node if this is a node, or None if it is a leaf.
    pub fn node(&self) -> Option<&NdCachedNode<C, D>> {
        match self {
            NdTreeBranch::Node(node) => Some(node),
            _ => None,
        }
    }

    /// Returns false if this branch contains at least one non-default cell, or
    /// true if it contains only default cells.
    pub fn is_empty(&self) -> bool {
        match self {
            NdTreeBranch::Leaf(cell_state) => *cell_state != C::default(),
            NdTreeBranch::Node(node) => node.is_empty(),
        }
    }
}

/// A trait for NdVecs that can be used to select a cell from an NdTree.
pub trait NdTreeIndex<D: Dim> {
    /// Returns the "branch index" corresponding to the child of a node at the
    /// given layer that contains the position defined by this vector. Each axis
    /// of a branch index is either 0 or 1.
    fn branch_idx(&self, layer: usize) -> ByteVec<D>;
}
impl<D: Dim> NdTreeIndex<D> for BigVec<D> {
    fn branch_idx(&self, layer: usize) -> ByteVec<D> {
        ByteVec::from_fn(|ax| (&self[ax] >> (layer - 1)).to_u8().unwrap() & 1)
    }
}
impl<D: Dim> NdTreeIndex<D> for IVec<D> {
    fn branch_idx(&self, layer: usize) -> ByteVec<D> {
        ByteVec::from_fn(|ax| (self[ax] >> (layer - 1)) as u8 & 1)
    }
}
impl<D: Dim> NdTreeIndex<D> for UVec<D> {
    fn branch_idx(&self, layer: usize) -> ByteVec<D> {
        ByteVec::from_fn(|ax| (self[ax] >> (layer - 1)) as u8 & 1)
    }
}

/// A trait for NdVecs that can be used to select a branch of an NdTreeNode.
/// This is only implemented by ByteVec<D>.
pub trait NdTreeBranchIndex<D: Dim>: NdRectVec {
    /// Returns the vector offset for this branch of a node at the given layer.
    fn branch_offset<N: NdVecNum>(self, layer: usize) -> NdVec<D, N>
    where
        N: From<u8> + std::ops::ShlAssign<usize>,
        D: DimFor<N>;
    /// Returns the "flat" index of the corresponding branch in an array;
    fn to_array_idx(self) -> usize;
    /// Returns the NdTreeBranchIndex given an array index.
    fn from_array_idx(array_idx: usize) -> Self;
    /// Flips the given axis of the branch index. (0 becomes 1, 1 becomes 0.)
    #[must_use]
    fn negate(self, ax: Axis) -> Self;

    /// Flips all axis of the branch index. (0 becomes 1, 1 becomes 0.)
    #[must_use]
    fn opposite(self) -> Self;
}
impl<D: Dim> NdTreeBranchIndex<D> for ByteVec<D> {
    fn branch_offset<N: NdVecNum>(self, layer: usize) -> NdVec<D, N>
    where
        N: From<u8> + std::ops::ShlAssign<usize>,
        D: DimFor<N>,
    {
        self.convert() << (layer - 1)
    }
    fn to_array_idx(self) -> usize {
        let mut ret = 0;
        for &ax in D::axes() {
            ret |= (self[ax] as usize & 1) << ax as usize;
        }
        ret
    }
    fn from_array_idx(array_idx: usize) -> Self {
        Self::from_fn(|ax| (array_idx >> ax as usize) as u8 & 1)
    }
    fn negate(self, ax: Axis) -> Self {
        let mut ret = self;
        ret[ax] = 1 - ret[ax];
        ret
    }
    fn opposite(self) -> Self {
        Self::from_fn(|ax| 1 - self[ax])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Tests NdTreeBranchIndex::to_array_idx() and ::from_array_idx().
    #[test]
    fn test_branch_idx() {
        let arr: [ByteVec3D; 8] = [
            NdVec([0, 0, 0]),
            NdVec([1, 0, 0]),
            NdVec([0, 1, 0]),
            NdVec([1, 1, 0]),
            NdVec([0, 0, 1]),
            NdVec([1, 0, 1]),
            NdVec([0, 1, 1]),
            NdVec([1, 1, 1]),
        ];
        for (idx, vec) in arr.iter().enumerate() {
            assert_eq!(*vec, ByteVec::from_array_idx(idx));
            assert_eq!(idx, vec.to_array_idx());
        }
    }
}
