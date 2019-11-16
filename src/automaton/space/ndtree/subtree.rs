use seahash::SeaHasher;
use std::hash::Hasher;
use std::marker::PhantomData;
use std::rc::Rc;

use super::*;

/// An interned NdTreeNode.
pub type NdSubTree<T, D> = Rc<NdTreeNode<T, D>>;

/// An NdTreeNode's child.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NdTreeChild<T: CellType, D: Dim> {
    /// All cells within this node are the same cell state.
    Leaf(T),

    /// A 2^d hypercube of interned subnodes.
    ///
    /// Until rust-lang #44580 (RFC 2000) is resolved, there's no way to use
    /// D::NDIM as the array size. It might be worth implementing a custom
    /// unsafe type for this, but at the time of writing such an optimization
    /// would be entirely premature.
    Branch(Vec<NdSubTree<T, D>>),
}

impl<T: CellType, D: Dim> Default for NdTreeChild<T, D> {
    fn default() -> Self {
        Self::Leaf(T::default())
    }
}

/// A single node in the NdTree, which contains information about its layer
/// (base-2 logarithm of hypercube side length) and its children.
#[derive(Debug, Clone)]
// TODO: custom Debug implementation
pub struct NdTreeNode<T: CellType, D: Dim> {
    /// The "layer" of this node (base-2 logarithm of hypercube side length).
    layer: usize,

    /// The child of this node, which is either a single cell state or a 2^d
    /// hypercube of nodes.
    ///
    /// If layer == 0, then it must be a single cell state.
    child: NdTreeChild<T, D>,

    hash_code: u64,

    phantom: PhantomData<D>,
}

impl<T: CellType, D: Dim> NdTreeNode<T, D> {
    /// Constructs a new empty NdTreeNode at a given layer.
    pub fn empty(layer: usize) -> Self {
        Self::with_child(layer, NdTreeChild::default())
    }
    /// Constructs a new NdTreeNode at a given layer and with a given child.
    pub fn with_child(layer: usize, child: NdTreeChild<T, D>) -> Self {
        let mut hasher = SeaHasher::new();
        layer.hash(&mut hasher);
        child.hash(&mut hasher);
        let ret = Self {
            layer,
            child,
            hash_code: hasher.finish(),
            phantom: PhantomData,
        };
        ret
    }
    /// Checks whether an equivalent node is present in the cache. If it is,
    /// destroys this one and returns the equivalent node from the cache; if
    /// not, adds this node to the cache and returns it.
    ///
    /// Also simplifies this node: if all branches are identical leaves, sets
    /// the child to a single leaf.
    pub fn intern(mut self, cache: &mut NdTreeCache<T, D>) -> NdSubTree<T, D> {
        // If this branch splits ...
        if let NdTreeChild::Branch(branches) = &self.child {
            // ... and the first split is a leaf ...
            if let NdTreeChild::Leaf(_) = branches[0].child {
                // ... and all the other splits are equivalent leaves ...
                if branches
                    .iter()
                    .skip(1)
                    .all(|branch| branch.child == branches[0].child)
                {
                    // ... then set this node's child to that same leaf.
                    self.child = branches[0].child.clone();
                }
            }
        }
        // Now actually construct the Rc and add it to the cache.
        cache.get(&self).clone().unwrap_or_else(|| {
            let ret = Rc::new(self);
            cache.insert(ret.clone());
            ret
        })
    }

    pub fn layer(&self) -> usize {
        self.layer
    }
    pub fn child(&self) -> &NdTreeChild<T, D> {
        &self.child
    }
    pub fn hash_code(&self) -> u64 {
        self.hash_code
    }

    /// Returns the length of a single side of the hypersquare contained in this
    /// subtree.
    pub fn len(&self) -> usize {
        // layer = 0 => len = 1
        // layer = 1 => len = 2
        // layer = 2 => len = 4
        // etc.
        1 << self.layer
    }

    /// The number of branches for this many dimensions (2^d).
    pub const BRANCHES: usize = 1 << D::NDIM;
    /// The bitmask for branch indices.
    const BRANCH_IDX_BITMASK: usize = Self::BRANCHES - 1;
    /// Computes this node's "branch index" for the given position.
    ///
    /// Each nth layer corresponds to the nth bit of each axis, which can either
    /// be 0 or 1. The "branch index" is a number between 0 and 2^d-1 composed
    /// from these bits; each bit in the branch index is taken from a different
    /// axis. It's like a bitwise NdVec.
    fn branch_idx(&self, pos: NdVec<D>) -> usize {
        let mut ret = 0;
        for ax in D::axes() {
            ret <<= 1;
            ret |= (pos[ax] as usize >> self.layer) & 1;
        }
        ret
    }

    /// "Zooms out" of the current tree; returns a new NdSubTree with the
    /// contents of the existing one centered in an empty grid.
    pub fn expand_centered(&self, cache: &mut NdTreeCache<T, D>) -> NdSubTree<T, D> {
        NdTreeNode::with_child(
            self.layer + 1,
            NdTreeChild::Branch({
                let mut new_branches = Vec::with_capacity(Self::BRANCHES);
                for branch_idx in 0..Self::BRANCHES {
                    new_branches[branch_idx] = match &self.child {
                        NdTreeChild::Leaf(cell_state) => {
                            NdTreeNode::with_child(self.layer, NdTreeChild::Leaf(*cell_state))
                                .intern(cache)
                        }
                        NdTreeChild::Branch(old_branches) => {
                            old_branches[branch_idx ^ Self::BRANCH_IDX_BITMASK].clone()
                        }
                    }
                }
                new_branches
            }),
        )
        .intern(cache)
    }

    fn split_child(&self, cache: &mut NdTreeCache<T, D>) -> Vec<NdSubTree<T, D>> {
        if self.layer == 0 {
            panic!("Cannot split NdTreeNode child at layer 0");
        }
        match &self.child {
            NdTreeChild::Leaf(_) => vec![
                Self::with_child(self.layer - 1, self.child.clone())
                    .intern(cache);
                Self::BRANCHES
            ],
            NdTreeChild::Branch(branches) => branches.clone(),
        }
    }

    pub fn get_cell(&self, pos: NdVec<D>) -> T {
        match &self.child {
            NdTreeChild::Leaf(cell_state) => *cell_state,
            NdTreeChild::Branch(branches) => branches[self.branch_idx(pos)].get_cell(pos),
        }
    }

    pub fn set_cell(
        &self,
        cache: &mut NdTreeCache<T, D>,
        pos: NdVec<D>,
        cell_state: T,
    ) -> NdSubTree<T, D> {
        if self.layer == 0 {
            Self::with_child(0, NdTreeChild::Leaf(cell_state)).intern(cache)
        } else {
            let branches = self.split_child(cache);
            branches[self.branch_idx(pos)].set_cell(cache, pos, cell_state)
        }
    }
}
