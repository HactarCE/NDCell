use seahash::SeaHasher;
use std::hash::BuildHasherDefault;
use std::rc::{Rc, Weak};
use weak_table::WeakHashSet;

use crate::automaton::*;

type NodeHasher = BuildHasherDefault<SeaHasher>;

/// A cached NdTreeNode.
pub type NdCachedNode<T, D> = Rc<NdTreeNode<T, D>>;

/// A cache of NdTreeNodes.
#[derive(Debug, Clone, Default)]
pub struct NdTreeCache<T: CellType, D: Dim> {
    /// A HashSet of all of the nodes.
    nodes: WeakHashSet<Weak<NdTreeNode<T, D>>, NodeHasher>,
    phantom: PhantomData<D>,
}

impl<T: CellType, D: Dim> NdTreeCache<T, D> {
    /// Returns a new empty NdTreeNode cache.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the cached node with the given branches, creating it if it does
    /// not exist.
    pub fn get_node(&mut self, branches: Vec<NdTreeBranch<T, D>>) -> NdCachedNode<T, D> {
        // Create an NdBaseTreeNode (cheaper than a full NdTreeNode) for HashSet
        // lookup.
        let base_node = NdBaseTreeNode::from(branches);
        // If the node is already in the cache, return the one from the cache.
        if let Some(existing_node) = self.nodes.get(&base_node) {
            return existing_node;
        }
        // Otherwise, make it a full node, add it to the cache, and return it.
        let ret = Rc::new(NdTreeNode::from(base_node));
        self.nodes.insert(ret.clone());
        ret
    }
    /// Returns the NdTreeNode at the given layer with all default cells.
    pub fn get_empty_node(&mut self, layer: usize) -> NdCachedNode<T, D> {
        let branches = vec![self.get_empty_branch(layer - 1); NdTreeNode::<T, D>::BRANCHES];
        self.get_node(branches)
    }
    /// Returns the NdTreeBranch containing a node at the given layer with all
    /// default cells (or just an NdTreeBranch::Leaf of the default cell state).
    pub fn get_empty_branch(&mut self, layer: usize) -> NdTreeBranch<T, D> {
        match layer {
            0 => NdTreeBranch::Leaf(T::default()),
            _ => NdTreeBranch::Node(self.get_empty_node(layer)),
        }
    }
}
