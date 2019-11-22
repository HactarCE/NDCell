use seahash::SeaHasher;
use std::hash::BuildHasherDefault;
use std::rc::{Rc, Weak};
use weak_table::WeakHashSet;

use crate::automaton::*;

type NodeHasher = BuildHasherDefault<SeaHasher>;

/// A cached NdTreeNode.
pub type NdCachedNode<C, D> = Rc<NdTreeNode<C, D>>;

/// A cache of NdTreeNodes.
#[derive(Debug, Clone, Default)]
pub struct NdTreeCache<C: CellType, D: Dim> {
    /// A HashSet of all of the nodes.
    nodes: WeakHashSet<Weak<NdTreeNode<C, D>>, NodeHasher>,
    phantom: PhantomData<D>,
}

impl<C: CellType, D: Dim> NdTreeCache<C, D> {
    /// Returns a new empty NdTreeNode cache.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the cached node with the given branches, creating it if it does
    /// not exist.
    pub fn get_node(&mut self, branches: Vec<NdTreeBranch<C, D>>) -> NdCachedNode<C, D> {
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
    pub fn get_empty_node(&mut self, layer: usize) -> NdCachedNode<C, D> {
        let branches = vec![self.get_empty_branch(layer - 1); NdTreeNode::<C, D>::BRANCHES];
        self.get_node(branches)
    }
    /// Returns the NdTreeBranch containing a node at the given layer with all
    /// default cells (or just an NdTreeBranch::Leaf of the default cell state).
    pub fn get_empty_branch(&mut self, layer: usize) -> NdTreeBranch<C, D> {
        match layer {
            0 => NdTreeBranch::Leaf(C::default()),
            _ => NdTreeBranch::Node(self.get_empty_node(layer)),
        }
    }
}
