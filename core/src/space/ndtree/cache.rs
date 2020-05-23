use dashmap::DashMap;
use seahash::SeaHasher;
use std::hash::BuildHasherDefault;
use std::sync::{Arc, RwLock};

use crate::*;

// TODO: use one node cache for all nodes with the same cell type.
// TODO: garbage-collect the hash set?

/// Fast hasher used for NdTreeNodes.
pub type NodeHasher = BuildHasherDefault<SeaHasher>;

/// A cached NdTreeNode.
pub type NdCachedNode<C, D> = Arc<NdTreeNode<C, D>>;

/// A cache of NdTreeNodes.
#[derive(Debug, Default)]
pub struct NdTreeCache<C: CellType, D: Dim> {
    /// A HashSet of all of the nodes.
    nodes: DashMap<NdBaseTreeNode<C, D>, NdCachedNode<C, D>, NodeHasher>,
    /// A cache of empty nodes at various layers.
    ///
    /// The element at index N is the empty node at layer N-1.
    empty_nodes: RwLock<Vec<NdCachedNode<C, D>>>,
}

impl<C: CellType, D: Dim> NdTreeCache<C, D> {
    /// Returns a new empty NdTreeNode cache.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the cached node with the given branches, creating it if it does
    /// not exist.
    pub fn get_node(&self, branches: Vec<NdTreeBranch<C, D>>) -> NdCachedNode<C, D> {
        // Create an NdBaseTreeNode (cheaper than a full NdTreeNode) for HashSet
        // lookup.
        let base_node = NdBaseTreeNode::from(branches);
        // If the node is already in the cache, return the one from the cache.
        if let Some(existing_node) = self.nodes.get(&base_node) {
            return existing_node.value().clone();
        }
        // Otherwise, make it a full node, add it to the cache, and return it.
        self.nodes
            .entry(base_node.clone())
            .or_insert(Arc::new(NdTreeNode::from(base_node)))
            .value()
            .clone()
    }
    /// Returns the NdTreeNode at the given layer with all default cells.
    pub fn get_empty_node(&self, layer: usize) -> NdCachedNode<C, D> {
        let empty_nodes = self.empty_nodes.read().unwrap();
        if let Some(ret) = empty_nodes.get(layer - 1) {
            // Cache hit
            ret.clone()
        } else {
            drop(empty_nodes);
            // Cache miss
            let branches = vec![self.get_empty_branch(layer - 1); D::TREE_BRANCHES];
            let ret = self.get_node(branches);
            // All lower entries in the cache have been filled by the recursive
            // call. Check again here to make sure that another thread hasn't
            // filled in this layer before us.
            let mut empty_nodes = self.empty_nodes.write().unwrap();
            if empty_nodes.len() < layer {
                empty_nodes.push(ret.clone())
            }
            ret
        }
    }
    /// Returns the NdTreeBranch containing a node at the given layer with all
    /// default cells (or just an NdTreeBranch::Leaf of the default cell state).
    pub fn get_empty_branch(&self, layer: usize) -> NdTreeBranch<C, D> {
        match layer {
            0 => NdTreeBranch::Leaf(C::default()),
            _ => NdTreeBranch::Node(self.get_empty_node(layer)),
        }
    }
    /// Returns a cached node, using a function to generate each branch.
    pub fn get_node_from_fn(
        &self,
        generator: impl FnMut(ByteVec<D>) -> NdTreeBranch<C, D>,
    ) -> NdCachedNode<C, D> {
        let branches = (0..D::TREE_BRANCHES)
            .map(ByteVec::from_array_idx)
            .map(generator)
            .collect();
        self.get_node(branches)
    }
    /// Returns a cached node, using a function of the cell position to generate
    /// each cell state. This can only be used for relatively small nodes, since
    /// an IVec is used for the position vector.
    pub fn get_small_node_from_cell_fn(
        &self,
        layer: usize,
        offset: IVec<D>,
        generator: &mut impl FnMut(IVec<D>) -> C,
    ) -> NdCachedNode<C, D> {
        self.get_node_from_fn(|branch_idx| {
            let branch_offset: IVec<D> = branch_idx.branch_offset(layer);
            let total_offset = &offset + branch_offset;
            if layer == 1 {
                NdTreeBranch::Leaf(generator(total_offset))
            } else {
                NdTreeBranch::Node(self.get_small_node_from_cell_fn(
                    layer - 1,
                    total_offset,
                    generator,
                ))
            }
        })
    }
}
