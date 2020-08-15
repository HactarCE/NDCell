//! "Indexed" NdTrees, to export/import MacroCell format or for passing to GLSL
//! in rendering.

use itertools::Itertools;
use std::collections::HashMap;
use std::marker::PhantomData;

use super::*;
use crate::Dim;

/// NdTree represented as a list of nodes.
#[derive(Debug)]
pub struct IndexedNdTree<D: Dim, T> {
    /// Layer of the largest node, in the context of this `IndexedNdTree`.
    ///
    /// Note that the minimum layer in this IndexedNdTree might represent
    /// multiple cells, such as when rendering zoomed out past 1:1.
    ///
    /// TODO: rewrite this comment more clearly
    max_layer: u32,
    /// List of all the nodes.
    nodes: Vec<IndexedNdTreeNode<D, T>>,
    /// Index of the root node.
    root_idx: usize,
}

/// Node in an `IndexedNdTree`.
#[derive(Debug)]
pub enum IndexedNdTreeNode<D: Dim, T> {
    /// Leaf node.
    Leaf(T, PhantomData<D>),
    /// Non-leaf node, containing "pointers" (array indices) to other nodes.
    NonLeaf(Box<[usize]>, PhantomData<D>),
}

impl<D: Dim, T> IndexedNdTree<D, T> {
    /// Returns the number of indexed layers; i.e. how many times you have to
    /// follow a pointer (including getting the initial node) to reach a leaf.
    /// The minimum is 1.
    pub fn max_layer(&self) -> u32 {
        self.max_layer
    }
    /// Returns the list of nodes.
    pub fn nodes(&self) -> &[IndexedNdTreeNode<D, T>] {
        &self.nodes
    }
    /// Returns the list index of the root node.
    pub fn root_idx(&self) -> usize {
        self.root_idx
    }

    /// Constructs a IndexedNdTree from a NodeRef, indexing nodes down to the
    /// given layer. Use `min_layer = 0` to store the entire NdTree.
    pub fn from_node<'a>(
        node_cache: &'a NodeCacheAccess<'a, D>,
        node: NodeRef<'a, D>,
        min_layer: u32,
        node_to_data: impl Fn(NodeRef<'a, D>) -> T,
    ) -> Self {
        assert!(node.layer() >= min_layer);
        IndexedNdTreeBuilder {
            node_cache,
            min_layer,
            nodes: vec![],
            indexed_nodes: HashMap::default(),
            node_to_data,
        }
        .build(node)
    }
    /// Converts this IndexedNdTree back into a NodeRef.
    pub fn to_node<'a>(
        &self,
        node_cache: &'a NodeCacheAccess<'a, D>,
        data_to_node: impl Fn(&T) -> NodeRef<'a, D>,
    ) -> NodeRef<'a, D> {
        self._to_node(node_cache, data_to_node, self.root_idx)
    }
    /// Converts a single root node (and its descendants) of this IndexedNdTree
    /// into a NodeRef.
    fn _to_node<'a>(
        &self,
        node_cache: &'a NodeCacheAccess<D>,
        data_to_node: impl Fn(&T) -> NodeRef<'a, D>,
        root: usize,
    ) -> NodeRef<'a, D> {
        todo!("Test this");
        match self.nodes[root] {
            IndexedNdTreeNode::Leaf(data, _) => data_to_node(&data),
            IndexedNdTreeNode::NonLeaf(indices, _) => node_cache.join_nodes(
                indices
                    .iter()
                    .map(|&index| self._to_node(node_cache, &data_to_node, index).as_raw())
                    .collect_vec(),
            ),
        }
    }
}

/// Builder for an `IndexedNdTree`.
struct IndexedNdTreeBuilder<'a, D: Dim, T, F> {
    node_cache: &'a NodeCacheAccess<'a, D>,
    min_layer: u32,
    nodes: Vec<IndexedNdTreeNode<D, T>>,
    indexed_nodes: HashMap<NodeRef<'a, D>, usize, NodeHasher>,
    node_to_data: F,
}
impl<'a, D: Dim, T, F: Fn(NodeRef<'a, D>) -> T> IndexedNdTreeBuilder<'a, D, T, F> {
    /// Returns the final `IndexedNdTree`, using the given node as the root.
    pub fn build(mut self, node: NodeRef<'a, D>) -> IndexedNdTree<D, T> {
        let root_idx = self.add_node(node);
        IndexedNdTree {
            max_layer: node.layer() - self.min_layer,
            nodes: self.nodes,
            root_idx,
        }
    }
    /// Adds a node recursively to the `IndexedNdTree` (if it is not already
    /// present) and returns its index.
    ///
    /// Panics if `original_node` is layer 0 (single cell).
    fn add_node(&mut self, original_node: NodeRef<'a, D>) -> usize {
        if let Some(&node_index) = self.indexed_nodes.get(&original_node) {
            node_index
        } else if original_node.layer() <= self.min_layer + 1 {
            // We compare against min_layer+1 because min_layer refers to the
            // minimum layer for *node children*, not *nodes*. For example,
            // min_layer=0 means the smallest node should be 2x2 (layer=1),
            // which contains 2^NDIM cells (layer=0). This is the recursive base
            // case for min_layer >= base_layer.
            self.push_indexed_node(
                original_node,
                IndexedNdTreeNode::Leaf((self.node_to_data)(original_node), PhantomData),
            )
        } else {
            // Subdivide this node into 2^NDIM pieces and recurse.
            let children = self
                .node_cache
                .subdivide(original_node)
                .unwrap()
                .into_iter()
                .map(|child| self.add_node(child))
                .collect_vec()
                .into_boxed_slice();
            self.push_indexed_node(
                original_node,
                IndexedNdTreeNode::NonLeaf(children, PhantomData),
            )
        }
    }
    /// Appends an `IndexedNdTreeNode` to the `IndexedNdTree`.
    fn push_indexed_node(
        &mut self,
        original_node: NodeRef<'a, D>,
        indexed_node: IndexedNdTreeNode<D, T>,
    ) -> usize {
        let new_index = self.nodes.len();
        self.indexed_nodes.insert(original_node, new_index);
        self.nodes.push(indexed_node);
        new_index
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{rle, Automaton2D};

    // TODO: write tests that include different `min_layer`s and `base_layer`s

    /// Thoroughly test a single "indexed" quadtree.
    #[test]
    fn test_indexed_ndtree() {
        // Load this pattern:
        //
        // - - - - x - - -
        // - - - - - x - -
        // - - - - x - x -
        // - - - - - x - x
        // x - - - - - x -
        // - x - - - - - x
        // x - x - - - - -
        // - x - x - - - -
        let automaton: Automaton2D = rle::RleEncode::from_rle(
            "
#CXRLE Pos=-4,-3
x = 8, y = 8, rule = B3/S23
4bo$5bo$4bobo$5bobo$o5bo$bo5bo$obo$bobo!
",
        )
        .unwrap();
        let node_access = automaton.tree.node_access();
        let node = automaton.tree.root.as_ref(&node_access);
        assert_eq!(3, node.layer());

        // The root node is at layer 3 (8x8), and there should be ...
        //  - three unique layer-2 nodes (4x4)
        //  - two unique layer-1 nodes (2x2)
        //  - two unique layer-0 nodes (individual cells)
        //
        // When we make the tree indexed down to layer N, we are indexing all
        // nodes of layer >= N, and leaving the rest as NdTreeNodes. The number
        // of indexed nodes is indexed_N.nodes.len().

        let indexed_0 = IndexedNdTree::from_node(&node_access, node, 0, |x| x);
        assert_eq!(3, indexed_0.max_layer);
        // 1+3+2+2 = 6, so there should be a total of eight nodes in this one.
        assert_eq!(6, indexed_0.nodes.len());
        assert_eq!(node, indexed_0.to_node(&node_access, |&x| x));

        let indexed_1 = IndexedNdTree::from_node(&node_access, node, 1, |x| x);
        assert_eq!(2, indexed_1.max_layer);
        // 1+3+2 = 6, so there should be a total of eight nodes in this one.
        assert_eq!(4, indexed_1.nodes.len());
        assert_eq!(node, indexed_1.to_node(&node_access, |&x| x));

        let indexed_2 = IndexedNdTree::from_node(&node_access, node, 2, |x| x);
        assert_eq!(1, indexed_2.max_layer);
        // 1+3 = 4
        assert_eq!(1, indexed_2.nodes.len());
        assert_eq!(node, indexed_2.to_node(&node_access, |&x| x));

        let indexed_3 = IndexedNdTree::from_node(&node_access, node, 3, |x| x);
        assert_eq!(3, indexed_3.max_layer);
        // 1 = 1
        assert_eq!(6, indexed_3.nodes.len());
        assert_eq!(node, indexed_3.to_node(&node_access, |&x| x));
    }
}
