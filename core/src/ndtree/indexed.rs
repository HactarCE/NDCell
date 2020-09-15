//! "Indexed" ND-tree, to export/import MacroCell format or for passing to GLSL
//! in rendering.

use itertools::Itertools;
use std::collections::HashMap;
use std::marker::PhantomData;

use super::{CachedNodeRefTrait, Layer, NodeHasher, NodeRef, NodeRefTrait};
use crate::dim::Dim;

/// ND-tree represented as a list of nodes.
#[derive(Debug)]
pub struct IndexedNdTree<D: Dim, T> {
    /// Number of non-leaf layers.
    ///
    /// If all nodes are leaf nodes, this is 0.
    ///
    /// Note that the minimum layer in this IndexedNdTree might represent
    /// multiple cells, such as when rendering zoomed out past 1:1.
    layers: usize,
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
    /// The minimum is 0.
    pub fn layers(&self) -> usize {
        self.layers
    }
    /// Returns the list of nodes.
    pub fn nodes(&self) -> &[IndexedNdTreeNode<D, T>] {
        &self.nodes
    }
    /// Returns the list index of the root node.
    pub fn root_idx(&self) -> usize {
        self.root_idx
    }

    /// Constructs a `IndexedNdTree` from a `NodeRef`, indexing nodes down to the
    /// given layer. Use `min_layer = 0` to store the entire ND-tree.
    pub fn from_node<'cache>(
        node: NodeRef<'cache, D>,
        min_layer: Layer,
        node_to_data: impl Fn(NodeRef<'cache, D>) -> T,
    ) -> Self {
        assert!(node.layer() >= min_layer);
        IndexedNdTreeBuilder {
            min_layer,
            nodes: vec![],
            node_indices: HashMap::default(),
            node_to_data,
        }
        .build(node)
    }
    /// Converts this IndexedNdTree back into a Node.
    pub fn to_node<'s, 'cache>(
        &'s self,
        data_to_node: impl Fn(&'s T) -> NodeRef<'cache, D>,
    ) -> NodeRef<'cache, D> {
        self._to_node(&data_to_node, self.root_idx)
    }
    /// Converts a single root node (and its descendants) of this IndexedNdTree
    /// into a Node.
    fn _to_node<'s, 'cache>(
        &'s self,
        data_to_node: &impl Fn(&'s T) -> NodeRef<'cache, D>,
        root: usize,
    ) -> NodeRef<'cache, D> {
        match &self.nodes[root] {
            IndexedNdTreeNode::Leaf(data, _) => data_to_node(data),
            IndexedNdTreeNode::NonLeaf(indices, _) => {
                let mut children = indices
                    .iter()
                    .map(|&index| self._to_node(data_to_node, index))
                    .peekable();
                let node_cache = children.peek().unwrap().cache();
                node_cache.join_nodes(children)
            }
        }
    }
}

/// Builder for an `IndexedNdTree`.
struct IndexedNdTreeBuilder<'cache, D: Dim, T, F> {
    min_layer: Layer,
    nodes: Vec<IndexedNdTreeNode<D, T>>,
    node_indices: HashMap<NodeRef<'cache, D>, usize, NodeHasher>,
    node_to_data: F,
}
impl<'cache, D: Dim, T, F: Fn(NodeRef<'cache, D>) -> T> IndexedNdTreeBuilder<'cache, D, T, F> {
    /// Returns the final `IndexedNdTree`, using the given node as the root.
    pub fn build(mut self, node: NodeRef<'cache, D>) -> IndexedNdTree<D, T> {
        let root_idx = self.add_node(node);
        IndexedNdTree {
            layers: (node.layer() - self.min_layer).to_usize(),
            nodes: self.nodes,
            root_idx,
        }
    }
    /// Adds a node recursively to the `IndexedNdTree` (if it is not already
    /// present) and returns its index.
    ///
    /// Panics if `original_node` is layer 0 (single cell).
    fn add_node(&mut self, original_node: NodeRef<'cache, D>) -> usize {
        if let Some(&node_index) = self.node_indices.get(&original_node) {
            node_index
        } else {
            let indexed_node = if original_node.layer() <= self.min_layer {
                // This is the recursive base case for `min_layer >=
                // base_layer`.
                IndexedNdTreeNode::Leaf((self.node_to_data)(original_node), PhantomData)
            } else {
                // Subdivide this node into 2^NDIM pieces and recurse.
                let children = original_node
                    .subdivide()
                    .unwrap()
                    .into_iter()
                    .map(|child| self.add_node(child))
                    .collect_vec()
                    .into_boxed_slice();
                IndexedNdTreeNode::NonLeaf(children, PhantomData)
            };
            self.push_indexed_node(original_node, indexed_node)
        }
    }
    /// Appends an `IndexedNdTreeNode` to the `IndexedNdTree`.
    fn push_indexed_node(
        &mut self,
        original_node: NodeRef<'cache, D>,
        indexed_node: IndexedNdTreeNode<D, T>,
    ) -> usize {
        let new_index = self.nodes.len();
        self.node_indices.insert(original_node, new_index);
        self.nodes.push(indexed_node);
        new_index
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::automaton::Automaton2D;
    use crate::io::rle;

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
        let node_cache = automaton.tree.root.cache().read();
        let root = automaton.tree.root.as_ref(&node_cache);
        assert_eq!(Layer(3), root.layer());

        // The root node is at layer 3 (8x8), and there should be ...
        //  - three unique layer-2 nodes (4x4)
        //  - two unique layer-1 nodes (2x2)
        //  - two unique layer-0 nodes (individual cells)
        //
        // When we make the tree indexed down to layer N, we are indexing all
        // nodes of layer >= N, and leaving the rest as NdTreeNodes. The number
        // of indexed nodes is indexed_N.nodes.len().

        // 1+3+2+2 = 8, so there should be a total of eight nodes in this one.
        let indexed_0 = IndexedNdTree::from_node(root, Layer(0), |x| x);
        assert_eq!(3, indexed_0.layers);
        assert_eq!(8, indexed_0.nodes.len());
        assert_eq!(root, indexed_0.to_node(|&x| x));
        for n in indexed_0.nodes() {
            if let IndexedNdTreeNode::Leaf(x, _) = n {
                assert_eq!(Layer(0), x.layer());
            }
        }

        // 1+3+2 = 6, so there should be a total of six nodes in this one.
        let indexed_1 = IndexedNdTree::from_node(root, Layer(1), |x| x);
        assert_eq!(2, indexed_1.layers);
        assert_eq!(6, indexed_1.nodes.len());
        assert_eq!(root, indexed_1.to_node(|&x| x));
        for n in indexed_1.nodes() {
            if let IndexedNdTreeNode::Leaf(x, _) = n {
                assert_eq!(Layer(1), x.layer());
            }
        }

        // 1+3 = 4
        let indexed_2 = IndexedNdTree::from_node(root, Layer(2), |x| x);
        assert_eq!(1, indexed_2.layers);
        assert_eq!(4, indexed_2.nodes.len());
        assert_eq!(root, indexed_2.to_node(|&x| x));
        for n in indexed_2.nodes() {
            if let IndexedNdTreeNode::Leaf(x, _) = n {
                assert_eq!(Layer(2), x.layer());
            }
        }

        // 1 = 1
        let indexed_3 = IndexedNdTree::from_node(root, Layer(3), |x| x);
        assert_eq!(0, indexed_3.layers);
        assert_eq!(1, indexed_3.nodes.len());
        assert_eq!(root, indexed_3.to_node(|&x| x));
        for n in indexed_3.nodes() {
            if let IndexedNdTreeNode::Leaf(x, _) = n {
                assert_eq!(Layer(3), x.layer());
            }
        }
    }
}
