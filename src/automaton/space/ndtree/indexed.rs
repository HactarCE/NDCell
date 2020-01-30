//! Code for producing "indexed" NdTrees, to export as MacroCell or for passing
//! to GLSL in rendering.

use std::collections::HashMap;

use super::*;

/// An NdTree represented as an list of nodes.
#[derive(Debug)]
pub struct IndexedNdTree<C: CellType, D: Dim> {
    layers: usize,
    nodes: Vec<Vec<IndexedNdTreeBranch<C, D>>>,
    root_idx: usize,
}

/// A branch of a node in a IndexedNdTree.
#[derive(Debug)]
pub enum IndexedNdTreeBranch<C: CellType, D: Dim> {
    /// The last "indexed" node in a IndexedNdTree; can be either an NdTreeNode
    /// or a single cell.
    Leaf(NdTreeBranch<C, D>),
    /// A pointer to another node in the list of nodes.
    Pointer(usize),
}

impl<C: CellType, D: Dim> IndexedNdTree<C, D> {
    /// Gets the number of indexed layers; i.e. how many times you have to
    /// follow a pointer (including getting the initial node) to reach a leaf.
    /// The minimum is 1.
    pub fn get_layer_count(&self) -> usize {
        self.layers
    }
    /// Gets the list of nodes.
    pub fn get_nodes(&self) -> &[Vec<IndexedNdTreeBranch<C, D>>] {
        &self.nodes
    }
    /// Gets the list index of the root node.
    pub fn get_root_idx(&self) -> usize {
        self.root_idx
    }

    /// Constructs a IndexedNdTree from an NdCachedNode, indexing nodes down to
    /// the given layer. Use `min_layer = 0` to store the entire NdTree.
    pub fn from_node(node: &NdCachedNode<C, D>, min_layer: usize) -> Self {
        IndexedNdTreeInProgress {
            min_layer,
            nodes: vec![],
            cache: HashMap::default(),
        }
        .complete(node)
    }
    /// Converts this IndexedNdTree back into an NdCachedNode.
    pub fn to_node(&self, cache: &mut NdTreeCache<C, D>) -> NdCachedNode<C, D> {
        self.partial_to_node(cache, self.root_idx)
    }
    /// Converts a single node (and its descendants) of this IndexedNdTree into an
    /// NdCachedNode.
    fn partial_to_node(
        &self,
        cache: &mut NdTreeCache<C, D>,
        start_idx: usize,
    ) -> NdCachedNode<C, D> {
        let mut branch_iter = self.nodes[start_idx].iter();
        cache.get_node_from_fn(|cache, _| match branch_iter.next().unwrap() {
            IndexedNdTreeBranch::Leaf(branch) => branch.clone(),
            IndexedNdTreeBranch::Pointer(idx) => {
                NdTreeBranch::Node(self.partial_to_node(cache, *idx))
            }
        })
    }
}

/// A temporary struct used to create a IndexedNdTree.
struct IndexedNdTreeInProgress<'a, C: CellType, D: Dim> {
    min_layer: usize,
    nodes: Vec<Vec<IndexedNdTreeBranch<C, D>>>,
    cache: HashMap<&'a NdCachedNode<C, D>, usize, NodeHasher>,
}
impl<'a, C: CellType, D: Dim> IndexedNdTreeInProgress<'a, C, D> {
    /// Adds a node recursively to the indexed NdTree if it is not already
    /// present and returns its index.
    fn add_node(&mut self, node: &'a NdCachedNode<C, D>) -> usize {
        if let Some(&node_index) = self.cache.get(node) {
            node_index
        } else {
            let indexed_branches;
            indexed_branches = node
                .branches
                .iter()
                .map(|branch| {
                    if branch.get_layer() <= self.min_layer {
                        IndexedNdTreeBranch::Leaf(branch.clone())
                    } else {
                        IndexedNdTreeBranch::Pointer(self.add_node(branch.node().unwrap()))
                    }
                })
                .collect();
            let node_index = self.nodes.len();
            self.nodes.push(indexed_branches);
            self.cache.insert(node, node_index);
            node_index
        }
    }
    /// Returns the final IndexedNdTree, using the given node as the root.
    pub fn complete(mut self, node: &'a NdCachedNode<C, D>) -> IndexedNdTree<C, D> {
        let root_idx = self.add_node(node);
        IndexedNdTree {
            layers: node.layer - self.min_layer,
            nodes: self.nodes,
            root_idx,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::automaton::{rle, Automaton2D};

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
        let node = automaton.tree.slice.root;
        assert_eq!(3, node.layer);
        let mut cache = automaton.tree.cache.borrow_mut();

        // The root node is of layer 3 (8x8), and there should be ...
        //  - three unique layer-2 nodes (4x4)
        //  - two unique layer-1 nodes (2x2)
        //  - two unique "layer-0" nodes (individual cells), but these don't
        //    have their own entries in the indexed tree
        //
        // When we make the tree indexed down to layer N, we are indexing all
        // nodes of layer >= N, and leaving the rest as NdTreeNodes. The number
        // of indexed nodes is indexed_N.nodes.len().

        let indexed_0 = IndexedNdTree::from_node(&node, 0);
        assert_eq!(3, indexed_0.layers);
        // 1+3+2 = 6, so there should be a total of eight nodes in this one.
        assert_eq!(6, indexed_0.nodes.len());
        assert_eq!(node, indexed_0.to_node(&mut cache));

        let indexed_1 = IndexedNdTree::from_node(&node, 1);
        assert_eq!(2, indexed_1.layers);
        // 1+3 = 4
        assert_eq!(4, indexed_1.nodes.len());
        assert_eq!(node, indexed_1.to_node(&mut cache));

        let indexed_2 = IndexedNdTree::from_node(&node, 2);
        assert_eq!(1, indexed_2.layers);
        // 1 = 1
        assert_eq!(1, indexed_2.nodes.len());
        assert_eq!(node, indexed_2.to_node(&mut cache));
    }
}
