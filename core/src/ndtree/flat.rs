//! Flattened representation of an ND-tree, to export/import MacroCell format or
//! for passing to GLSL in rendering.

use itertools::Itertools;
use std::marker::PhantomData;

use super::{Layer, NodeRef, NodeRefTrait};
use crate::dim::Dim;
use crate::HashMap;

/// ND-tree represented as a list of nodes.
#[derive(Debug)]
pub struct FlatNdTree<D: Dim, T> {
    /// Number of non-leaf layers.
    ///
    /// If all nodes are leaf nodes, this is 0.
    ///
    /// Note that the minimum layer in this ND-tree might represent multiple
    /// cells, such as when rendering zoomed out past 1:1.
    layers: usize,
    /// List of all the nodes.
    nodes: Vec<FlatNdTreeNode<D, T>>,
    /// Index of the root node.
    root_idx: usize,
}

/// Node in a `FlatNdTree`.
#[derive(Debug)]
pub enum FlatNdTreeNode<D: Dim, T> {
    /// Leaf node.
    Leaf(T, PhantomData<D>),
    /// Non-leaf node, containing "pointers" (array indices) to other nodes.
    NonLeaf(Box<[usize]>, PhantomData<D>),
}

impl<D: Dim, T> FlatNdTree<D, T> {
    /// Returns the number of flat layers; i.e., how many times you have to
    /// follow a pointer (including getting the initial node) to reach a leaf.
    /// The minimum is 0.
    pub fn layers(&self) -> usize {
        self.layers
    }
    /// Returns the list of nodes.
    pub fn nodes(&self) -> &[FlatNdTreeNode<D, T>] {
        &self.nodes
    }
    /// Returns the list index of the root node.
    pub fn root_idx(&self) -> usize {
        self.root_idx
    }

    /// Constructs a `FlatNdTree` from a `NodeRef`, flattening nodes down to the
    /// given layer. Use `min_layer = 0` to store the entire ND-tree.
    pub fn from_node<'pool>(
        node: impl NodeRefTrait<'pool, D = D>,
        min_layer: Layer,
        node_to_data: impl Fn(NodeRef<'pool, D>) -> T,
    ) -> Self {
        assert!(node.layer() >= min_layer);
        FlatNdTreeBuilder {
            min_layer,
            nodes: vec![
                // Index 0 is always the unique empty node.
                FlatNdTreeNode::NonLeaf(
                    vec![0; D::BRANCHING_FACTOR].into_boxed_slice(),
                    PhantomData,
                ),
            ],
            node_indices: HashMap::default(),
            node_to_data,
        }
        .build(node.as_ref())
    }
    /// Converts this `FlatNdTree` back into a `NodeRef`. `data_to_node` must
    /// return an empty node at `min_layer` when passed `None`.
    pub fn to_node<'s, 'pool>(
        &'s self,
        data_to_node: &'s impl Fn(Option<&'s T>) -> NodeRef<'pool, D>,
    ) -> NodeRef<'pool, D> {
        self._to_node(data_to_node, self.root_idx, self.layers)
    }
    /// Converts a single root node (and its descendants) of this `FlatNdTree`
    /// into a `NodeRef`.
    fn _to_node<'s, 'pool>(
        &'s self,
        data_to_node: &'s impl Fn(Option<&'s T>) -> NodeRef<'pool, D>,
        root: usize,
        layers_remaining: usize,
    ) -> NodeRef<'pool, D> {
        // Handle empty nodes specially.
        if root == 0 {
            let empty_node_at_min_layer = data_to_node(None);
            assert!(empty_node_at_min_layer.is_empty());
            if layers_remaining == 0 {
                return empty_node_at_min_layer;
            } else {
                let layer = empty_node_at_min_layer.layer() + Layer(layers_remaining as u32);
                return empty_node_at_min_layer.pool().get_empty(layer);
            }
        }

        match &self.nodes[root] {
            FlatNdTreeNode::Leaf(data, _) => data_to_node(Some(data)),
            FlatNdTreeNode::NonLeaf(indices, _) => {
                let mut children = indices
                    .iter()
                    .map(|&index| self._to_node(data_to_node, index, layers_remaining - 1))
                    .peekable();
                let node_pool = children.peek().unwrap().pool();
                node_pool.join_nodes(children)
            }
        }
    }
}

/// Builder for a `FlatNdTree`.
struct FlatNdTreeBuilder<'pool, D: Dim, T, F> {
    min_layer: Layer,
    nodes: Vec<FlatNdTreeNode<D, T>>,
    node_indices: HashMap<NodeRef<'pool, D>, usize>,
    node_to_data: F,
}
impl<'pool, D: Dim, T, F: Fn(NodeRef<'pool, D>) -> T> FlatNdTreeBuilder<'pool, D, T, F> {
    /// Returns the final `FlatNdTree`, using the given node as the root.
    pub fn build(mut self, node: NodeRef<'pool, D>) -> FlatNdTree<D, T> {
        let root_idx = self.add_node(node);
        FlatNdTree {
            layers: (node.layer() - self.min_layer).to_usize(),
            nodes: self.nodes,
            root_idx,
        }
    }
    /// Adds a node recursively to the `FlatNdTree` (if it is not already
    /// present) and returns its index.
    ///
    /// Panics if `original_node` is layer 0 (single cell).
    fn add_node(&mut self, original_node: NodeRef<'pool, D>) -> usize {
        if original_node.is_empty() {
            0 // Index 0 contains an empty node for all layers.
        } else if let Some(&node_index) = self.node_indices.get(&original_node) {
            node_index
        } else {
            let flat_node = if original_node.layer() <= self.min_layer {
                // This is the recursive base case for `min_layer >=
                // base_layer`.
                FlatNdTreeNode::Leaf((self.node_to_data)(original_node), PhantomData)
            } else {
                // Subdivide this node into 2^NDIM pieces and recurse.
                let children = original_node
                    .subdivide()
                    .unwrap()
                    .into_iter()
                    .map(|child| self.add_node(child))
                    .collect_vec()
                    .into_boxed_slice();
                FlatNdTreeNode::NonLeaf(children, PhantomData)
            };
            self.push_flat_node(original_node, flat_node)
        }
    }
    /// Appends a `FlatNdTreeNode` to the `FlatNdTree`.
    fn push_flat_node(
        &mut self,
        original_node: NodeRef<'pool, D>,
        flat_node: FlatNdTreeNode<D, T>,
    ) -> usize {
        let new_index = self.nodes.len();
        self.node_indices.insert(original_node, new_index);
        self.nodes.push(flat_node);
        new_index
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::{CaFormatTrait, Rle};
    use crate::ndtree::NdTree2D;

    /// Thoroughly test a single "flat" quadtree.
    #[test]
    fn test_flat_ndtree() {
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
        let ndtree: NdTree2D = Rle::from_string_to_ndtree(
            "
            #CXRLE Pos=-4,-3
            x = 8, y = 8, rule = B3/S23
            4bo$5bo$4bobo$5bobo$o5bo$bo5bo$obo$bobo!
            ",
        )
        .expect("Failed to import RLE");
        let root = ndtree.root_ref();
        assert_eq!(Layer(3), root.layer());

        let empty_0 = root.pool().get_empty(Layer(0));
        let empty_1 = root.pool().get_empty(Layer(1));
        let empty_2 = root.pool().get_empty(Layer(2));
        let empty_3 = root.pool().get_empty(Layer(3));
        let empty_10 = root.pool().get_empty(Layer(10));

        // The root node is at layer 3 (8x8), and there should be ...
        //  - one empty node, shared by all layers
        //  - two unique non-empty layer-2 nodes (4x4)
        //  - one unique non-empty layer-1 nodes (2x2)
        //  - one unique non-empty layer-0 nodes (individual cells)
        //
        // When we make the tree flat down to layer N, we are flattening all
        // nodes of layer >= N, and leaving the rest as `NdTreeNodes`. The
        // number of flat nodes is `flat_N.nodes.len()`.

        // 1+1+2+1+1 = 6, so there should be a total of six nodes in this one.
        let flat_0 = FlatNdTree::from_node(&root, Layer(0), |x| x);
        assert_eq!(3, flat_0.layers());
        assert_eq!(6, flat_0.nodes().len());
        assert_eq!(root, flat_0.to_node(&|x| *x.unwrap_or(&empty_0)));
        for n in flat_0.nodes() {
            if let FlatNdTreeNode::Leaf(x, _) = n {
                assert_eq!(Layer(0), x.layer());
            }
        }

        // 1+1+2+1 = 5, so there should be a total of five nodes in this one.
        let flat_1 = FlatNdTree::from_node(&root, Layer(1), |x| x);
        assert_eq!(2, flat_1.layers());
        assert_eq!(5, flat_1.nodes().len());
        assert_eq!(root, flat_1.to_node(&|x| *x.unwrap_or(&empty_1)));
        for n in flat_1.nodes() {
            if let FlatNdTreeNode::Leaf(x, _) = n {
                assert_eq!(Layer(1), x.layer());
            }
        }

        // 1+1+2 = 4
        let flat_2 = FlatNdTree::from_node(&root, Layer(2), |x| x);
        assert_eq!(1, flat_2.layers());
        assert_eq!(4, flat_2.nodes().len());
        assert_eq!(root, flat_2.to_node(&|x| *x.unwrap_or(&empty_2)));
        for n in flat_2.nodes() {
            if let FlatNdTreeNode::Leaf(x, _) = n {
                assert_eq!(Layer(2), x.layer());
            }
        }

        // 1+1 = 2
        let flat_3 = FlatNdTree::from_node(&root, Layer(3), |x| x);
        assert_eq!(0, flat_3.layers());
        assert_eq!(2, flat_3.nodes().len());
        assert_eq!(root, flat_3.to_node(&|x| *x.unwrap_or(&empty_3)));
        for n in flat_3.nodes() {
            if let FlatNdTreeNode::Leaf(x, _) = n {
                assert_eq!(Layer(3), x.layer());
            }
        }

        // Test empty node.
        let flat = FlatNdTree::from_node(empty_10, Layer(0), |_| panic!());
        assert_eq!(10, flat.layers());
        assert_eq!(1, flat.nodes().len());
        assert_eq!(
            empty_10,
            flat.to_node(&|x| {
                assert_eq!(None, x);
                empty_0
            }),
        );
    }
}
