use itertools::Itertools;

use super::*;
use crate::axis::Axis;
use crate::dim::Dim;
use crate::ndrect::BigRect;
use crate::ndtree::{Layer, NdTree, NodeRef, NodeRefTrait, Region, SharedNodePool};
use crate::ndvec::{BigVec, BigVec6D};
use crate::num::{BigInt, Zero};
use crate::HashMap;

impl CaFormatTrait for Macrocell {
    fn rule(&self) -> Option<&str> {
        self.rule.as_deref()
    }
    fn with_rule(mut self, rule: Option<impl ToString>) -> Self {
        self.rule = rule.map(|r| r.to_string());
        self
    }

    fn generation(&self) -> BigInt {
        self.gen.clone()
    }
    fn with_generation(mut self, generation: BigInt) -> Self {
        self.gen = generation;
        self
    }

    fn comments(&self) -> &str {
        &self.comments
    }
    fn comments_mut(&mut self) -> &mut String {
        &mut self.comments
    }

    fn region<D: Dim>(&self) -> Region<D> {
        // Convert from 6D to ND.
        let center = BigVec::from_fn(|ax| self.offset[ax].clone());

        let size = match self.nodes.last() {
            None | Some(MacrocellNode::Empty) => return Region::Empty,
            Some(MacrocellNode::Leaf8x8 { .. }) => 8.into(),
            Some(MacrocellNode::NonLeaf { layer, .. }) => layer.big_len(),
        };

        Region::Rect(BigRect::centered(center, &(size / 2)))
    }

    fn to_ndtree<D: Dim>(&self, node_pool: SharedNodePool<D>) -> Result<NdTree<D>, Self::Err> {
        let node_pool_access = node_pool.access();
        let mut nodes: Vec<NodeRef<'_, D>> = vec![];

        for macrocell_node in &self.nodes {
            let new_node = match macrocell_node {
                // Put a dummy node at index 0 -- we'll handle index 0 specially
                // anyway.
                MacrocellNode::Empty => node_pool_access.get_empty_base(),

                MacrocellNode::Leaf8x8 { bits } => {
                    if D::NDIM != 2 {
                        return Err(MacrocellError::LeafNodeNon2D);
                    }
                    node_pool_access.get_from_cells(
                        (0..8)
                            // Reverse because Macrocell stores cells with
                            // *decreasing* Y values.
                            .rev()
                            .flat_map(|y| (0..8).map(move |x| (x, y)))
                            .map(|(x, y)| (bits[y] >> x) & 1)
                            .collect_vec(),
                    )
                }

                MacrocellNode::NonLeaf { layer, children } => {
                    if children.len() != D::BRANCHING_FACTOR {
                        return Err(MacrocellError::InvalidContent);
                    }
                    let children_iter = macrocell_index_iter::<D>().map(|i| children[i]);
                    match layer {
                        Layer(0) => return Err(MacrocellError::InvalidContent),
                        Layer(1) => {
                            // `as u8` will take the least-significant byte,
                            // which is fine here.
                            node_pool_access
                                .get_from_cells(children_iter.map(|i| i as u8).collect_vec())
                        }
                        Layer(_) => node_pool_access.join_nodes(
                            children_iter
                                .map(|child_index| match child_index {
                                    0 => Ok(node_pool_access.get_empty(layer.child_layer())),
                                    i => {
                                        let child = nodes
                                            .get(i)
                                            .copied()
                                            .ok_or(MacrocellError::InvalidContent)?;
                                        if child.layer() == layer.child_layer() {
                                            Ok(child)
                                        } else {
                                            Err(MacrocellError::InvalidContent)
                                        }
                                    }
                                })
                                .collect::<MacrocellResult<Vec<_>>>()?,
                        ),
                    }
                }
            };

            nodes.push(new_node);
        }

        let offset = BigVec::from_fn(|ax| self.offset[ax].clone());
        Ok(NdTree::from_node_centered_on(
            nodes
                .pop()
                .unwrap_or_else(|| node_pool_access.get_empty_base()),
            offset,
        ))
    }

    fn from_ndtree<D: Dim>(
        ndtree: &NdTree<D>,
        rect: Option<BigRect<D>>,
    ) -> Result<Self, Self::Err> {
        let mut offset = BigVec6D::origin();
        let center = ndtree.center_pos();
        for &ax in D::axes() {
            offset[ax] = center[ax].clone();
        }

        let mut nodes = vec![MacrocellNode::Empty];
        let ndtree = match rect {
            Some(r) => ndtree.get_region(Region::Rect(r)),
            None => ndtree.clone(),
        };
        add_macrocell_node(
            &mut nodes,
            &mut HashMap::default(),
            ndtree.root_ref().as_ref(),
        );

        Ok(Macrocell {
            rule: None,
            gen: BigInt::zero(),
            offset,
            nodes,
            comments: String::new(),
        })
    }
}

fn add_macrocell_node<'pool, D: Dim>(
    node_list: &mut Vec<MacrocellNode>,
    node_indices: &mut HashMap<NodeRef<'pool, D>, usize>,
    new_node: NodeRef<'pool, D>,
) -> usize {
    // If the node is empty, return sentinel 0.
    if new_node.is_empty() {
        return 0;
    }

    // If we've already processed the node, return its index.
    if let Some(&index) = node_indices.get(&new_node) {
        return index;
    }

    let children = match new_node.layer() {
        Layer(0) => panic!("Cannot create Macrocell from 1x1 node"),
        Layer(1) => {
            let cells = new_node.as_leaf().unwrap().cells();
            macrocell_index_iter::<D>()
                .map(|i| cells[i] as usize)
                .collect_vec()
        }
        Layer(_) => {
            let children = new_node.subdivide().unwrap();
            macrocell_index_iter::<D>()
                .map(|i| add_macrocell_node(node_list, node_indices, children[i]))
                .collect_vec()
        }
    };

    let new_index = node_list.len();
    node_list.push(MacrocellNode::NonLeaf {
        layer: new_node.layer(),
        children: children.into_boxed_slice(),
    });
    node_indices.insert(new_node, new_index);

    // Return the index where the new node was inserted
    new_index
}

fn macrocell_index_iter<D: Dim>() -> impl Iterator<Item = usize> {
    (0..D::BRANCHING_FACTOR)
        // Reverse the order on all axes ...
        .rev()
        // ... except the X axis.
        .map(|i| i ^ Axis::X.bit())
}
