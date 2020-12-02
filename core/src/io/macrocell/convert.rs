use itertools::Itertools;
use std::sync::Arc;

use super::{Macrocell, MacrocellError, MacrocellNode, MacrocellResult};
use crate::automaton::{Automaton, AutomatonRef, NdAutomaton};
use crate::axis::Axis;
use crate::dim::Dim;
use crate::ndtree::{Layer, NdTree, NodePool, NodeRef, NodeRefTrait, SharedNodePool};
use crate::ndvec::{BigVec, BigVec6D};
use crate::num::{BigInt, Zero};
use crate::sim::rule::{NdRule, Rule};
use crate::HashMap;

impl Automaton {
    /// Loads an automaton from a Macrocell string using a new node pool.
    pub fn from_macrocell_str(
        s: &str,
        resolve_rule: impl FnOnce(Option<&str>) -> MacrocellResult<Rule>,
    ) -> MacrocellResult<Self> {
        Self::from_macrocell(&s.parse()?, resolve_rule)
    }

    /// Loads an automaton from a Macrocell struct using a new node pool.
    pub fn from_macrocell(
        macrocell: &Macrocell,
        resolve_rule: impl FnOnce(Option<&str>) -> MacrocellResult<Rule>,
    ) -> MacrocellResult<Self> {
        fn _macrocell_to_ndautomaton<D: Dim>(
            macrocell: &Macrocell,
            rule: Arc<dyn NdRule<D>>,
        ) -> MacrocellResult<Automaton> {
            NdAutomaton::from_macrocell_with_node_pool(
                macrocell,
                &SharedNodePool::new().access(),
                |_| Ok(rule),
            )
            .map(|a| a.into())
        }

        match resolve_rule(macrocell.rule())? {
            Rule::Rule1D(rule) => _macrocell_to_ndautomaton(&macrocell, rule),
            Rule::Rule2D(rule) => _macrocell_to_ndautomaton(&macrocell, rule),
            Rule::Rule3D(rule) => _macrocell_to_ndautomaton(&macrocell, rule),
            Rule::Rule4D(rule) => _macrocell_to_ndautomaton(&macrocell, rule),
            Rule::Rule5D(rule) => _macrocell_to_ndautomaton(&macrocell, rule),
            Rule::Rule6D(rule) => _macrocell_to_ndautomaton(&macrocell, rule),
        }
    }
}
impl AutomatonRef<'_> {
    /// Exports the automaton to a Macrocell string.
    pub fn to_macrocell_string(&self) -> String {
        match self {
            Self::Automaton1D(a) => a.to_macrocell_string(),
            Self::Automaton2D(a) => a.to_macrocell_string(),
            Self::Automaton3D(a) => a.to_macrocell_string(),
            Self::Automaton4D(a) => a.to_macrocell_string(),
            Self::Automaton5D(a) => a.to_macrocell_string(),
            Self::Automaton6D(a) => a.to_macrocell_string(),
        }
    }

    /// Exports the automaton to a Macrocell struct.
    pub fn to_macrocell(&self) -> Macrocell {
        match self {
            Self::Automaton1D(a) => a.to_macrocell(),
            Self::Automaton2D(a) => a.to_macrocell(),
            Self::Automaton3D(a) => a.to_macrocell(),
            Self::Automaton4D(a) => a.to_macrocell(),
            Self::Automaton5D(a) => a.to_macrocell(),
            Self::Automaton6D(a) => a.to_macrocell(),
        }
    }
}
impl<D: Dim> NdAutomaton<D> {
    /// Loads an automaton from a Macrocell string using a new node pool.
    pub fn from_macrocell_str(
        s: &str,
        resolve_rule: impl FnOnce(Option<&str>) -> MacrocellResult<Arc<dyn NdRule<D>>>,
    ) -> MacrocellResult<Self> {
        Self::from_macrocell_str_with_node_pool(s, &SharedNodePool::new().access(), resolve_rule)
    }
    /// Loads an automaton from a Macrocell string using an existing node pool.
    pub fn from_macrocell_str_with_node_pool(
        s: &str,
        node_pool: &NodePool<D>,
        resolve_rule: impl FnOnce(Option<&str>) -> MacrocellResult<Arc<dyn NdRule<D>>>,
    ) -> MacrocellResult<Self> {
        Self::from_macrocell_with_node_pool(&s.parse()?, node_pool, resolve_rule)
    }

    /// Loads an automaton from a Macrocell struct using an existing node pool.
    pub fn from_macrocell_with_node_pool(
        macrocell: &Macrocell,
        node_pool: &NodePool<D>,
        resolve_rule: impl FnOnce(Option<&str>) -> MacrocellResult<Arc<dyn NdRule<D>>>,
    ) -> MacrocellResult<Self> {
        let rule = resolve_rule(macrocell.rule())?;
        let generations = macrocell.generation();
        Ok(NdAutomaton {
            tree: NdTree::from_macrocell_with_node_pool(macrocell, node_pool)?,
            rule,
            generations,
        })
    }

    /// Exports the automaton to a Macrocell string.
    pub fn to_macrocell_string(&self) -> String {
        self.to_macrocell().to_string()
    }
    /// Exports the automaton to a Macrocell struct.
    pub fn to_macrocell(&self) -> Macrocell {
        self.tree
            .to_macrocell()
            .with_rule(Some(self.rule.to_string()))
            .with_generation(self.generations.clone())
    }
}

impl<D: Dim> NdTree<D> {
    /// Loads an ND-tree from a Macrocell string using a new node pool.
    pub fn from_macrocell_str(s: &str) -> MacrocellResult<Self> {
        Self::from_macrocell_str_with_node_pool(s, &SharedNodePool::new().access())
    }
    /// Loads an ND-tree from a Macrocell string using an existing node pool.
    pub fn from_macrocell_str_with_node_pool(
        s: &str,
        node_pool: &NodePool<D>,
    ) -> MacrocellResult<Self> {
        Self::from_macrocell_with_node_pool(&s.parse()?, node_pool)
    }

    /// Loads an ND-tree from a Macrocell struct using an existing node pool.
    pub fn from_macrocell_with_node_pool(
        macrocell: &Macrocell,
        node_pool: &NodePool<D>,
    ) -> MacrocellResult<Self> {
        let mut nodes: Vec<NodeRef<'_, D>> = vec![];

        for macrocell_node in &macrocell.nodes {
            let new_node = match macrocell_node {
                // Put a dummy node at index 0 -- we'll handle index 0 specially
                // anyway.
                MacrocellNode::Empty => node_pool.get_empty_base(),

                MacrocellNode::Leaf8x8 { bits } => {
                    if D::NDIM != 2 {
                        return Err(MacrocellError::LeafNodeNon2D);
                    }
                    node_pool.get_from_cells(
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
                            node_pool.get_from_cells(children_iter.map(|i| i as u8).collect_vec())
                        }
                        Layer(_) => node_pool.join_nodes(
                            children_iter
                                .map(|child_index| match child_index {
                                    0 => Ok(node_pool.get_empty(layer.child_layer())),
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

        let offset = BigVec::from_fn(|ax| macrocell.offset[ax].clone());
        Ok(NdTree::from_node_centered_on(
            nodes.pop().unwrap_or_else(|| node_pool.get_empty_base()),
            offset,
        ))
    }

    /// Exports a rectangle from the automaton to a Macrocell struct. If `rect`
    /// is `None`, the entire grid is exported.
    pub fn to_macrocell(&self) -> Macrocell {
        let mut offset = BigVec6D::origin();
        let center = self.center();
        for &ax in D::axes() {
            offset[ax] = center[ax].clone();
        }

        let mut nodes = vec![MacrocellNode::Empty];
        add_macrocell_node(
            &mut nodes,
            &mut HashMap::default(),
            self.root_ref().as_ref(),
        );

        Macrocell {
            rule: None,
            gen: BigInt::zero(),
            offset,
            nodes,
            comments: String::new(),
        }
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
