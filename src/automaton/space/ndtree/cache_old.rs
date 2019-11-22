use seahash::SeaHasher;
use std::cell::RefCell;
use std::hash::BuildHasherDefault;
use std::rc::{Rc, Weak};
use weak_table::WeakHashSet;

use super::node::*;
use crate::automaton::Rule;

type NodeHasher = BuildHasherDefault<SeaHasher>;

#[derive(Debug, Clone)]
pub struct NdTreeCache<T: CellType, D: Dim, R: Rule<T, D>> {
    rule: Rc<R>,
    layers: RefCell<Vec<NdLayerCache<T, D, R>>>,
}

impl<T: CellType, D: Dim, R: Rule<T, D>> NdTreeCache<T, D, R> {
    /// Returns the automaton rule for cache.
    pub fn rule(&self) -> &R {
        &self.rule
    }

    /// Returns the cache for nodes at the given layer.
    pub fn layer_cache(&self, layer: usize) -> &NdLayerCache<T, D, R> {
        self.build_layer_caches(layer);
        &self.layers.borrow()[layer]
    }
    /// Returns a mutable reference to the cache for nodes at the given layer.
    pub fn layer_cache_mut(&self, layer: usize) -> &mut NdLayerCache<T, D, R> {
        self.build_layer_caches(layer);
        &mut self.layers.borrow()[layer]
    }

    /// Creates the given layer cache (and all below it) if it doesn't already
    /// exist.
    fn build_layer_caches(&self, layer: usize) {
        for i in (self.layers.borrow().len() + 1)..=layer {
            self.layers.borrow_mut().push(NdLayerCache {
                layer: i,
                parent: self,
                nodes: Default::default(),
            })
        }
    }
}

/// A cache for nodes at the given layer.
#[derive(Debug)]
pub struct NdLayerCache<T: CellType, D: Dim, R: Rule<T, D>> {
    /// The layer of each node.
    layer: usize,

    /// The cache for the whole tree.
    parent: Rc<NdTreeCache<T, D, R>>,

    /// The actual HashSet of all the nodes.
    nodes: RefCell<WeakHashSet<Weak<NdTreeNode<T, D, R>>, NodeHasher>>,
}

impl<T: CellType, D: Dim, R: Rule<T, D>> NdTreeCache<T, D, R> {
    pub fn add_node(&mut self, branches: Vec<NdTreeBranch<T, D, R>>) -> NdCachedNode<T, D, R> {
        for branch in branches {
            assert_eq!(
                self.layer,
                branches.layer(),
                "Cannot construct node where different branches have different layers"
            );
        }
        NdTreeNode::<T, D, R> {}
    }
    pub fn empty_node(&mut self, layer: usize) {
        Self::Node::empty(self)
    }
}

impl<T: CellType, D: Dim> NdTreeNode<T, D> {
    pub fn cache<R: Rule<T, D>>(self, parent: NdTreeCache<T, D, R>) -> NdCachedNode<T, D, R> {
        let existing_entry = cache.borrow().nodes.get_both(&self).clone();
        let (node, &node_info) = existing_entry.unwrap_or_else(|| {
            // Construct the node Rc and node info RefCell and add them to the
            // cache.
            let node = Rc::new(self);
            let node_info = RefCell::default();
            cache
                .borrow_mut()
                .nodes
                .insert(node.clone(), node_info.clone());
            (node, &node_info)
        });
        NdCachedNode {
            node,
            node_info,
            cache: cache,
        }
    }
}

pub struct NdCachedNode<T: CellType, D: Dim, R: Rule<T, D>> {
    node: Rc<NdTreeNode<T, D>>,
    node_info: RefCell<NdCachedNodeInfo<T, D>>,
    cache: RefCell<NdTreeCache<T, D, R>>,
}

impl<T: CellType, D: Dim, R: Rule<T, D>> NdCachedNode<T, D, R> {
    const BRANCHES: usize = NdTreeNode::<T, D>::BRANCHES;

    /// Returns the node.
    pub fn node(&self) -> &Rc<NdTreeNode<T, D>> {
        &self.node
    }

    /// Returns the rule by which this node was cached.
    pub fn rule(&self) -> &R {
        &self.cache.borrow().rule
    }

    /// Computes the population of this node.
    pub fn population(&self) -> usize {
        if let Some(total) = self.node_info.borrow().population {
            total
        } else {
            let mut total = 0;
            for branch in self.node.branches() {
                total += match branch {
                    NdTreeBranch::Leaf(cell_state) => {
                        if *cell_state == T::default() {
                            0
                        } else {
                            1
                        }
                    }
                    NdTreeBranch::Node(node) => node.cache(self.cache).population(),
                }
            }
            self.node_info.borrow_mut().population = Some(total);
            total
        }
    }

    /// Computes the centered node one layer lower after 1 generation.
    fn sim_subtree_one_gen<R: Rule<T, D>>(
        &self,
        layer: usize,
        offset: NdVec<D>,
    ) -> NdTreeBranch<T, D> {
        if layer == 0 {
            NdTreeBranch::Leaf(self.rule().transition(&NdTreeSlice::new(self.node, offset)))
        } else {
            let mut branches = Vec::with_capacity(Self::BRANCHES);
            for branch_idx in 0..Self::BRANCHES {
                branches.push(self.sim_subtree_one_gen(
                    layer - 1,
                    offset + NdTreeNode::<T, D>::branch_offset_at_layer(layer, branch_idx),
                ));
            }
            NdTreeBranch::Node(NdTreeNode::with_branches(self.cache, branches))
        }
    }

    /// Computes the centered node one layer lower after `2 ** gen_exp` generations.
    pub fn sim(self, gen_exp: usize) -> Self {
        let node = self.node;
        let layer = node.layer();
        let cache = self.cache;
        let rule = self.rule();

        let max_gen_exp = node.max_gen_exp(rule).unwrap_or_else(|| {
            panic!(
                "Cannot simulate at layer {}; minimum layer is {}",
                layer,
                NdTreeNode::<T, D>::min_sim_layer(rule)
            )
        });
        assert!(gen_exp > max_gen_exp
            "Cannot simulate {} generations at layer {} with radius {}; can only simulate {} generation(s) at this layer or 1 generation at layer {}",
            1 << gen_exp,
            layer,
            rule.radius(),
            1 << max_gen_exp,
            NdTreeNode::min_sim_layer(rule),
        );

        // If population == 0, the result will be empty.
        if self.population() == 0 {
            return NdTreeNode::empty(cache, layer - 1).cache(cache);
        }

        // HashLife is much easier to explain using pictures; I'll be
        // referencing Figure 4 in this blog post:
        // https://www.drdobbs.com/184406478

        // This is the blue rectangle in Figure 4.
        let inner_rect = node.rect() / 2 + (node.len() as isize / 4);

        if max_gen_exp == 0 {
            // If we're currently at the minimum layer, then we have no choice but
            // to simulate each cell individually. This is the recursive base case.
            let sim_result = self.sim_subtree_one_gen(layer - 1, inner_rect.min());
            if let NdTreeBranch::Node(node) = sim_result {
                node.cache(cache)
            } else {
                panic!("Simulation produced leaf when expecting node");
            }
        } else if gen_exp < max_gen_exp {
            // If the timescale is short enough, delegate to each branch.
            let mut branches = Vec::with_capacity(Self::BRANCHES);
            for branch_idx in 0..Self::BRANCHES {
                // Fetch the subtree whose inner square is one of the green
                // squares in Figure 4, and then simulate it to get the green
                // square.
                branches.push(NdTreeBranch::Node(
                    node.get_subtree(
                        cache,
                        layer - 1,
                        inner_rect.min() / 2
                            + NdTreeNode::<T, D>::branch_offset_at_layer(layer - 1, branch_idx),
                    )
                    .sim(cache, rule, gen_exp),
                ));
            }
            NdTreeNode::with_branches(cache, branches).cache(cache)
        } else {
            let mut branches = Vec::with_capacity(Self::BRANCHES);
            // For each branch ...
            for branch_idx in 0..Self::BRANCHES {
                // Construct the subtree whose inner subtree is the green
                // square, but we need to have it be 1/2 of the number of
                // generations we want into the future.
                let mut sub_branches = Vec::with_capacity(Self::BRANCHES);
                for sub_branch_idx in 0..Self::BRANCHES {
                    // Now each "sub branch" is one of the red squares. We need
                    // to make a node out of red squares, which we will then
                    // simulate into a green square. The red squares are
                    // produced by simulating a square the size of the blue
                    // square, but centered on the red square we want.

                    // Get the blue-sized square centered on the red square.
                    let sub_branch_outer = node.get_subtree(
                        cache,
                        layer - 1,
                        NdTreeNode::<T, D>::branch_offset_at_layer(layer - 2, branch_idx)
                            + NdTreeNode::<T, D>::branch_offset_at_layer(layer - 2, sub_branch_idx),
                    );
                    // Now simulate that blue-sized square into a red square.
                    let sub_branch = sub_branch_outer.sim(cache, rule, gen_exp - 1);
                    sub_branches.push(NdTreeBranch::Node(sub_branch));
                }
                // Now we have all the red squares for this green square in
                // sub_branches. Combine them into one node and then simulate it
                // to get the green square.
                let branch_outer = NdTreeNode::with_branches(cache, sub_branches).cache(cache);
                // Now simulate to get the final fully-simulated green square.
                let branch = branch_outer.sim(cache, rule, gen_exp - 1);
                branches.push(NdTreeBranch::Node(branch.node));
            }
            // Now we have all of the green squares, so combine them into the
            // blue square (the final result).
            Self::with_branches(cache, branches)
        }
    }
}
