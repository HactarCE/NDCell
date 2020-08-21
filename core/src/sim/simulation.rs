//! The functions that apply a rule to each cell in a grid.

use itertools::Itertools;
use std::rc::Rc;
use std::sync::Arc;

use crate::num::{BigInt, One, Signed, Zero};
// TODO: glob bad
use crate::*;

// TODO: parallelize using threadpool and crossbeam_channel (call execute threadpool.max_count times with closures that just loop)

// TODO: consider renaming to Simulator or something else

/// A HashLife simulation of a given automaton that caches simulation results.
#[derive(Debug)]
pub struct Simulation<D: Dim> {
    rule: Arc<dyn Rule<D>>,
    min_layer: u32,
}
impl<D: Dim> Default for Simulation<D> {
    fn default() -> Self {
        Self::new(Arc::new(DummyRule))
    }
}

impl<D: Dim> Simulation<D> {
    /// Constructs a new Simulation using the given rule.
    pub fn from<R: 'static + Rule<D>>(rule: R) -> Self {
        Self::new(Arc::new(rule))
    }
    /// Constructs a new Simulation using the given rule.
    pub fn new(rule: Arc<dyn Rule<D>>) -> Self {
        // Determine the minimum layer at which we can simulate one generation
        // of the automaton, using `n / 4 >= r`. (See the documentation for
        // Simulation::advance_inner_node() for an explanation.) Even at r=0 or
        // r=1, the minimum layer is 2 because we need to return the inner node
        // (which is at a lower layer) and the minimum layer is 1.
        let mut min_layer = 2;
        while (1 << min_layer as usize) / 4 < rule.radius() {
            min_layer += 1;
        }

        Self { rule, min_layer }
    }

    /// Advances the given NdTree by the given number of generations.
    pub fn step(&mut self, tree: &mut NdTree<D>, step_size: &BigInt) {
        assert!(
            step_size.is_positive(),
            "Step size must be a positive integer"
        );
        // Prepare the transition function. (Clone self.rule to avoid a &self
        // reference which would prevent self.advance_inner_node() from taking a
        // &mut self.)
        let rule = self.rule.clone();
        let mut transition_function = rule.transition_function();
        // Expand out to the sphere of influence of the existing pattern,
        // following `expansion_distance >= r * t` (rounding `r` and `t` each to
        // the next-highest power of two).
        let radius_log2 = self.rule.radius().next_power_of_two().trailing_zeros();
        let step_size_log2 = step_size.bits();
        let min_expansion_distance = BigInt::one() << (radius_log2 as u64 + step_size_log2);
        let mut expansion_distance = BigInt::zero();
        while expansion_distance < min_expansion_distance {
            tree.expand();
            expansion_distance += tree.len() >> 2;
        }
        // Now expand one more layer to guarantee that the sphere of influence
        // is within the inner node, because Simulation::advance_inner_node()
        // must always returns a node one layer lower than its input. (This also
        // ensures that we aren't somehow still at layer 1; we need to be at at
        // least layer 2 so that the result can be at layer 1, which is the
        // minimum layer for a node.)
        tree.expand();
        // Now do the actual simulation.
        let node_access = tree.node_access();
        let new_root = ArcNode::new(
            tree.cache(),
            self.advance_inner_node(
                tree.root.as_ref(&node_access),
                step_size,
                &mut transition_function,
            ),
        );
        drop(node_access);
        tree.set_root(new_root);
        // Shrink the tree as much as possible to avoid wasted space.
        tree.shrink();

        // TODO: garbage collect
    }

    /// Computes the inner node for a given node after the given numebr of
    /// generations.
    ///
    /// A node's inner node is the node one layer down, centered on the original
    /// node. For example, the inner node of a 16x16 node (layer 4) is the 8x8
    /// node (layer 3) centered on it. HashLife always performs calculations
    /// like this; a node is progressed some distance into the future, and the
    /// state of its inner node is the result. This is because without outside
    /// information, it is impossible to predict the state of the entire node
    /// (since adjacent cells outside of the node could affect it), but it is
    /// always possible in theory to predict the inner node of a node with
    /// length `n` after `t` generations using a rule with max neighborhood
    /// radius `r` if `n / 4 >= r * t`. (`r` defines the maximum speed that
    /// information can travel, so `r * t` is the distance that information can
    /// travel in time `t`, and `n / 4` is the distance from any edge of the
    /// inner node to the edge of the outer node.) In practice, however, each
    /// layer must be computed separately, so the `r` and `t` must each be
    /// replaced with their next lowest power of two.
    #[must_use = "This method returns a new value instead of mutating its input"]
    fn advance_inner_node<'a>(
        &mut self,
        node: NodeRef<'a, D>,
        generations: &BigInt,
        transition_function: &mut TransitionFunction<D>,
    ) -> NodeRef<'a, D> {
        // Make sure we're above the minimum layer.
        assert!(
            node.layer() >= self.min_layer,
            "Cannot advance inner node at layer below minimum simulation layer"
        );

        let ret = if let Some(result) = node.result() {
            // If the result is already computed, just return that.
            return result;
        } else if generations.is_zero() {
            // Handle the simplest case of just not simulating anything. This is
            // one of the recursive base cases.
            node.centered_inner().unwrap()
        } else if node.is_empty() {
            // If the entire node is empty, then in the future it will remain
            // empty. This is not strictly necessary, but it is an obvious
            // optimization for rules without "B0" behavior.

            // Rather than constructing a new node or fetching one from the
            // cache, just return one of the children of this one (since we know
            // it's empty).
            match node.as_enum() {
                NodeRefEnum::Leaf(node) => node.subdivide().unwrap()[0],
                NodeRefEnum::NonLeaf(node) => node.child_at_index(0),
            }
        } else if node.layer() == self.min_layer {
            // If this is the minimum layer, just process each cell
            // individually. This another recursive base case.
            assert!(
                generations.is_one(),
                "Cannot simulate more than 1 generation at minimum layer"
            );
            let old_cell_ndarray = Rc::new(NdArray::from(node));
            let base_offset = 1 << (node.layer() as usize - 2);
            // cache.get_small_node_from_cell_fn(
            //     node.layer() as usize - 1,
            //     NdVec::origin(),
            //     &mut |pos| {
            //         let slice = old_cell_ndarray.clone().offset_slice(-&pos - base_offset);
            //         transition_function(slice)
            //     },
            // )
            todo!("simulate for one generation");
        } else if node.layer() - 1 <= node.repr().base_layer {
            // If this node's children are leaf nodes, the node is small enough
            // to process each cell individually. This is the final recursive
            // base case.

            todo!("simulate for multiple generations")
        } else {
            // In the algorithm described below, there are two `t/2`s that must
            // add up to `t` (where `t` is the number of generations to
            // simulate). But of course if `t` is odd, then this may not be the
            // case. It hardly matters whether `t_outer` or `t_inner` is larger,
            // as long as they differ by no more than `1` and they add up to
            // `t`.
            let t_inner = generations / 2;
            let t_outer = generations - &t_inner;

            // Let `L` be the layer of the current node, and let `t` be the
            // number of generations to simulate. Colors refer to Figure 4 in
            // this article: https://www.drdobbs.com/jvm/_/184406478.
            //
            // We already checked that this node's children (at layer `L-1`) are
            // not leaf nodes, but its grandchildren (at layer `L-2`) might be.

            // TODO: Note that the use of NdArray here assumes that NdRect
            // iterates in the same order as NdArray; this probably shouldn't be
            // relied upon.

            // 1. Make a 4^D array of nodes at layer `L-2` of the original node
            //    at time `0`.
            let unsimmed_quarter_size_nodes: NdArray<NodeRef<'a, D>, D> = NdArray::from_flat_slice(
                UVec::repeat(4_usize),
                URect::<D>::span(UVec::origin(), UVec::repeat(3_usize))
                    .iter()
                    .map(|pos| {
                        node.as_non_leaf()
                            .unwrap()
                            .grandchild_at_index(node_math::leaf_node_pos_to_cell_index::<D>(
                                2, pos,
                            ))
                            .unwrap()
                    })
                    .collect_vec(),
            );

            // 2. Combine adjacent nodes at layer `L-2` to make a 3^D array of
            //    nodes at layer `L-1` and time `0`.
            let unsimmed_half_size_nodes: NdArray<NodeRef<'a, D>, D> = NdArray::from_flat_slice(
                UVec::repeat(3_usize),
                URect::<D>::span(UVec::origin(), UVec::repeat(2_usize))
                    .iter()
                    .map(|pos| {
                        node.cache_access()
                            .join_nodes(
                                NdRect::span(pos.clone(), pos + 1)
                                    .iter()
                                    .map(|pos| unsimmed_quarter_size_nodes[pos].as_raw())
                                    .collect_vec(),
                            )
                            .as_ref()
                    })
                    .collect_vec(),
            );

            // 3. Simulate each of those nodes to get a new node at layer `L-2`
            //    and time `t/2` (red squares).
            let half_simmed_quarter_size_nodes: NdArray<NodeRef<'a, D>, D> =
                unsimmed_half_size_nodes
                    .map(|&node| self.advance_inner_node(node, &t_inner, transition_function));

            // 4. Combine adjacent nodes from step #3 to make a 2^D array of
            //    nodes at layer `L-1` and time `t/2`.
            let half_simmed_half_size_nodes: NdArray<NodeRef<'a, D>, D> = NdArray::from_flat_slice(
                UVec::repeat(2_usize),
                URect::<D>::span(UVec::origin(), UVec::repeat(1_usize))
                    .iter()
                    .map(|pos| {
                        node.cache_access()
                            .join_nodes(
                                NdRect::span(pos.clone(), pos + 1)
                                    .iter()
                                    .map(|pos| half_simmed_quarter_size_nodes[pos].as_raw())
                                    .collect_vec(),
                            )
                            .as_ref()
                    })
                    .collect_vec(),
            );

            // 5. Simulate each of those nodes to get a new node at layer `L-2`
            //    and time `t` (green squares).
            let fully_simmed_quarter_size_nodes: NdArray<&RawNode, D> = half_simmed_half_size_nodes
                .map(|&node| {
                    self.advance_inner_node(node, &t_outer, transition_function)
                        .as_raw()
                });

            // 6. Combine the nodes from step #5 to make a new node at layer
            //    `L-1` and time `t` (blue square). This is the final result.
            node.cache_access()
                .join_nodes(fully_simmed_quarter_size_nodes.into_flat_slice())
                .as_ref()
        };

        // Cache that result so we don't have to do all that work next time.
        node.set_result(Some(ret));
        ret
    }
}
