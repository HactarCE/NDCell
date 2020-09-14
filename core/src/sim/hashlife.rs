//! HashLife simulation algorithm.

use itertools::Itertools;
use std::convert::TryInto;
use std::sync::Arc;

use super::rule::{DummyRule, Rule, TransitionFunction};
use crate::dim::Dim;
use crate::ndarray::NdArray;
use crate::ndrect::{NdRect, URect};
use crate::ndtree::{
    CachedNodeRefTrait, HashLifeParams, Layer, NdTree, NodeRef, NodeRefEnum, NodeRefTrait,
};
use crate::ndvec::UVec;
use crate::num::{BigInt, One, Signed, ToPrimitive, Zero};

// TODO: parallelize using threadpool and crossbeam_channel (call execute threadpool.max_count times with closures that just loop)

/// A HashLife simulator for a CA rule.
///
/// This struct takes ND-trees and computes their future state after some number
/// of generations.
#[derive(Debug)]
pub struct HashLife<D: Dim> {
    rule: Arc<dyn Rule<D>>,
    min_layer: Layer,
}
impl<D: Dim> Default for HashLife<D> {
    fn default() -> Self {
        Self::new(Arc::new(DummyRule))
    }
}

impl<D: Dim> HashLife<D> {
    /// Constructs a `Simulation` using the given rule.
    pub fn from<R: 'static + Rule<D>>(rule: R) -> Self {
        Self::new(Arc::new(rule))
    }
    /// Constructs a `Simulation` using the given rule.
    pub fn new(rule: Arc<dyn Rule<D>>) -> Self {
        // Determine the minimum layer at which we can simulate one generation
        // of the automaton, using `n / 4 >= r`. (See the documentation for
        // Simulation::advance_inner_node() for an explanation.) Even at r=0 or
        // r=1, the minimum layer is 2 because we need to return the inner node
        // (which is at a lower layer) and the minimum layer is 1.
        let mut min_layer = Layer(2);
        while min_layer.len().unwrap() / 4 < rule.radius() {
            min_layer = min_layer.parent_layer();
        }

        Self { rule, min_layer }
    }

    /// Advances the given NdTree by the given number of generations.
    pub fn step(&self, tree: &mut NdTree<D>, gens: &BigInt) {
        if gens.is_negative() {
            panic!("Cannot simulate negative timestep");
        }
        if gens.is_zero() {
            // No need to simulate anything!
            return;
        }

        // The number of trailing zeros (i.e. index of the first `1` bit) gives
        // the greatest power-of-2 multiple. We can call `.unwrap()` because we
        // already handled the case of `gens == 0`.
        let log2_step_size = gens
            .trailing_zeros()
            .unwrap()
            .try_into()
            .expect("Step size too large!");

        let _node_cache = Arc::clone(tree.cache());
        let node_cache = _node_cache.read();
        let mut sim_params = node_cache.sim_lock().write();

        // Set the simulation step size. (Invalidate the cache if necessary.)
        sim_params.set_log2_step_size(log2_step_size);

        // Prepare the transition function. (Clone self.rule to avoid a &self
        // reference which would prevent self.advance_inner_node() from taking a
        // &mut self.)
        let rule = self.rule.clone();
        let mut transition_function = rule.transition_function();

        // If the number of generations is not a power of 2, we may have to
        // break this into multiple power-of-2-sized steps.
        let num_steps = (gens >> log2_step_size).to_u64().expect(
            "Simulation requires too many individual steps; try using a power of 2 step size",
        );
        for _ in 0..num_steps {
            // Expand out to the sphere of influence of the existing pattern,
            // following `expansion_distance >= r * t` (rounding `r` and `t` each to
            // the next-highest power of two).
            let log2_radius = self.rule.radius().next_power_of_two().trailing_zeros();
            let min_expansion_distance = BigInt::one() << (log2_radius + log2_step_size);
            let mut expansion_distance = BigInt::zero();
            while expansion_distance < min_expansion_distance {
                tree.expand(&*node_cache);
                expansion_distance += tree.len() >> 2;
            }

            // Now expand one more layer to guarantee that the sphere of influence
            // is within the inner node, because Simulation::advance_inner_node()
            // must always returns a node one layer lower than its input. (This also
            // ensures that we aren't somehow still at layer 1; we need to be at at
            // least layer 2 so that the result can be at layer 1, which is the
            // minimum layer for a node.)
            tree.expand(&*node_cache);
            // Now do the actual simulation.
            let new_root = self.advance_inner_node(
                tree.root().as_ref(&*node_cache),
                &sim_params,
                &mut transition_function,
            );
            tree.set_root_centered(new_root);

            // Shrink the tree as much as possible to avoid wasted space. TODO:
            // is it better to have this inside the loop or outside the loop?
            tree.shrink(&*node_cache);
        }

        // TODO: garbage collect at some point
    }

    /// Computes the inner node for a given node after some predetermined number
    /// of generations.
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
    fn advance_inner_node<'cache>(
        &self,
        node: NodeRef<'cache, D>,
        sim_params: &HashLifeParams<D>,
        transition_function: &mut TransitionFunction<'_, D>,
    ) -> NodeRef<'cache, D> {
        // Make sure we're above the minimum layer.
        assert!(
            node.layer() >= self.min_layer,
            "Cannot advance inner node at layer below minimum simulation layer"
        );

        if let Some(result) = node.result() {
            // If the result is already computed, just return that.
            return result;
        }

        let ret: NodeRef<'cache, D> = if node.is_empty() {
            // If the entire node is empty, then in the future it will remain
            // empty. This is not strictly necessary, but it is an obvious
            // optimization for rules without "B0" behavior.

            // Rather than constructing a new node or fetching one from the
            // cache, just return one of the children of this one (since we know
            // it's empty).
            match node.as_enum() {
                NodeRefEnum::Leaf(n) => n.cache().get_empty(n.layer().child_layer()),
                // It's easier to get a reference to a child than to look up an
                // empty node.
                NodeRefEnum::NonLeaf(n) => n.child_at_index(0).into(),
            }
        } else if node.layer() == self.min_layer || node.layer().child_layer() <= Layer::base::<D>()
        {
            // If this is the minimum layer or the node's children are leaf
            // nodes, just process each cell individually. This is the final
            // recursive base case.

            // We start with a node at layer `L` and time `0` and will end with
            // a node at layer `L-1` at time `t`, centered on the original node.
            let l = node.layer();
            let t = sim_params.node_step_size(l, self.rule.radius()).unwrap();

            // We're able to do this because the "speed of light" is equal to
            // `r*t`, where `r` is the maximum Chebyshev radius of a cell's
            // neighborhood (https://en.wikipedia.org/wiki/Chebyshev_distance)
            // and `r*t` is less than the "padding" between the edge of the
            // result node and the original node.
            let mut rt = self.rule.radius() * t;
            assert!(
                rt <= l.len().unwrap() / 4,
                "Cannot simulate {} generations at {:?} with radius {}",
                t,
                l,
                self.rule.radius(),
            );

            let mut cells_ndarray = NdArray::from(node);
            let mut cells_ndarray_len = l.len().unwrap();
            // For each timestep ...
            for _ in 0..t {
                // `rt` is how much "padding" we need around each edge of the
                // result in order to simulate remaining generations.
                rt -= self.rule.radius();
                let new_cells_ndarray_len = l.child_layer().len().unwrap() + 2 * rt;
                // Compute the offset between the lowest corner of the current
                // `cells_ndarray` and the new one.
                let offset = (cells_ndarray_len - new_cells_ndarray_len) / 2;
                // Make a new array that is as big as we need.
                cells_ndarray = transition_function(
                    &cells_ndarray,
                    URect::with_size(UVec::repeat(offset), UVec::repeat(new_cells_ndarray_len)),
                );
                cells_ndarray_len = new_cells_ndarray_len;
            }

            // Finally, make an ND-tree from those cells
            node.cache().get_from_cells(cells_ndarray.into_flat_slice())
        } else {
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
            let unsimmed_quarter_size_nodes: NdArray<NodeRef<'cache, D>, D> =
                NdArray::from_flat_slice(
                    UVec::repeat(4_usize),
                    (0..(D::BRANCHING_FACTOR * D::BRANCHING_FACTOR))
                        .map(|i| node.as_non_leaf().unwrap().grandchild_at_index(i))
                        .collect_vec(),
                );

            // 2. Combine adjacent nodes at layer `L-2` to make a 3^D array of
            //    nodes at layer `L-1` and time `0`.
            let unsimmed_half_size_nodes: NdArray<NodeRef<'cache, D>, D> = NdArray::from_flat_slice(
                UVec::repeat(3_usize),
                URect::<D>::span(UVec::origin(), UVec::repeat(2_usize))
                    .iter()
                    .map(|pos| {
                        node.cache().join_nodes(
                            NdRect::span(pos.clone(), pos + 1)
                                .iter()
                                .map(|pos| unsimmed_quarter_size_nodes[pos]),
                        )
                    })
                    .collect_vec(),
            );

            // 3. Simulate each of those nodes to get a new node at layer `L-2`
            //    and time `t/2` (red squares).
            let half_simmed_quarter_size_nodes: NdArray<NodeRef<'cache, D>, D> =
                unsimmed_half_size_nodes
                    .map(|&n| self.advance_inner_node(n, sim_params, transition_function));

            // 4. Combine adjacent nodes from step #3 to make a 2^D array of
            //    nodes at layer `L-1` and time `t/2`.
            let half_simmed_half_size_nodes =
                URect::<D>::span(UVec::origin(), UVec::repeat(1_usize))
                    .iter()
                    .map(|pos| {
                        node.cache().join_nodes(
                            NdRect::span(pos.clone(), pos + 1)
                                .iter()
                                .map(|pos| half_simmed_quarter_size_nodes[pos]),
                        )
                    });

            let this_nodes_log2_step_size =
                sim_params.log2_node_step_size(node.layer(), self.rule.radius());
            let childrens_log2_step_size =
                sim_params.log2_node_step_size(node.layer().child_layer(), self.rule.radius());

            // 5. Simulate each of those nodes to get a new node at layer `L-2`
            //    and time `t` (green squares).
            let fully_simmed_quarter_size_nodes = half_simmed_half_size_nodes.map(|node| {
                if this_nodes_log2_step_size != childrens_log2_step_size {
                    self.advance_inner_node(node, sim_params, transition_function)
                } else {
                    // ... unless that `t/2` was actually the total number of
                    // generations we needed to simulate, in which case don't
                    // simulate any more. Just grab the inner node of each of
                    // those nodes (which is like simulating them for zero
                    // generations).
                    node.centered_inner().unwrap()
                }
            });

            // 6. Combine the nodes from step #5 to make a new node at layer
            //    `L-1` and time `t` (blue square). This is the final result.
            node.cache().join_nodes(fully_simmed_quarter_size_nodes)
        };

        // Cache that result so we don't have to do all that work next time.
        node.set_result(Some(ret));
        ret
    }
}
