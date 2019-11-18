use seahash::SeaHasher;
use std::fmt;
use std::hash::Hasher;
use std::marker::PhantomData;
use std::ops::Index;
use std::rc::Rc;

use super::*;
use crate::automaton::Rule;

/// An interned NdTreeNode.
pub type NdSubTree<T, D> = Rc<NdTreeNode<T, D>>;

/// An NdTreeNode's child.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NdTreeBranch<T: CellType, D: Dim> {
    /// All cells within this branch are the same cell state.
    Leaf(T),

    /// An interned subnode.
    Node(NdSubTree<T, D>),
}

impl<T: CellType, D: Dim> Default for NdTreeBranch<T, D> {
    fn default() -> Self {
        Self::Leaf(T::default())
    }
}

impl<T: CellType, D: Dim> NdTreeBranch<T, D> {
    fn empty(cache: &mut NdTreeCache<T, D>, layer: usize) -> Self {
        if layer == 0 {
            Self::Leaf(T::default())
        } else {
            Self::Node(NdTreeNode::empty(cache, layer))
        }
    }
}

// TODO TODO TODO resume here
// TODO make layer 1 be 2x2; the field is always split
// the enum choice moves _inside_ the vector; the vector is contained in the node

/// A single node in the NdTree, which contains information about its layer
/// (base-2 logarithm of hypercube side length) and its children.
#[derive(Clone)]
pub struct NdTreeNode<T: CellType, D: Dim> {
    /// The "layer" of this node (base-2 logarithm of hypercube side length).
    layer: usize,

    /// The branches of this node, stored as a flattened 2^d hypercube of nodes
    /// one layer lower.
    ///
    /// If layer == 1, then all of these must be `NdTreeBranch::Leaf`s. If layer
    /// > 1, then all of these must be `NdTreeBranch::Branch`es.
    ///
    /// Until rust-lang #44580 (RFC 2000) is resolved, there's no way to use
    /// D::NDIM as the array size. It might be worth implementing a custom
    /// unsafe type for this, but at the time of writing such an optimization
    /// would be entirely premature.
    branches: Vec<NdTreeBranch<T, D>>,

    hash_code: u64,

    phantom: PhantomData<D>,
}

impl<T: CellType, D: Dim> Debug for NdTreeNode<T, D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "NdTreeNode {{ layer: {:?}, branches: {:?} }}",
            self.layer, self.branches
        )
    }
}

impl<T: CellType, D: Dim> NdTreeNode<T, D> {
    /// Constructs a new empty NdTreeNode at a given layer.
    pub fn empty(cache: &mut NdTreeCache<T, D>, layer: usize) -> NdSubTree<T, D> {
        if layer == 0 {
            panic!("Cannot construct NdTreeNode at layer 0.");
        }
        let branches = vec![NdTreeBranch::empty(cache, layer - 1); Self::BRANCHES];
        Self::with_branches(cache, branches)
    }
    /// Constructs a new NdTreeNode at a given layer and with the given branches.
    pub fn with_branches(
        cache: &mut NdTreeCache<T, D>,
        branches: Vec<NdTreeBranch<T, D>>,
    ) -> NdSubTree<T, D> {
        // Check that there are the right number of branches.
        if branches.len() != Self::BRANCHES {
            panic!(
                "NdTreeNode of {} dimensions must have {} branches; got {:?} instead.",
                D::NDIM,
                Self::BRANCHES,
                branches
            );
        }
        // Check that the branches are all at the same layer (and infer this
        // node's layer based on them).
        let mut branch_layers = branches.iter().map(|branch| match branch {
            NdTreeBranch::Leaf(_) => 0,
            NdTreeBranch::Node(node) => node.layer,
        });
        let layer = branch_layers.next().unwrap() + 1;
        if !branch_layers.all(|branch_layer| branch_layer == layer - 1) {
            panic!("NdTreeNode branches have different layers: {:?}", branches);
        }
        // Compute the hash code.
        let mut hasher = SeaHasher::new();
        layer.hash(&mut hasher);
        branches.hash(&mut hasher);
        // Construct the node.
        let node = Self {
            layer,
            branches,
            hash_code: hasher.finish(),
            phantom: PhantomData,
        };
        node.intern(cache)
    }
    /// Checks whether an equivalent node is present in the cache. If it is,
    /// destroys this one and returns the equivalent node from the cache; if
    /// not, adds this node to the cache and returns it.
    fn intern(self, cache: &mut NdTreeCache<T, D>) -> NdSubTree<T, D> {
        // Construct the Rc and add it to the cache.
        cache.get_key(&self).clone().unwrap_or_else(|| {
            let ret = Rc::new(self);
            cache.insert(ret.clone(), Default::default());
            ret
        })
    }

    pub fn layer(&self) -> usize {
        self.layer
    }
    pub fn branches(&self) -> &Vec<NdTreeBranch<T, D>> {
        &self.branches
    }
    pub fn hash_code(&self) -> u64 {
        self.hash_code
    }

    /// Returns the length of a single side of the hypersquare contained in this
    /// subtree.
    pub fn len(&self) -> usize {
        // layer = 1 => len = 2
        // layer = 2 => len = 4
        // layer = 3 => len = 8
        // etc.
        1 << self.layer
    }
    /// Returns the bounding rectangle for this node, with the origin as the
    /// lower bound.
    pub fn rect(&self) -> NdRect<D> {
        Self::rect_at_layer(self.layer)
    }
    /// Returns the bounding rectangle for a node at the given layer, with the
    /// origin as the lower bound.
    pub fn rect_at_layer(layer: usize) -> NdRect<D> {
        NdRect::span(NdVec::origin(), NdVec::origin() + ((1 << layer) - 1))
    }

    /// The number of branches for this many dimensions (2^d).
    pub const BRANCHES: usize = 1 << D::NDIM;
    /// The bitmask for branch indices.
    const BRANCH_IDX_BITMASK: usize = Self::BRANCHES - 1;
    /// Computes the "branch index" corresponding to this node's child
    /// containing the given position.
    ///
    /// Each nth layer corresponds to the nth bit of each axis, which can either
    /// be 0 or 1. The "branch index" is a number in 0..(2 ** d) composed
    /// from these bits; each bit in the branch index is taken from a different
    /// axis. It's like a bitwise NdVec.
    fn branch_idx(&self, pos: NdVec<D>) -> usize {
        let mut ret = 0;
        for ax in D::axes() {
            ret <<= 1;
            ret |= (pos[ax] as usize >> self.layer) & 1;
        }
        ret
    }
    /// Computes the vector offset for the given branch of this node.
    fn branch_offset(&self, branch_idx: usize) -> NdVec<D> {
        Self::branch_offset_at_layer(self.layer, branch_idx)
    }
    fn branch_offset_at_layer(layer: usize, branch_idx: usize) -> NdVec<D> {
        let mut ret = NdVec::origin();
        let halfway: isize = 1 << (layer - 1);
        for ax in D::axes() {
            // If the current bit of the branch index is 1, add half of the
            // length of this node to the corresponding axis in the result.
            let branch_bit = (branch_idx as isize >> ax as u8) & 1;
            ret[ax] += halfway * branch_bit;
        }
        ret
    }

    /// "Zooms out" of the current tree by a factor of two; returns a new
    /// NdSubTree with the contents of the existing one centered in an empty
    /// grid.
    pub fn expand_centered(&self, cache: &mut NdTreeCache<T, D>) -> NdSubTree<T, D> {
        let new_branches = self
            .branches
            .iter()
            .enumerate()
            .map(|(branch_idx, old_branch)| {
                // Create a new node with this node's Nth branch in the opposite
                // corner. (Bitwise XOR of branch index produces the branch index of
                // the opposite corner.) E.g. This node's northwest branch is placed
                // in the southeast branch of a new node, which will be in the
                // northwest branch of the result.
                let mut inner_branches =
                    vec![NdTreeBranch::empty(cache, self.layer - 1); Self::BRANCHES];
                inner_branches[branch_idx ^ Self::BRANCH_IDX_BITMASK] = old_branch.clone();
                NdTreeBranch::Node(Self::with_branches(cache, inner_branches))
            })
            .collect();
        NdTreeNode::with_branches(cache, new_branches)
    }

    /// Returns the cell value at the given position, modulo the node size.
    pub fn get_cell(&self, pos: NdVec<D>) -> T {
        match &self.branches[self.branch_idx(pos)] {
            NdTreeBranch::Leaf(cell_state) => *cell_state,
            NdTreeBranch::Node(node) => node.get_cell(pos),
        }
    }
    /// Constructs a new node with the cell at the given position, modulo the node
    /// size, having the given value.
    pub fn set_cell(
        &self,
        cache: &mut NdTreeCache<T, D>,
        pos: NdVec<D>,
        cell_state: T,
    ) -> NdSubTree<T, D> {
        let mut new_branches = self.branches.clone();
        // Get the branch containing the given cell.
        let branch = &mut new_branches[self.branch_idx(pos)];
        match branch {
            // The branch is a single cell, so set that cell.
            NdTreeBranch::Leaf(old_cell_state) => *old_cell_state = cell_state,
            // The branch is a node, so recurse on that node.
            NdTreeBranch::Node(node) => *node = node.set_cell(cache, pos, cell_state),
        }
        Self::with_branches(cache, new_branches)
    }

    pub fn get_subtree(
        &self,
        cache: &mut NdTreeCache<T, D>,
        layer: usize,
        offset: NdVec<D>,
    ) -> NdSubTree<T, D> {
        if layer == 0 {
            panic!("Cannot get subtree at layer 0");
        }
        if let NdTreeBranch::Node(node) = self.get_subtree_branch(cache, layer, offset) {
            node
        } else {
            panic!("Requested subtree at layer {}, but got single cell", layer);
        }
    }

    /// Constructs a new node at the given layer whose lower bound is the given
    /// offset. This operation may be expensive if coordinates of the offset
    /// have prime factors other than 2.
    pub fn get_subtree_branch(
        &self,
        cache: &mut NdTreeCache<T, D>,
        layer: usize,
        offset: NdVec<D>,
    ) -> NdTreeBranch<T, D> {
        let result_rect = Self::rect_at_layer(layer) + offset;
        // Check bounds.
        if !self.rect().contains(result_rect) {
            panic!(
                "Subtree {{ layer: {}, offset: {:?} }} out of bounds for layer {}",
                layer, offset, self.layer
            );
        }
        // If it fits within this node, and it's the same layer/size as this
        // node, then it's exactly the same as this node.
        if layer == self.layer {
            return NdTreeBranch::Node(self.clone().intern(cache));
        }
        // Check whether the result is a subtree of a single branch of this
        // node.
        let min_branch_idx = self.branch_idx(result_rect.min());
        let max_branch_idx = self.branch_idx(result_rect.max());
        if min_branch_idx == max_branch_idx {
            // If it is, just delegate to that branch.
            let branch_idx = min_branch_idx;
            match &self.branches[branch_idx] {
                NdTreeBranch::Leaf(cell_state) => NdTreeBranch::Leaf(*cell_state),
                NdTreeBranch::Node(node) => {
                    node.get_subtree_branch(cache, layer, offset - self.branch_offset(branch_idx))
                }
            }
        } else {
            // If it isn't, then divide and conquer.
            let mut new_branches = Vec::with_capacity(Self::BRANCHES);
            for branch_idx in 0..Self::BRANCHES {
                new_branches.push(self.get_subtree_branch(
                    cache,
                    layer - 1,
                    offset + Self::branch_offset_at_layer(layer, branch_idx),
                ));
            }
            NdTreeBranch::Node(Self::with_branches(cache, new_branches))
        }
    }

    /// Returns the minimum layer that can compute the transition function for
    /// its centered cells making up a node one layer smaller.
    ///
    /// In general, a node at layer `L` can simulate any automaton with a radius
    /// `r` if `r <= 2**L / 4`. An exception is made for `r = 0`, which requires
    /// layer 2 rather than layer 0 or 1.
    pub fn min_sim_layer<R: Rule<T, D>>(rule: &R) -> usize {
        let r = rule.radius();
        let mut min_layer = 2;
        while r > (1 << min_layer) / 4 {
            min_layer += 1;
        }
        min_layer
    }
    fn sim_subtree_one_gen<R: Rule<T, D>>(
        &self,
        cache: &mut NdTreeCache<T, D>,
        rule: &R,
        layer: usize,
        offset: NdVec<D>,
    ) -> NdTreeBranch<T, D> {
        if layer == 0 {
            NdTreeBranch::Leaf(
                rule.transition(&NdTreeSlice::new(self.clone().intern(cache), offset)),
            )
        } else {
            let mut branches = Vec::with_capacity(Self::BRANCHES);
            for branch_idx in 0..Self::BRANCHES {
                branches.push(self.sim_subtree_one_gen(
                    cache,
                    rule,
                    layer - 1,
                    offset + Self::branch_offset_at_layer(layer, branch_idx),
                ));
            }
            NdTreeBranch::Node(NdTreeNode::with_branches(cache, branches))
        }
    }
    /// Computes the offset node one layer lower after `2 ** gen_pow` generations.
    pub fn sim_inner<R: Rule<T, D>>(
        &self,
        cache: &mut NdTreeCache<T, D>,
        rule: &R,
        gen_pow: usize,
    ) -> NdSubTree<T, D> {
        let max_gens = (1 << self.layer) >> Self::min_sim_layer(rule);
        let generations = 1 << gen_pow;
        if generations > max_gens {
            panic!(
                "Cannot simulate {} generations at layer {} with radius {}; can only simulate {} generation(s) at this layer or 1 generation at layer {}",
                generations,
                self.layer,
                rule.radius(),
                max_gens,
                Self::min_sim_layer(rule),
            );
        }

        // HashLife is much easier to explain using pictures; I'll be
        // referencing Figure 4 in this blog post:
        // https://www.drdobbs.com/184406478

        // This is the blue rectangle in Figure 4.
        let inner_rect = self.rect() / 2 + (self.len() as isize / 4);

        if max_gens == 1 {
            // If we're currently at the minimum layer, then we have no choice but
            // to simulate each cell individually. This is the recursive base case.
            let sim_result =
                self.sim_subtree_one_gen(cache, rule, self.layer - 1, inner_rect.min());
            if let NdTreeBranch::Node(node) = sim_result {
                node
            } else {
                panic!("Simulation produced leaf when expecting node");
            }
        } else if generations < max_gens {
            // If the timescale is short enough, delegate to each branch.
            let mut branches = Vec::with_capacity(Self::BRANCHES);
            for branch_idx in 0..Self::BRANCHES {
                // Fetch the subtree whose inner square is one of the green
                // squares in Figure 4, and then simulate it to get the green
                // square.
                branches.push(NdTreeBranch::Node(
                    self.get_subtree(
                        cache,
                        self.layer - 1,
                        inner_rect.min() / 2
                            + Self::branch_offset_at_layer(self.layer - 1, branch_idx),
                    )
                    .sim_inner(cache, rule, gen_pow),
                ));
            }
            Self::with_branches(cache, branches)
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
                    let sub_branch_outer = self.get_subtree(
                        cache,
                        self.layer - 1,
                        Self::branch_offset_at_layer(self.layer - 2, branch_idx)
                            + Self::branch_offset_at_layer(self.layer - 2, sub_branch_idx),
                    );
                    // Now simulate that blue-sized square into a red square.
                    let sub_branch = sub_branch_outer.sim_inner(cache, rule, generations / 2);
                    sub_branches.push(NdTreeBranch::Node(sub_branch));
                }
                // Now we have all the red squares for this green square in
                // sub_branches. Combine them into one node and then simulate it
                // to get the green square.
                let branch_outer = Self::with_branches(cache, sub_branches);
                // Now simulate to get the final fully-simulated green square.
                let branch = branch_outer.sim_inner(cache, rule, generations / 2);
                branches.push(NdTreeBranch::Node(branch));
            }
            // Now we have all of the green squares, so combine them into the
            // blue square (the final result).
            Self::with_branches(cache, branches)
        }
    }
}

impl<T: CellType, D: Dim> Index<NdVec<D>> for NdTreeNode<T, D> {
    type Output = T;
    fn index(&self, pos: NdVec<D>) -> &T {
        match &self.branches[self.branch_idx(pos)] {
            NdTreeBranch::Leaf(cell_state) => cell_state,
            NdTreeBranch::Node(node) => &node[pos],
        }
    }
}

impl<T: CellType, D: Dim> Eq for NdTreeNode<T, D> {}
impl<T: CellType, D: Dim> PartialEq for NdTreeNode<T, D> {
    fn eq(&self, rhs: &Self) -> bool {
        // Check for pointer equality (very fast; guarantees true).
        std::ptr::eq(self, rhs)
            // If that fails, check hash codes (very fast; guarantees false).
            || (self.hash_code() == rhs.hash_code()
                // If neither of those worked, we have to check the hard way.
                && self.layer() == rhs.layer()
                && self.branches() == rhs.branches())
    }
}

impl<T: CellType, D: Dim> Hash for NdTreeNode<T, D> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        // We already cached our own hash; just rehash that if you want to.
        self.hash_code().hash(hasher);
    }
}
