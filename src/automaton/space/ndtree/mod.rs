use std::borrow::Borrow;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

mod cache;
mod node;
mod slice;

use super::*;
pub use cache::*;
pub use node::*;
pub use slice::*;

/// An N-dimensional generalization of a quadtree.
#[derive(Debug, Clone)]
pub struct NdTree<C: CellType, D: Dim> {
    /// The cache for this tree's nodes.
    pub cache: Rc<RefCell<NdTreeCache<C, D>>>,
    /// The slice describing the root node and offset.
    pub slice: NdTreeSlice<C, D>,
}

/// A 1D grid represented as a bintree.
pub type NdTree1D<C> = NdTree<C, Dim1D>;
/// A 2D grid represented as a quadtree.
pub type NdTree2D<C> = NdTree<C, Dim2D>;
/// A 3D grid represented as an octree.
pub type NdTree3D<C> = NdTree<C, Dim3D>;
/// A 4D grid represented as a tree with nodes of degree 16.
pub type NdTree4D<C> = NdTree<C, Dim4D>;
/// A 5D grid represented as a tree with nodes of degree 32.
pub type NdTree5D<C> = NdTree<C, Dim5D>;
/// A 6D grid represented as a tree with nodes of degree 64.
pub type NdTree6D<C> = NdTree<C, Dim6D>;

impl fmt::Display for NdTree<bool, Dim2D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let rect = self.rect();
        write!(
            f,
            "{}",
            rect.axis_range(Axis::Y)
                .map(|y| rect
                    .axis_range(Axis::X)
                    .map(|x| if self.get_cell([x, y].into()) {
                        "#"
                    } else {
                        "."
                    })
                    .collect::<Vec<&str>>()
                    .join(" "))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl<C: CellType, D: Dim> Default for NdTree<C, D> {
    fn default() -> Self {
        Self::new()
    }
}

impl<C: CellType, D: Dim> Borrow<NdTreeSlice<C, D>> for NdTree<C, D> {
    fn borrow(&self) -> &NdTreeSlice<C, D> {
        &self.slice
    }
}

impl<C: CellType, D: Dim> NdTree<C, D> {
    /// Constructs a new empty NdTree with an empty node cache centered on the
    /// origin.
    pub fn new() -> Self {
        let mut cache = NdTreeCache::default();
        let root = cache.get_empty_node(1);
        let offset = NdVec::origin() - 1;
        Self {
            cache: Rc::new(RefCell::new(cache)),
            slice: NdTreeSlice { root, offset },
        }
    }

    /// Returns the immutable slice containing at least all of the nonzero cells
    /// in the grid.
    pub fn slice(&self) -> &NdTreeSlice<C, D> {
        &self.slice
    }

    /// Returns the hyperrectangle that this NdTree spans.
    pub fn rect(&self) -> NdRect<D> {
        self.slice.rect()
    }
    /// Returns the minimum position in this NdTree.
    pub fn min(&self) -> NdVec<D> {
        self.rect().min()
    }
    /// Returns the maximum position in this NdTree.
    pub fn max(&self) -> NdVec<D> {
        self.rect().max()
    }

    /// Returns the number of non-default cells in this NdTree.
    pub fn population(&mut self) -> usize {
        self.slice.root.population
    }

    /// "Zooms out" of the current tree by a factor of 2.
    ///
    /// This is accomplished by replacing each branch with node containing the
    /// old contents of the branch in the opposite corner. For example, the NE
    /// node is replaced with a new node having the old NE node in its SW
    /// corner. The final result is that the entire tree contains the same
    /// contents as before, but with 25% padding on each edge.
    fn expand(&mut self) {
        let mut cache = self.cache.borrow_mut();
        let new_branches = self
            .slice
            .root
            .branches
            .iter()
            .enumerate()
            .map(|(branch_idx, old_branch)| {
                // Compute the index of the opposite branch (diagonally opposite
                // on all axes).
                let opposite_branch_idx = branch_idx ^ NdTreeNode::<C, D>::BRANCH_IDX_BITMASK;
                // All branches of this node will be empty ...
                let mut inner_branches = vec![
                    cache.get_empty_branch(old_branch.get_layer());
                    NdTreeNode::<C, D>::BRANCHES
                ];
                // ... except for the opposite branch, which is closest to the center.
                inner_branches[opposite_branch_idx] = old_branch.clone();
                // And return a branch with that node.
                NdTreeBranch::Node(cache.get_node(inner_branches))
            })
            .collect();
        self.slice.root = cache.get_node(new_branches);
        self.slice.offset -= self.slice.root.len() as isize / 4;
    }
    /// "Zooms out" by calling NdTree::expand() until the given position is
    /// contained in the known part of the tree, and return the number of calls
    /// to NdTree::expand() that were necessary.
    fn expand_to(&mut self, pos: NdVec<D>) -> usize {
        for i in 0.. {
            if self.rect().contains(pos) {
                return i;
            }
            self.expand();
        }
        0
    }
    /// "Zooms in" to the current tree as much as possible without losing
    /// non-empty cells. Returns the number of times the tree was contracted by
    /// a factor of 2.
    fn contract(&mut self) -> usize {
        // If we are already at the minimum layer, do not contract further.
        if self.slice.root.layer == 1 {
            return 0;
        }
        let mut cache = self.cache.borrow_mut();
        let new_branches = self
            .slice
            .root
            .branches
            .iter()
            .enumerate()
            .map(|(branch_idx, old_branch)| {
                // Do the opposite of NdTree::expand(). First compute the index
                // of the opposite branch.
                let opposite_branch_idx = branch_idx ^ NdTreeNode::<C, D>::BRANCH_IDX_BITMASK;
                match old_branch {
                    NdTreeBranch::Leaf(_) => panic!("Cannot contract tree past layer 1"),
                    NdTreeBranch::Node(old_node) => old_node.branches[opposite_branch_idx].clone(),
                }
            })
            .collect();
        let new_node = cache.get_node(new_branches);
        drop(cache);
        // Make sure the populations are the same (i.e. we haven't lost any
        // cells); otherwise don't do anything.
        if new_node.population == self.slice.root.population {
            self.slice.offset += new_node.len() as isize / 2;
            self.slice.root = new_node;
            1 + self.contract()
        } else {
            0
        }
    }
    /// Offsets the entire grid so that the given position is the new origin.
    pub fn recenter(&mut self, pos: NdVec<D>) {
        self.slice.offset -= pos;
    }

    /// Returns the state of the cell at the given position.
    pub fn get_cell(&self, pos: NdVec<D>) -> C {
        self.slice.get_cell(pos).unwrap_or_default()
    }
    /// Sets the state of the cell at the given position.
    pub fn set_cell(&mut self, pos: NdVec<D>, cell_state: C) {
        self.expand_to(pos);
        self.slice.root = self.slice.root.set_cell(
            &mut self.cache.borrow_mut(),
            pos - self.slice.offset,
            cell_state,
        );
    }

    // /// Simulate the grid for 2**gen_pow generations.
    // pub fn sim<R: Rule<C, D>>(&mut self, rule: &R, gen_pow: usize) {
    //     // // Ensure that there is enough space to actually simulate all the cells that might change.
    //     // while self.slice.root().layer() < NdTreeNode::min_sim_layer(rule) {
    //     //     self.expand_centered();
    //     // }
    //     // for _ in 0..gen_pow {
    //     //     self.expand_centered();
    //     // }
    //     // let gen_pow = 0;
    //     // let new_root = self.slice.root().sim_inner(&mut self.cache, rule, gen_pow);
    //     // let new_offset = self.slice.offset() + new_root.len() as isize / 4;
    //     // self.slice = NdTreeSlice::new(new_root, new_offset);
    //     // self.contract();
    //     unimplemented!()
    // }

    // pub fn get_non_default(&mut self) -> Vec<NdVec<D>> {
    //     // self.slice
    //     //     .root()
    //     //     .get_non_default(&mut self.cache, self.slice.offset())
    //     unimplemented!()
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use std::collections::HashMap;

    fn assert_ndtree_valid(
        expected_cells: &HashMap<Vec3D, u8>,
        ndtree: &mut NdTree3D<u8>,
        cells_to_check: &Vec<Vec3D>,
    ) {
        assert_eq!(
            expected_cells
                .iter()
                .filter(|(_, &cell_state)| cell_state != 0)
                .count(),
            ndtree.population()
        );
        for pos in cells_to_check {
            assert_eq!(
                *expected_cells.get(pos).unwrap_or(&0),
                ndtree.get_cell(*pos)
            );
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            max_shrink_iters: 4096,
            ..Default::default()
        })]

        /// Tests set_cell() and get_cell() by comparing against a HashMap.
        #[test]
        fn test_ndtree_set_get(
            cells_to_set: Vec<(Vec3D, u8)>,
            mut cells_to_get: Vec<Vec3D>,
        ) {
            let mut ndtree = NdTree::new();
            let mut hashmap = HashMap::new();
            for (pos, state) in cells_to_set {
                hashmap.insert(pos, state);
                ndtree.set_cell(pos, state);
                cells_to_get.push(pos);
            }
            assert_ndtree_valid(&hashmap, &mut ndtree, &cells_to_get);
            // Test that expansion preserves population and positions.
            let old_rect = ndtree.rect();
            while ndtree.slice.root.layer < 5 {
                ndtree.expand();
                assert_ndtree_valid(&hashmap, &mut ndtree, &cells_to_get);
            }
            // Test that contraction actually contracts.
            ndtree.contract();
            assert!(ndtree.rect().len(Axis::X) <= old_rect.len(Axis::X));
            // Test that contraction preserves population and positions.
            assert_ndtree_valid(&hashmap, &mut ndtree, &cells_to_get);
        }
    }
}
