use std::fmt;
use std::sync::Arc;

mod cache;
mod indexed;
mod node;
mod slice;

use super::*;
pub use cache::*;
pub use indexed::*;
pub use node::*;
pub use slice::*;

/// An N-dimensional generalization of a quadtree.
#[derive(Debug, Clone)]
pub struct NdTree<C: CellType, D: Dim> {
    /// The cache for this tree's nodes.
    pub cache: Arc<NdTreeCache<C, D>>,
    /// The slice describing the root node and offset.
    pub slice: NdTreeSlice<C, D>,
}
impl<C: CellType, D: Dim> PartialEq for NdTree<C, D> {
    fn eq(&self, other: &Self) -> bool {
        self.slice == other.slice
    }
}
impl<C: CellType, D: Dim> Eq for NdTree<C, D> {}

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

impl<C: CellType, D: Dim> fmt::Display for NdTree<C, D>
where
    NdTreeSlice<C, D>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.slice)
    }
}

impl<C: CellType, D: Dim> Default for NdTree<C, D> {
    fn default() -> Self {
        Self::new()
    }
}

impl<C: CellType, D: Dim> AsRef<NdTreeSlice<C, D>> for NdTree<C, D> {
    fn as_ref(&self) -> &NdTreeSlice<C, D> {
        &self.slice
    }
}

impl<C: CellType, D: Dim> NdTree<C, D> {
    /// Constructs a new empty NdTree with an empty node cache centered on the
    /// origin.
    pub fn new() -> Self {
        let cache = NdTreeCache::default();
        let root = cache.get_empty_node(1);
        let offset = NdVec::repeat(-1);
        Self {
            cache: Arc::new(cache),
            slice: NdTreeSlice { root, offset },
        }
    }

    /// Returns the root node of this tree.
    pub fn get_root(&self) -> &NdCachedNode<C, D> {
        &self.slice.root
    }
    /// Sets the root node of this tree.
    pub fn set_root(&mut self, new_root: NdCachedNode<C, D>) {
        self.slice.root = new_root;
    }
    /// Sets the root node of this tree and adjusts the offset so that the tree remains centered on the same point.
    pub fn set_root_centered(&mut self, new_root: NdCachedNode<C, D>) {
        self.slice.offset += &((self.get_root().len() - new_root.len()) / 2);
        self.set_root(new_root);
    }

    /// "Zooms out" of the current tree by a factor of 2.
    ///
    /// This is accomplished by replacing each branch with node containing the
    /// old contents of the branch in the opposite corner. For example, the NE
    /// node is replaced with a new node having the old NE node in its SW
    /// corner. The final result is that the entire tree contains the same
    /// contents as before, but with 25% padding on each edge.
    pub fn expand(&mut self) {
        let empty_sub_branch = self.cache.get_empty_branch(self.slice.root.layer - 1);
        let old_root = self.slice.root.clone();
        self.slice.root = self.cache.get_node_from_fn(|branch_idx| {
            let old_branch = &old_root[branch_idx.clone()];
            // Compute the index of the opposite branch (diagonally opposite
            // on all axes).
            let opposite_branch_idx = branch_idx.opposite();
            // All branches of this node will be empty ...
            let mut inner_branches = vec![empty_sub_branch.clone(); D::TREE_BRANCHES];
            // ... except for the opposite branch, which is closest to the center.
            inner_branches[opposite_branch_idx.to_array_idx()] = old_branch.clone();
            // And return a branch with that node.
            NdTreeBranch::Node(self.cache.get_node(inner_branches))
        });
        self.slice.offset -= &(self.get_root().len() / 4);
    }
    /// "Zooms out" by calling NdTree::expand() until the given position is
    /// contained in the known part of the tree, and return the number of calls
    /// to NdTree::expand() that were necessary.
    pub fn expand_to(&mut self, pos: &BigVec<D>) -> usize {
        for i in 0.. {
            if self.slice.rect().contains(pos) {
                return i;
            }
            self.expand();
        }
        unreachable!();
    }
    /// "Zooms in" to the current tree as much as possible without losing
    /// non-empty cells. Returns the number of times the tree was shrunk by a
    /// factor of 2.
    pub fn shrink(&mut self) -> usize {
        // If we are already at the minimum layer, do not shrink further.
        if self.get_root().layer == 1 {
            return 0;
        }
        let new_node = self.get_root().get_inner_node(&self.cache);
        // Make sure the populations are the same (i.e. we haven't lost any
        // cells); otherwise don't do anything.
        if new_node.population == self.get_root().population {
            self.set_root_centered(new_node);
            1 + self.shrink()
        } else {
            0
        }
    }
    /// Offsets the entire grid so that the given position is the new origin.
    pub fn recenter(&mut self, pos: BigVec<D>) {
        self.slice.offset -= pos;
    }

    /// Returns the state of the cell at the given position.
    pub fn get_cell(&self, pos: &BigVec<D>) -> C {
        self.slice.get_cell(pos).unwrap_or_default()
    }
    /// Sets the state of the cell at the given position.
    pub fn set_cell(&mut self, pos: &BigVec<D>, cell_state: C) {
        self.expand_to(&pos);
        self.slice.root =
            self.slice
                .root
                .set_cell(&self.cache, &(pos - &self.slice.offset), cell_state);
    }

    /// Returns an NdTreeSlice of the smallest node in the grid containing the
    /// given rectangle.
    ///
    /// The result may not directly correspond to an existing node; it may be
    /// centered on an existing node, and thus composed out of smaller existing
    /// nodes. The only guarantee is that the node will be less than twice as
    /// big as it needs to be.
    pub fn get_slice_containing(&mut self, rect: &BigRect<D>) -> NdTreeSlice<C, D> {
        // Ensure that the desired rectangle is contained in the whole tree.
        self.expand_to(&rect.min());
        self.expand_to(&rect.max());
        let mut slice = self.slice.clone();
        let mut smaller_slice = self.slice.clone();
        self.shrink();
        // "Zoom in" on either a corner, an edge/face, or the center until it
        // can't shrink any more. Each iteration of the loop "zooms in" by a
        // factor of 2. If we've zoomed in too far, break out of the loop and
        // use the previous one.
        while smaller_slice.rect().contains(rect) {
            slice = smaller_slice;
            // If we can't shrink any further, don't.
            if slice.root.layer == 1 {
                break;
            }
            // These variables, together with new_branch_idx later on, form a
            // single sub-branch index for use with
            // NdTreeNode::get_sub_branch().
            let mut min_sub_branch_idx = ByteVec::origin();
            let mut offset_vec = slice.offset.clone();
            // Compute the "half" (negative, centered, or positive on each axis)
            // of this node to "zoom in" to.
            let slice_rect = slice.rect();
            let pivot = slice_rect.min() + &(slice.root.len() / 2);
            for &ax in D::axes() {
                if rect.max()[ax] < pivot[ax] {
                    // If the target rect is entirely on the negative side, then
                    // let this axis of the sub-branch index be 0.
                } else if pivot[ax] <= rect.min()[ax] {
                    // If the target rect is entirely on the positive side, then
                    // let this axis of the sub-branch index be 2.
                    min_sub_branch_idx[ax] = 2;
                    offset_vec[ax] += slice.root.len() / 2;
                } else {
                    // Otherwise, it must be in the middle, so let this axis of
                    // the sub-branch index be 1.
                    min_sub_branch_idx[ax] = 1;
                    offset_vec[ax] += slice.root.len() / 4;
                }
            }
            // Now compose a new node from sub-branches selected using
            // sub_branch_idx_1 and sub_branch_idx_2.
            let smaller_node = self.cache.get_node_from_fn(|new_branch_idx| {
                slice
                    .root
                    .get_sub_branch(&min_sub_branch_idx + new_branch_idx)
                    .clone()
            });
            smaller_slice = NdTreeSlice {
                root: smaller_node,
                offset: offset_vec,
            };
        }
        return slice;
    }
}

#[cfg(test)]
mod tests {
    use num::BigInt;
    use proptest::prelude::*;
    use std::collections::HashMap;

    use super::*;

    fn assert_ndtree_valid(
        expected_cells: &HashMap<IVec2D, bool>,
        ndtree: &mut NdTree2D<bool>,
        cells_to_check: &Vec<IVec2D>,
    ) {
        assert_eq!(
            BigInt::from(
                expected_cells
                    .iter()
                    .filter(|(_, &cell_state)| cell_state)
                    .count()
            ),
            ndtree.get_root().population
        );
        for pos in cells_to_check {
            assert_eq!(
                *expected_cells.get(pos).unwrap_or(&false),
                ndtree.get_cell(&pos.convert())
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
            cells_to_set: Vec<(IVec2D, bool)>,
            mut cells_to_get: Vec<IVec2D>,
        ) {
            let mut ndtree = NdTree::new();
            let mut hashmap = HashMap::new();
            let do_it = cells_to_set.len() <= 3;
            for (pos, state) in cells_to_set {
                hashmap.insert(pos.convert(), state);
                ndtree.set_cell(&pos.convert(), state);
                if do_it {
                }
                cells_to_get.push(pos);
            }
            assert_ndtree_valid(&hashmap, &mut ndtree, &cells_to_get);
            // Test that expansion preserves population and positions.
            let old_rect = ndtree.slice.rect();
            while ndtree.slice.root.layer < 5 {
                ndtree.expand();
                assert_ndtree_valid(&hashmap, &mut ndtree, &cells_to_get);
            }
            // Test that shrinking actually shrinks.
            ndtree.shrink();
            assert!(ndtree.slice.rect().len(X) <= old_rect.len(X));
            // Test that shrinking preserves population and positions.
            assert_ndtree_valid(&hashmap, &mut ndtree, &cells_to_get);
        }

        /// Tests that NdTreeCache automatically caches identical nodes.
        #[ignore]
        #[test]
        fn test_ndtree_cache(
            cells_to_set: Vec<(IVec2D, bool)>,
        ) {
            prop_assume!(!cells_to_set.is_empty());
            let mut ndtree = NdTree::new();
            for (pos, state) in cells_to_set {
                ndtree.set_cell(&(pos - 128).convert(), state);
                ndtree.set_cell(&(pos + 128).convert(), state);
            }
            let branches = &ndtree.slice.root.branches;
            let subnode1 = branches[0].node().unwrap();
            let subnode2 = branches[branches.len() - 1].node().unwrap();
            assert_eq!(subnode1, subnode2);
            assert_eq!(subnode1.hash_code, subnode2.hash_code);
            assert!(std::ptr::eq(subnode1, subnode2));
        }

        /// Tests NdTree::get_slice_containing().
        #[test]
        fn test_ndtree_get_slice_containing(
            cells_to_set: Vec<(IVec2D, bool)>,
            center: IVec2D,
            x_radius in 0..20isize,
            y_radius in 0..20isize,
        ) {
            let mut ndtree = NdTree::new();
            let mut hashmap = HashMap::new();
            for (pos, state) in cells_to_set {
                hashmap.insert(pos, state);
                ndtree.set_cell(&pos.convert(), state);
            }
            let half_diag = NdVec([x_radius, y_radius]);
            let rect = NdRect::span(center - half_diag, center + half_diag).convert();
            let slice = ndtree.get_slice_containing(&rect);
            let slice_rect = slice.rect();
            assert!(slice_rect.contains(&rect));
            assert!(
                slice.root.layer == 1
                || slice.root.len() < rect.len(X) * 4
                || slice.root.len() < rect.len(Y) * 4
            );
            for (pos, state) in hashmap {
                if slice_rect.contains(&pos.convert()) {
                    if let Some(cell_state) = slice.get_cell(&pos.convert()) {
                        assert_eq!(state, cell_state);
                    }
                }
            }
        }
    }
}
