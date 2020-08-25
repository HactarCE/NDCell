//! References to nodes in a cache.

use std::fmt;
use std::hash::{Hash, Hasher};
use std::ptr::NonNull;

use super::{Layer, NodeCache, NodeCacheAccess, NodeFlags, NodeRepr, RawNode};
use crate::dim::Dim;
use crate::ndrect::{BigRect, URect};
use crate::ndvec::{BigVec, UVec};
use crate::num::{BigInt, BigUint};

/// Common functionality for references to ND-tree nodes.
pub trait Node<'cache, D: Dim>: Copy {
    /// Returns a reference to the raw node structure.
    fn as_raw(self) -> &'cache RawNode;
    /// Returns a `NodeRef` of the node.
    fn as_ref(self) -> NodeRef<'cache, D>;
    /// Returns a `NodeRefEnum` of the node.
    fn as_enum(self) -> NodeRefEnum<'cache, D>;

    /// Returns the leaf node wrapped in `Some` if it is a leaf node, or `None`
    /// if it is not.
    #[inline]
    fn as_leaf(self) -> Option<LeafNodeRef<'cache, D>> {
        match self.as_enum() {
            NodeRefEnum::Leaf(node) => Some(node),
            NodeRefEnum::NonLeaf(_) => None,
        }
    }
    /// Returns the non-leaf node wrapped in `Some` if it is a non-leaf node, or
    /// `None` if it is a leaf node.
    #[inline]
    fn as_non_leaf(self) -> Option<NonLeafNodeRef<'cache, D>> {
        match self.as_enum() {
            NodeRefEnum::Leaf(_) => None,
            NodeRefEnum::NonLeaf(node) => Some(node),
        }
    }
    /// Returns `true` if the node is a leaf node.
    #[inline]
    fn is_leaf(self) -> bool {
        self.as_leaf().is_some()
    }
    /// Returns `true` if the node is a non-leaf node.
    #[inline]
    fn is_non_leaf(self) -> bool {
        self.as_non_leaf().is_some()
    }
    /// Returns `true` if all cells in the node are state #0.
    ///
    /// This is O(1) because each node stores a flag to indicate if it is empty.
    #[inline]
    fn is_empty(self) -> bool {
        unsafe { self.flags() }.is_empty()
    }

    /// Returns the cache that the node is stored in.
    fn cache(self) -> &'cache NodeCache<D>;
    /// Returns node access for the cache that the node is stored in.
    fn cache_access(self) -> &'cache NodeCacheAccess<'cache, D>;
    /// Returns the representation of the node.
    #[inline]
    fn repr(self) -> &'cache NodeRepr<D> {
        self.cache().node_repr()
    }
    /// Asserts that both nodes are from the same cache.
    #[inline]
    fn assert_same_cache<'a>(self, other: impl Node<'a, D>) {
        assert!(
            std::ptr::eq(self.cache(), other.cache()),
            "Attempt to operate on nodes from different caches",
        );
    }

    /// Returns the layer of the node.
    #[inline]
    fn layer(self) -> Layer {
        self.as_raw().layer()
    }
    /// Returns the flags of the node.
    ///
    /// # Safety
    ///
    /// Modifying these flags can break invariants (such as the flag indicating
    /// whether the node is empty) or interfere with garbage collection.
    #[inline]
    unsafe fn flags(self) -> &'cache NodeFlags {
        self.as_raw().flags()
    }
    #[inline]
    /// Returns the node one layer below this one that results from simulating
    /// this node some fixed number of generations, or `None` if that result
    /// hasn't been computed yet.
    fn result(self) -> Option<NodeRef<'cache, D>> {
        unsafe {
            NonNull::new(self.as_raw().result() as *mut _)
                .map(|ptr| NodeRef::new(self.cache(), self.cache_access(), ptr))
        }
    }
    /// Atomically sets the result of simulating this node for some fixed number
    /// of generations.
    ///
    /// # Panics
    ///
    /// Panics if `result` is from a different node cache.
    #[inline]
    fn set_result<'a>(self, result: Option<impl Node<'a, D>>) {
        if let Some(res) = result {
            // Nodes must not contain pointers to nodes in other caches.
            self.assert_same_cache(res);
        }
        self.as_raw().set_result(
            result
                .map(|node| node.as_raw() as *const _)
                .unwrap_or(std::ptr::null()),
        )
    }

    /// Returns the number of cells along each axis of the node as a `BigInt`.
    #[inline]
    fn big_len(self) -> BigInt {
        self.layer().big_len()
    }
    /// Returns the number of cells along each axis of the node as a `BigInt`.
    #[inline]
    fn big_num_cells(self) -> BigInt {
        self.layer().big_num_cells::<D>()
    }
    /// Returns a rectangle the size of the node with the lower corner at the
    /// origin as a `BigRect`.
    #[inline]
    fn big_rect(self) -> BigRect<D> {
        self.layer().big_rect()
    }

    /// Returns the given position modulo the size of the node along each axis.
    #[inline]
    fn modulo_pos(self, pos: &BigVec<D>) -> BigVec<D> {
        pos & &(self.big_len() - 1)
    }

    /// Returns the cell at the given position, modulo the node length along
    /// each axis.
    #[inline]
    fn cell_at_pos(self, pos: &BigVec<D>) -> u8 {
        // Modulo the node length along each axis.
        let pos = self.modulo_pos(pos);
        match self.as_enum() {
            // If this is a leaf node, the position must be small enough to
            // convert to a UVec.
            NodeRefEnum::Leaf(node) => node.cells()[node.pos_to_cell_index(pos.to_uvec())],
            // If this is not a leaf node, delegate to one of the children.
            NodeRefEnum::NonLeaf(node) => node
                .child_at_index(node.child_index_with_pos(&pos))
                .cell_at_pos(&pos),
        }
    }

    /// Returns the population of the node.
    #[inline]
    fn population(self) -> &'cache BigUint {
        todo!("Compute population")
    }

    /// Subdivides the node into 2^NDIM smaller nodes at one layer lower. If the
    /// node is only a single cell, returns `Err()` containing that cell state.
    #[inline]
    fn subdivide(self) -> Result<Vec<NodeRef<'cache, D>>, u8> {
        self.cache_access().subdivide(self)
    }
    /// Creates a node one layer lower containing the contents of the center of
    /// the node.
    #[inline]
    fn centered_inner(self) -> Result<NodeRef<'cache, D>, ()> {
        self.cache_access().centered_inner(self)
    }
    /// Creates an identical node except with the cell at the given position
    /// (modulo the node length along each axis) modified.
    #[inline]
    fn set_cell(self, pos: &BigVec<D>, cell_state: u8) -> NodeRef<'cache, D> {
        self.cache_access().set_cell(self, pos, cell_state)
    }
}

/// Enumeration of references to leaf or non-leaf nodes.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum NodeRefEnum<'cache, D: Dim> {
    /// Leaf node.
    Leaf(LeafNodeRef<'cache, D>),
    /// Non-leaf node.
    NonLeaf(NonLeafNodeRef<'cache, D>),
}
impl<'cache, D: Dim> From<LeafNodeRef<'cache, D>> for NodeRefEnum<'cache, D> {
    #[inline]
    fn from(node: LeafNodeRef<'cache, D>) -> Self {
        Self::Leaf(node)
    }
}
impl<'cache, D: Dim> From<NonLeafNodeRef<'cache, D>> for NodeRefEnum<'cache, D> {
    #[inline]
    fn from(node: NonLeafNodeRef<'cache, D>) -> Self {
        Self::NonLeaf(node)
    }
}
impl<'cache, D: Dim> Node<'cache, D> for NodeRefEnum<'cache, D> {
    #[inline]
    fn as_raw(self) -> &'cache RawNode {
        self.as_ref().as_raw()
    }
    #[inline]
    fn as_ref(self) -> NodeRef<'cache, D> {
        match self {
            NodeRefEnum::Leaf(node) => node.as_ref(),
            NodeRefEnum::NonLeaf(node) => node.as_ref(),
        }
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'cache, D> {
        self
    }
    #[inline]
    fn cache(self) -> &'cache NodeCache<D> {
        self.as_ref().cache()
    }
    #[inline]
    fn cache_access(self) -> &'cache NodeCacheAccess<'cache, D> {
        self.as_ref().cache_access()
    }
}

/// Reference to a leaf node in a cache.
///
/// Cells are stored in a flattened array in row-major order.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct LeafNodeRef<'cache, D: Dim>(NodeRef<'cache, D>);
impl<'cache, D: Dim> Node<'cache, D> for LeafNodeRef<'cache, D> {
    #[inline]
    fn as_raw(self) -> &'cache RawNode {
        self.0.as_raw()
    }
    #[inline]
    fn as_ref(self) -> NodeRef<'cache, D> {
        self.0
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'cache, D> {
        self.into()
    }
    #[inline]
    fn cache(self) -> &'cache NodeCache<D> {
        self.0.cache()
    }
    #[inline]
    fn cache_access(self) -> &'cache NodeCacheAccess<'cache, D> {
        self.0.cache_access()
    }
}
impl<'cache, D: Dim> LeafNodeRef<'cache, D> {
    /// Returns the flattened cell index corresponding to the given position in
    /// a leaf node, modulo the node length along each axis.
    #[inline]
    pub fn pos_to_cell_index(&self, pos: UVec<D>) -> usize {
        self.layer().leaf_cell_index(pos)
    }
    /// Returns the vector position corresponding to the given cell index in a
    /// leaf node.
    ///
    /// Panics if the index is out of range.
    #[inline]
    pub fn cell_index_to_pos(&self, index: usize) -> UVec<D> {
        self.layer().leaf_pos(index)
    }

    /// Returns the number of cells along each axis of the node.
    #[inline]
    pub fn len(&self) -> usize {
        self.layer().len().unwrap_or_else(|| unreachable!())
    }
    /// Returns the total number of cells in the node.
    #[inline]
    pub fn num_cells(&self) -> usize {
        self.cells().len()
    }
    /// Returns a rectangle the size of the node with the lower corner at the
    /// origin.
    #[inline]
    pub fn rect(&self) -> URect<D> {
        self.layer().rect().unwrap_or_else(|| unreachable!())
    }

    /// Returns the strides of the cell array for a leaf node at this layer.
    ///
    /// See the documentation at the crate root for more details.
    #[inline]
    pub fn strides(&self) -> UVec<D> {
        self.layer().leaf_strides()
    }

    /// Returns the cells of the node in a flattened array.
    ///
    /// See the documentation at the crate root for more details.
    #[inline]
    pub fn cells(self) -> &'cache [u8] {
        self.as_raw().cell_slice()
    }
}

/// Reference to a non-leaf node in a cache.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct NonLeafNodeRef<'cache, D: Dim>(NodeRef<'cache, D>);
impl<'cache, D: Dim> Node<'cache, D> for NonLeafNodeRef<'cache, D> {
    #[inline]
    fn as_raw(self) -> &'cache RawNode {
        self.0.as_raw()
    }
    #[inline]
    fn as_ref(self) -> NodeRef<'cache, D> {
        self.0
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'cache, D> {
        self.into()
    }
    #[inline]
    fn cache(self) -> &'cache NodeCache<D> {
        self.0.cache()
    }
    #[inline]
    fn cache_access(self) -> &'cache NodeCacheAccess<'cache, D> {
        self.0.cache_access()
    }
}
impl<'cache, D: Dim> NonLeafNodeRef<'cache, D> {
    /// Returns the index of the child containing the given position, module the
    /// node size along each axis.
    #[inline]
    pub fn child_index_with_pos(&self, pos: &BigVec<D>) -> usize {
        self.layer().non_leaf_child_index(pos)
    }

    /// Returns the array of pointers to the node's children, each of which is
    /// one layer below the node.
    #[inline]
    pub fn raw_children(self) -> &'cache [NonNull<RawNode>] {
        unsafe { &self.as_raw().node_ptr_slice() }
    }
    /// Returns an iterator over the node's children, each of which is one layer
    /// below this node.
    #[inline]
    pub fn children(self) -> impl Iterator<Item = NodeRef<'cache, D>> {
        (0..D::BRANCHING_FACTOR).map(move |i| self.child_at_index(i))
    }
    /// Returns a reference to the child node at the given index.
    ///
    /// Panics if the index is not between 0 and 2^NDIM (exclusive).
    #[inline]
    pub fn child_at_index(self, index: usize) -> NodeRef<'cache, D> {
        assert!(index < D::BRANCHING_FACTOR);
        unsafe {
            NodeRef::new(
                self.cache(),
                self.cache_access(),
                self.raw_children()[index],
            )
        }
    }
    /// Returns a reference to the child node containing the given position,
    /// modulo the node length along each axis.
    #[inline]
    pub fn child_with_pos(self, pos: &BigVec<D>) -> NodeRef<'cache, D> {
        self.child_at_index(self.layer().non_leaf_child_index(pos))
    }
    /// Returns a reference to the grandchild node at the given index.
    ///
    /// # Panics
    ///
    /// This method panics if the index is not between 0 and 4^NDIM (exclusive).
    #[inline]
    pub fn grandchild_at_index(self, index: usize) -> NodeRef<'cache, D> {
        assert!(index < D::BRANCHING_FACTOR * D::BRANCHING_FACTOR);
        let (child_index, grandchild_index) = split_grandchild_index::<D>(index);
        match self.child_at_index(child_index).as_enum() {
            // TODO: optimize by only creating the child requested, not all
            // 2^NDIM
            NodeRefEnum::Leaf(node) => node.subdivide().unwrap()[grandchild_index],
            NodeRefEnum::NonLeaf(node) => node.child_at_index(grandchild_index),
        }
    }
}

/// Reference to a node in a cache.
///
/// Hashing and equality are based on the pointer to the node and to the cache,
/// rather than the contents of the node. This relies on only a single
/// "canonical" instance of each unique node in the cache.
#[derive(Copy, Clone)]
pub struct NodeRef<'cache, D: Dim> {
    cache: &'cache NodeCache<D>,
    cache_access: &'cache NodeCacheAccess<'cache, D>,
    raw_node: &'cache RawNode,
}
impl<D: Dim> fmt::Debug for NodeRef<'_, D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeRef({:p})", self.raw_node)
    }
}
impl<D: Dim> PartialEq for NodeRef<'_, D> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.cache, other.cache) && std::ptr::eq(self.raw_node, other.raw_node)
    }
}
impl<D: Dim> Eq for NodeRef<'_, D> {}
impl<D: Dim> Hash for NodeRef<'_, D> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.cache, state);
        std::ptr::hash(self.raw_node, state);
    }
}
impl<'cache, D: Dim> Node<'cache, D> for NodeRef<'cache, D> {
    #[inline]
    fn as_raw(self) -> &'cache RawNode {
        self.raw_node
    }
    #[inline]
    fn as_ref(self) -> NodeRef<'cache, D> {
        self
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'cache, D> {
        let repr = self.repr();
        let cell_slice_len = self.raw_node.cell_slice().len();
        if self.layer() <= repr.base_layer() {
            debug_assert_eq!(cell_slice_len, self.layer().num_cells::<D>().unwrap());
            LeafNodeRef(self).into()
        } else {
            debug_assert_eq!(
                cell_slice_len,
                D::BRANCHING_FACTOR * std::mem::size_of::<NonNull<RawNode>>()
            );
            NonLeafNodeRef(self).into()
        }
    }
    #[inline]
    fn cache(self) -> &'cache NodeCache<D> {
        self.cache
    }
    #[inline]
    fn cache_access(self) -> &'cache NodeCacheAccess<'cache, D> {
        self.cache_access
    }
}
impl<'cache, D: Dim> NodeRef<'cache, D> {
    /// Creates a new reference to a raw node.
    ///
    /// # Safety
    ///
    /// `raw_node` must point to a valid node obtained from `cache_access`, and
    /// `cache_access` must have been obtained from `cache`.
    pub unsafe fn new(
        cache: &'cache NodeCache<D>,
        cache_access: &'cache NodeCacheAccess<'cache, D>,
        raw_node: NonNull<RawNode>,
    ) -> Self {
        // As long as we hold `cache_access`, no one is allowed to drop nodes,
        // so it's safe to extend the lifetime of the reference to `'cache`.
        // (Garbage collection acquires exclusive access to the node cache while
        // removing nodes.)
        Self {
            cache,
            cache_access,
            raw_node: std::mem::transmute::<&'_ RawNode, &'cache RawNode>(raw_node.as_ref()),
        }
    }
}

/// Iterator over the children of a non-leaf node.
#[derive(Debug)]
pub struct NodeChildrenIter<'node, D: Dim> {
    node: NonLeafNodeRef<'node, D>,
    slice: &'node [NonNull<RawNode>],
    node_repr: &'node NodeRepr<D>,
    next_index: usize,
}
impl<'node, D: Dim> Iterator for NodeChildrenIter<'node, D> {
    type Item = NodeRef<'node, D>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let child = self.slice.get(self.next_index);
        if child.is_some() {
            self.next_index += 1
        };
        child.map(|&ptr| unsafe { NodeRef::new(self.node.cache(), self.node.cache_access(), ptr) })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let ret = D::BRANCHING_FACTOR - self.next_index;
        (ret, Some(ret))
    }
}
impl<'node, D: Dim> ExactSizeIterator for NodeChildrenIter<'node, D> {}

/// Splits a grandchild index (in the range from 0 to 4^NDIM, exclusive) into
/// two child indices (each in the range from 0 to 2^NDIM, exclusive). The first
/// is the index into a node to get the child, and the second is the index into
/// that child to get the grandchild.
fn split_grandchild_index<D: Dim>(index: usize) -> (usize, usize) {
    let grandchild_pos = Layer(2).leaf_pos::<D>(index);

    // Use the upper bit along each axis.
    let child_index = Layer(1).leaf_cell_index(grandchild_pos.clone() >> 1);
    // Use the lower bit along each axis.
    let grandchild_index = Layer(1).leaf_cell_index(grandchild_pos & 1);

    (child_index, grandchild_index)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dim::Dim3D;

    /// Tests `split_grandchild_index()`.
    #[test]
    fn test_ndtree_node_split_grandchild_index() {
        for (i, pos) in Layer(2).rect::<Dim3D>().unwrap().iter().enumerate() {
            let (child_index, grandchild_index) = split_grandchild_index::<Dim3D>(i);
            println!(
                "{:?}; pos {:?}; ch {:?}; grch {:?}",
                i, pos, child_index, grandchild_index
            );
            for &ax in Dim3D::axes() {
                if pos[ax] < 2 {
                    assert_eq!(0, (child_index >> ax as usize) & 1);
                } else {
                    assert_eq!(1, (child_index >> ax as usize) & 1);
                }

                assert_eq!(pos[ax] & 1, (grandchild_index >> ax as usize) & 1);
            }
        }
    }
}
