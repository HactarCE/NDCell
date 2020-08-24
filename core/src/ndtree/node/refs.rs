use itertools::Itertools;
use size_hint::HintSize;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ptr::NonNull;

use super::{Layer, NodeCache, NodeCacheAccess, NodeFlags, NodeRepr, RawNode};
use crate::dim::Dim;
use crate::ndrect::{BigRect, URect};
use crate::ndvec::{BigVec, UVec};
use crate::num::{BigInt, BigUint, Integer};

/// Common functionality for references to NdTree nodes.
pub trait Node<'cache, D: Dim>: Copy {
    /// Returns a reference to the raw node structure.
    fn as_raw(self) -> &'cache RawNode;
    /// Returns a `NodeRef` of this node.
    fn as_ref(self) -> NodeRef<'cache, D>;
    /// Returns a `NodeRefEnum` of this node.
    fn as_enum(self) -> NodeRefEnum<'cache, D>;

    /// Returns the leaf node wrapped in `Some` if this is a leaf node, or
    /// `None` if it is not.
    #[inline]
    fn as_leaf(self) -> Option<LeafNodeRef<'cache, D>> {
        match self.as_enum() {
            NodeRefEnum::Leaf(node) => Some(node),
            NodeRefEnum::NonLeaf(_) => None,
        }
    }
    /// Returns the non-leaf node wrapped in `Some` if this is a non-leaf node,
    /// or `None` if it is a leaf node.
    #[inline]
    fn as_non_leaf(self) -> Option<NonLeafNodeRef<'cache, D>> {
        match self.as_enum() {
            NodeRefEnum::Leaf(_) => None,
            NodeRefEnum::NonLeaf(node) => Some(node),
        }
    }
    /// Returns true if this node is a leaf node, or false otherwise.
    #[inline]
    fn is_leaf(self) -> bool {
        self.as_leaf().is_some()
    }
    /// Returns true if this node is a non-leaf, or false otherwise.
    #[inline]
    fn is_non_leaf(self) -> bool {
        self.as_non_leaf().is_some()
    }
    /// Returns true if all cells in this node are state #0, or false if any is
    /// not.
    ///
    /// This is O(1) because each node stores a flag to indicate if it is empty.
    #[inline]
    fn is_empty(self) -> bool {
        unsafe { self.flags() }.is_empty()
    }

    fn cache(self) -> &'cache NodeCache<D>;
    // TODO: document
    fn cache_access(self) -> &'cache NodeCacheAccess<'cache, D>;
    /// Returns details about the representation of nodes in this automaton.
    #[inline]
    fn repr(self) -> &'cache NodeRepr<D> {
        self.cache().node_repr()
    }

    /// Returns the layer of this node.
    #[inline]
    fn layer(self) -> Layer {
        self.as_raw().layer()
    }
    /// Returns the flags of this node. This is marked as unsafe because
    /// modifying node flags outside of garbage collection may make garbage
    /// collection unsafe.
    #[inline]
    unsafe fn flags(self) -> &'cache NodeFlags {
        self.as_raw().flags()
    }
    #[inline]
    fn result(self) -> Option<NodeRef<'cache, D>> {
        unsafe {
            self.as_raw()
                .result()
                .as_ref()
                .map(|ptr| NodeRef::new(self.cache(), self.cache_access(), ptr))
        }
    }
    #[inline]
    fn set_result(self, result: Option<NodeRef<D>>) {
        self.as_raw().set_result(
            result
                .map(|node| node.as_raw() as *const _)
                .unwrap_or(std::ptr::null()),
        )
    }

    #[inline]
    fn big_len(self) -> BigInt {
        self.layer().big_len()
    }
    #[inline]
    fn big_num_cells(self) -> BigInt {
        self.layer().big_num_cells::<D>()
    }
    #[inline]
    fn big_rect(self) -> BigRect<D> {
        self.layer().big_rect()
    }
    #[inline]
    fn modulo_pos(self, pos: &BigVec<D>) -> BigVec<D> {
        pos & &(self.big_len() - 1)
    }

    /// Returns the cell at the given position, modulo the node length along
    /// each axis.
    #[inline]
    fn cell_at_pos(self, pos: &BigVec<D>) -> u8 {
        // Modulo the node length along each axis using bitwise AND.
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

    #[inline]
    fn population(self) -> &'cache BigUint {
        todo!("Compute population")
    }

    #[inline]
    fn subdivide(self) -> Result<Vec<NodeRef<'cache, D>>, u8> {
        self.cache_access().subdivide(self)
    }
    #[inline]
    fn centered_inner(self) -> Result<NodeRef<'cache, D>, ()> {
        self.cache_access().centered_inner(self)
    }
    #[inline]
    fn set_cell(self, pos: &BigVec<D>, cell_state: u8) -> NodeRef<'cache, D> {
        self.cache_access().set_cell(self, pos, cell_state)
    }
}

/// Reference to a node.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum NodeRefEnum<'cache, D: Dim> {
    Leaf(LeafNodeRef<'cache, D>),
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

/// Reference to a leaf node.
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

    #[inline]
    pub fn len(&self) -> usize {
        // TODO: exclusive to leaf nodes
        self.layer().len().unwrap_or_else(|| unreachable!()).get()
    }
    #[inline]
    pub fn num_cells(&self) -> usize {
        // TODO: exclusive to leaf nodes
        self.cells().len()
    }
    #[inline]
    pub fn rect(&self) -> URect<D> {
        // TODO: exclusive to leaf nodes
        self.layer().rect().unwrap_or_else(|| unreachable!())
    }
    #[inline]
    pub fn strides(&self) -> UVec<D> {
        // TODO: exclusive to leaf nodes
        self.layer().leaf_strides()
    }

    /// Returns the cells of this node.
    #[inline]
    pub fn cells(self) -> &'cache [u8] {
        unsafe { self.as_raw().cell_slice() }
    }
}

/// Reference to a non-leaf node.
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
    /// Returns the index of the child of this node containing the given
    /// position.
    #[inline]
    pub fn child_index_with_pos(&self, pos: &BigVec<D>) -> usize {
        self.layer().non_leaf_child_index(pos)
    }

    /// Returns the array of pointers to this node's children, each of which is
    /// one layer below this node.
    #[inline]
    pub fn raw_children(self) -> &'cache [NonNull<RawNode>] {
        unsafe { &self.as_raw().node_ptr_slice() }
    }
    /// Returns an iterator over this node's children, each of which is one
    /// layer below this node.
    #[inline]
    pub fn children(self) -> impl Iterator<Item = NodeRef<'cache, D>> {
        (0..D::BRANCHING_FACTOR).map(move |i| self.child_at_index(i))
    }
    /// Returns a reference to the child node at the given index.
    ///
    /// Panics if the index is not between 0 and 2^NDIM-1.
    #[inline]
    pub fn child_at_index(self, index: usize) -> NodeRef<'cache, D> {
        assert!(index < D::BRANCHING_FACTOR);
        unsafe {
            NodeRef::new(
                self.cache(),
                self.cache_access(),
                self.raw_children()[index].as_ref(),
            )
        }
    }
    /// Returns a reference to the child node containing the given position,
    /// modulo the length of this node along each axis.
    #[inline]
    pub fn child_with_pos(self, pos: &BigVec<D>) -> NodeRef<'cache, D> {
        self.child_at_index(self.layer().non_leaf_child_index(pos))
    }
    /// Returns a reference to the grandchild node at the given index wrapped in
    /// `Ok`, or the child that would contain it wrapped in `Err` if this node's
    /// children are leaf nodes.
    ///
    /// Panics if the index is not between 0 and 4^NDIM-1.
    #[inline]
    pub fn grandchild_at_index(
        self,
        index: usize,
    ) -> Result<NodeRef<'cache, D>, LeafNodeRef<'cache, D>> {
        todo!("Test this");
        assert!(index < D::BRANCHING_FACTOR * D::BRANCHING_FACTOR);

        // Split the index into child and grandchild indices.
        let grandchild_pos = Layer(2).leaf_pos::<D>(index);
        let child_index = Layer(1).leaf_cell_index(grandchild_pos >> 1);
        let grandchild_index = Layer(1).leaf_cell_index(grandchild_pos & 1);

        match self.child_at_index(child_index).as_enum() {
            NodeRefEnum::Leaf(node) => Err(node),
            NodeRefEnum::NonLeaf(node) => Ok(node.child_at_index(grandchild_index)),
        }
    }
}

// TODO: document and explain invariants
// TODO: also mention how equality works (by pointer, not value)
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
        let cell_slice_len = unsafe { self.raw_node.cell_slice().len() };
        if self.layer() <= repr.base_layer() {
            debug_assert_eq!(cell_slice_len, self.layer().num_cells::<D>().unwrap().get());
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
    pub unsafe fn new(
        cache: &'cache NodeCache<D>,
        cache_access: &'cache NodeCacheAccess<'cache, D>,
        raw_node: &'_ RawNode,
    ) -> Self {
        // TODO: explain why this is unsafe (all args must be from same cache)
        // TODO: justify lifetime lengthening for &RawNode
        // TODO: consider taking NonNull<RawNode> instead of reference
        Self {
            cache,
            cache_access,
            raw_node: std::mem::transmute::<&'_ RawNode, &'cache RawNode>(raw_node),
        }
    }
}

#[derive(Debug)]
pub struct NodeChildren<'node, D: Dim> {
    node: NonLeafNodeRef<'node, D>,
    slice: &'node [NonNull<RawNode>],
    node_repr: &'node NodeRepr<D>,
    next_index: usize,
}
impl<'node, D: Dim> Iterator for NodeChildren<'node, D> {
    type Item = NodeRef<'node, D>;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let child = self.slice.get(self.next_index);
        if child.is_some() {
            self.next_index += 1
        };
        child.map(unsafe {
            |&ptr| NodeRef::new(self.node.cache(), self.node.cache_access(), ptr.as_ref())
        })
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let ret = D::BRANCHING_FACTOR - self.next_index;
        (ret, Some(ret))
    }
}
impl<'node, D: Dim> ExactSizeIterator for NodeChildren<'node, D> {}
