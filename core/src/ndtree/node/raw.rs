//! Owned ND-tree node.
//!
//! This is based loosely on `hlife_algo.cpp` in Golly.

use std::hash::Hash;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU8, AtomicUsize, Ordering::Relaxed};

use super::{Layer, NodeRepr};
use crate::dim::Dim;

/// Owned ND-tree node.
///
/// Several methods of `RawNode` are `unsafe`, and it's usually better to use a
/// borrowing wrapper around one.
///
/// All `unsafe` methods on this struct cause undefined behavior if the node is
/// a non-leaf and any of its descendants has been dropped.
#[derive(Debug)]
pub struct RawNode {
    /// Layer of this node.
    layer: Layer,
    /// Linear combination of spacetime residues of this node.
    ///
    /// Each spacetime residue is an arbitrary linear combination of spatial
    /// positions and temporal position (global generation count), modulo some
    /// value. The exact combinations used depend on the automaton.
    residue: u8,
    /// Flags, used for garbage collection and various other things.
    flags: NodeFlags,
    /// Number of bytes in the slice containing this node's data (stored
    /// separately as a u16 to save space). This can be inferred from the layer
    /// and `NodeRepr`, but we don't have access to to the `NodeRepr` while
    /// being dropped.
    ///
    /// This value must be inaccurate, or else many operations that should be
    /// safe will cause UB.
    slice_len: u16,
    /// Owned slice data, which contains cells if this is a leaf node or
    /// pointers to other nodes if this is a non-leaf node.
    ///
    /// If this is a leaf node, the slice contains `layer.num_cells()` cells.
    ///
    /// If this is a non-leaf node, the slice contains pointers to 2^NDIM other
    /// nodes of the layer below. For example, a 3D non-leaf node at layer 5
    /// contains 2^3 = 8 pointers to nodes at layer 4, each comprising one
    /// octant of the node at layer 5.
    slice_ptr: NonNull<u8>,
    /// Pointer to the node one layer below this one that results from
    /// simulating this node some fixed number of generations, or null if that
    /// result hasn't been computed yet.
    ///
    /// TODO: may need some sentinel value to indicate the computation is
    /// in-progress and/or queued.
    result_ptr: AtomicUsize,
}
impl Drop for RawNode {
    #[inline]
    fn drop(&mut self) {
        unsafe { std::ptr::drop_in_place(self.cell_slice_ptr().as_ptr()) };
    }
}
impl Hash for RawNode {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.layer.hash(state);
        self.residue.hash(state);
        // Children and cells are all the same for empty nodes, so only bother
        // hashing it if the node is non-empty.
        if !self.is_empty() {
            self.cell_slice().hash(state);
        }
    }
}
impl PartialEq for RawNode {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.layer == other.layer
            && self.residue == other.residue
            && (self.is_empty() == other.is_empty()
                || self.slice_ptr == other.slice_ptr
                || self.cell_slice() == other.cell_slice())
    }
}
impl Eq for RawNode {}
impl RawNode {
    /// Creates an empty placeholder node which is equal to other empty nodes at
    /// the same layer.
    ///
    /// # Safety
    ///
    /// This node must NOT be stored in the cache or used for any other purpose
    /// than looking up the canonical empty node.
    pub unsafe fn new_empty_placeholder<D: Dim>(layer: Layer) -> Self {
        Self {
            layer,
            residue: 0,
            flags: NodeFlags::with_empty(true),
            // Since the slice length is zero, dropping the slice won't actually
            // do anything (which is good, because we didn't allocate a slice!)
            slice_len: 0,
            slice_ptr: NonNull::dangling(),
            result_ptr: AtomicUsize::new(0),
        }
    }

    /// Creates a new leaf node with the given cell contents.
    ///
    /// The layer of the node is inferred based on the number of cells.
    ///
    /// # Panics
    ///
    /// This function panics if any cell is out of range according to `repr`, if
    /// the number of cells does not match the inferred layer, or if the
    /// inferred layer is above `repr.base_layer()`.
    pub fn new_leaf<D: Dim>(repr: &NodeRepr<D>, cells: Box<[u8]>) -> Self {
        let layer = Layer::from_num_cells::<D>(cells.len()).expect("Invalid leaf node cell count");
        assert!(layer <= repr.base_layer());
        for &cell in &*cells {
            assert!((cell as usize) < repr.state_count());
        }
        let cell_count = cells.len();
        let is_empty = cells.iter().all(|&x| x == 0);
        let start_of_cells = &Box::leak(cells)[0];

        Self {
            layer: repr.base_layer(),
            residue: 0,
            flags: NodeFlags::with_empty(is_empty),
            slice_len: cell_count as u16,
            slice_ptr: NonNull::from(start_of_cells),
            result_ptr: AtomicUsize::new(0),
        }
    }
    /// Creates a new non-leaf node with the given children.
    ///
    /// The layer of the node is inferred based on the layer of the children.
    ///
    /// # Panics
    ///
    /// This function panics if the number of children is not equal to 2^NDIM,
    /// or the inferred layer is at or below `repr.base_layer()`.
    ///
    /// This function panics in debug mode if the children's layers do not
    /// match.
    pub fn new_non_leaf<D: Dim>(repr: &NodeRepr<D>, children: Box<[&RawNode]>) -> Self {
        assert_eq!(D::BRANCHING_FACTOR, children.len());
        let child_layer = children[0].layer();
        let layer = child_layer.parent_layer();
        assert!(layer > repr.base_layer());
        for child in &children[1..] {
            debug_assert_eq!(child_layer, child.layer());
        }
        let is_empty = children.iter().all(|child| child.is_empty());
        let start_of_children = &Box::leak(children)[0];

        Self {
            layer,
            residue: 0,
            flags: NodeFlags::with_empty(is_empty),
            slice_len: (D::BRANCHING_FACTOR * std::mem::size_of::<*const Self>()) as u16,
            slice_ptr: NonNull::from(start_of_children).cast(),
            result_ptr: AtomicUsize::new(0),
        }
    }

    /// Returns the same node with a different residue.
    #[inline]
    pub fn with_residue(mut self, residue: u8) -> Self {
        self.residue = residue;
        self
    }

    /// Returns the layer of the node.
    #[inline]
    pub fn layer(&self) -> Layer {
        self.layer
    }
    /// Returns `true` if all cells in the node are state #0.
    ///
    /// This is O(1) because each node stores a flag to indicate if it is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        unsafe { self.flags() }.is_empty()
    }
    /// Returns the flags of this node.
    ///
    /// # Safety
    ///
    /// Modifying these flags can break invariants (such as the flag indicating
    /// whether the node is empty) or interfere with garbage collection.
    #[inline]
    pub unsafe fn flags(&self) -> &NodeFlags {
        &self.flags
    }
    /// Returns the node one layer below this one that results from simulating
    /// this node some fixed number of generations, or null if that result
    /// hasn't been computed yet.
    #[inline]
    pub fn result(&self) -> *const RawNode {
        self.result_ptr.load(Relaxed) as *const RawNode
    }
    /// Atomically sets the result of simulating this node for some fixed number
    /// of generations.
    #[inline]
    pub fn set_result(&self, result_ptr: *const RawNode) {
        self.result_ptr.store(result_ptr as usize, Relaxed);
    }

    /// Returns a pointer to the cell state slice, assuming this is a leaf node.
    #[inline]
    fn cell_slice_ptr(&self) -> NonNull<[u8]> {
        NonNull::new(std::ptr::slice_from_raw_parts_mut(
            self.slice_ptr.as_ptr(),
            self.slice_len as usize,
        ))
        .unwrap()
    }
    /// Returns a pointer to the node pointer slice, assuming this is a non-leaf
    /// node.
    #[inline]
    fn node_slice_ptr(&self) -> NonNull<[NonNull<RawNode>]> {
        NonNull::new(std::ptr::slice_from_raw_parts_mut(
            self.slice_ptr.as_ptr().cast(),
            self.slice_len as usize / std::mem::size_of::<NonNull<RawNode>>(),
        ))
        .unwrap()
    }

    /// Returns the cell state slice, assuming this is a leaf node.
    ///
    /// This is "safe" even for non-leaf nodes, but the result is
    /// nonsense and some cell states may be too large.
    #[inline]
    pub fn cell_slice(&self) -> &[u8] {
        unsafe { self.cell_slice_ptr().as_ptr().as_ref().unwrap() }
    }
    /// Returns the node pointer slice, assuming this is a non-leaf node.
    ///
    /// # Safety
    ///
    /// This method is `unsafe` if called on a leaf node, because the slice may
    /// be the wrong length or may contain zeros (which are invalid for
    /// `NonNull`), causing UB.
    #[inline]
    pub unsafe fn node_ptr_slice(&self) -> &[NonNull<RawNode>] {
        debug_assert_eq!(
            self.slice_len as usize,
            std::mem::size_of::<NonNull<RawNode>>(),
        );
        self.node_slice_ptr().as_ptr().as_ref().unwrap()
    }

    /// Returns a new node with the same contents as this one.
    ///
    /// # Safety
    ///
    /// This is marked as `unsafe` because many parts of this crate depend on
    /// nodes not implementing `Copy`/`Clone`, as there should only ever be one
    /// canonical instance of a node for any given ND-tree.
    pub unsafe fn copy(&self) -> Self {
        let cell_slice_copy = self.cell_slice().to_vec().into_boxed_slice();
        let new_slice_ptr = &Box::leak(cell_slice_copy)[0];

        Self {
            layer: self.layer,
            residue: self.residue,
            flags: NodeFlags::with_empty(self.is_empty()),
            slice_len: self.slice_len,
            slice_ptr: NonNull::from(new_slice_ptr),
            result_ptr: AtomicUsize::new(0),
        }
    }

    /// Mark this node and all nodes reachable from it to be kept during garbage
    /// collection. This should only be called at the beginning of GC.
    ///
    /// # Safety
    ///
    /// This method causes UB if the node is a non-leaf and any of its
    /// descendants have been dropped.
    pub unsafe fn mark_gc_keep_recursive<D: Dim>(&self, node_repr: &NodeRepr<D>) {
        let already_marked = self.flags().mark_gc_keep();
        if !already_marked {
            if let Some(res) = self.result().as_ref() {
                res.mark_gc_keep_recursive(node_repr);
            }
            if self.layer > node_repr.base_layer() {
                for child in self.node_ptr_slice() {
                    child.as_ref().mark_gc_keep_recursive(node_repr);
                }
            }
        }
    }
}

/// Flag to indicate whether the node is reachable from a node that must be
/// preserved during GC, and therefore this node must also be preserved.
const NODE_FLAG_REACHABLE: u8 = 1 << 0;
/// Flag to indicate whether a node contains only cell state zero.
const NODE_FLAG_EMPTY: u8 = 1 << 1;

/// Flags, used for garbage collection and various other things.
#[derive(Debug)]
pub struct NodeFlags(AtomicU8);
impl NodeFlags {
    /// Creates flags with the given boolean indicating whether the node is
    /// empty.
    #[inline]
    pub fn with_empty(is_empty: bool) -> Self {
        Self(AtomicU8::from(if is_empty { NODE_FLAG_EMPTY } else { 0 }))
    }

    /// Returns whether the node is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.load(Relaxed) & NODE_FLAG_EMPTY != 0
    }

    /// Marks the node as reachable for the purposes of garbage collection and
    /// returns the old value of the flag.
    #[inline]
    pub fn mark_gc_keep(&self) -> bool {
        self.0.fetch_or(NODE_FLAG_REACHABLE, Relaxed) & NODE_FLAG_REACHABLE != 0
    }
    /// Clears garbage collection flags from the node and returns the old value
    /// of the flag indicating whether the node needs to be kept.
    #[inline]
    pub fn clear_gc(&self) -> bool {
        self.0.fetch_and(!NODE_FLAG_REACHABLE, Relaxed) & NODE_FLAG_REACHABLE != 0
    }
}
