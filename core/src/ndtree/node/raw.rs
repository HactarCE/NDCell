use std::hash::Hash;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU8, AtomicUsize, Ordering::Relaxed};

use super::utils;
use super::NodeRepr;
use crate::Dim;

// TODO: document struct & methods and explain invariants
#[derive(Debug)]
pub struct RawNode {
    /// Layer of this node.
    ///
    /// `1 << layer` gives the number of cells along each dimension. For
    /// example, a 3D node at layer 5 is 32x32x32.
    layer: u32,
    /// Linear combination of spacetime residues of this node.
    ///
    /// Each spacetime residue is an arbitrary linear combination of spatial
    /// positions and temporal position (global generation count), modulo some
    /// value. The exact combinations used depend on the automaton.
    residue: u8,
    /// Flags, used for GC and various other things.
    flags: NodeFlags,
    /// Number of bytes in the slice containing this node's data (stored
    /// separately as a u16 to save space). This can be inferred from the layer
    /// and NodeRepr, but we don't have access to to the NodeRepr during
    /// deconstruction.
    slice_len: u16,
    /// Slice data, which contains cells if this is a leaf node or pointers to
    /// other nodes if this is a non-leaf node.
    ///
    /// If this is a leaf node, the slice contains (1 << base_layer)^NDIM cells.
    /// For example, a 3D leaf node at layer 2 contains (1 << 2)^3 = 64 cells.
    /// All leaf nodes have the same layer, which is determined by base_layer in
    /// the NodeRepr.
    ///
    /// If this is a non-leaf node, the contains pointers to 2^NDIM other nodes
    /// of the layer below. For example, a 3D non-leaf node at layer 5 contains
    /// 2^3 = 8 pointers to nodes at layer 4, each comprising one octant of the
    /// node at layer 5.
    slice_ptr: NonNull<u8>,
    /// Pointer to the node one layer below this one that results from
    /// simulating this node some fixed number of generations. This pointer may
    /// be null if that result hasn't been computed yet.
    result_ptr: AtomicUsize,
}
impl Drop for RawNode {
    fn drop(&mut self) {
        unsafe { std::ptr::drop_in_place(self.cell_slice_ptr().as_ptr()) };
    }
}
impl Hash for RawNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.layer.hash(state);
        self.residue.hash(state);
        // Children and cells are all the same for empty nodes.
        if !self.is_empty() {
            unsafe { self.cell_slice() }.hash(state);
        }
    }
}
impl PartialEq for RawNode {
    fn eq(&self, other: &Self) -> bool {
        self.layer == other.layer
            && self.residue == other.residue
            && (self.is_empty() == other.is_empty()
                || self.slice_ptr == other.slice_ptr
                || unsafe { self.cell_slice() == other.cell_slice() })
    }
}
impl Eq for RawNode {}
impl RawNode {
    pub unsafe fn new_empty_placeholder<D: Dim>(layer: u32) -> Self {
        Self {
            layer,
            residue: 0,
            flags: NodeFlags::with_empty(true),
            slice_len: 0,
            slice_ptr: NonNull::dangling(),
            result_ptr: AtomicUsize::new(0),
        }
    }
    pub fn new_leaf<D: Dim>(repr: &NodeRepr<D>, cells: Box<[u8]>) -> Self {
        let layer = cells.len().trailing_zeros() / D::NDIM as u32;
        assert_eq!(cells.len(), super::math::node_num_cells::<D>(layer));
        for &cell in &*cells {
            assert!((cell as usize) < repr.state_count);
        }
        let packed_cells = utils::pack_cells(cells, repr.bits_per_cell);
        let cell_count = packed_cells.len();
        let is_empty = packed_cells.iter().all(|&x| x == 0);
        let start_of_cells = &Box::leak(packed_cells)[0];

        Self {
            layer: repr.base_layer,
            residue: 0,
            flags: NodeFlags::with_empty(is_empty),
            slice_len: cell_count as u16,
            slice_ptr: NonNull::from(start_of_cells),
            result_ptr: AtomicUsize::new(0),
        }
    }
    pub fn new_non_leaf<D: Dim>(repr: &NodeRepr<D>, children: Box<[&RawNode]>) -> Self {
        assert_eq!(D::BRANCHING_FACTOR, children.len());
        let layer = children[0].layer + 1;
        for child in &children[1..] {
            debug_assert_eq!(layer - 1, child.layer());
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
    pub fn with_parity(mut self, residue: u8) -> Self {
        self.residue = residue;
        self
    }

    pub fn layer(&self) -> u32 {
        self.layer
    }
    pub fn is_empty(&self) -> bool {
        unsafe { self.flags() }.is_empty()
    }
    pub unsafe fn flags(&self) -> &NodeFlags {
        &self.flags
    }
    pub fn result(&self) -> *const RawNode {
        self.result_ptr.load(Relaxed) as *const RawNode
    }
    pub fn set_result(&self, result_ptr: *const RawNode) {
        self.result_ptr.store(result_ptr as usize, Relaxed);
    }

    fn cell_slice_ptr(&self) -> NonNull<[u8]> {
        NonNull::new(std::ptr::slice_from_raw_parts_mut(
            self.slice_ptr.as_ptr(),
            self.slice_len as usize,
        ))
        .unwrap()
    }
    fn node_slice_ptr(&self) -> NonNull<[NonNull<RawNode>]> {
        NonNull::new(std::ptr::slice_from_raw_parts_mut(
            self.slice_ptr.as_ptr().cast(),
            self.slice_len as usize / std::mem::size_of::<NonNull<RawNode>>(),
        ))
        .unwrap()
    }

    pub unsafe fn cell_slice(&self) -> &[u8] {
        self.cell_slice_ptr().as_ptr().as_ref().unwrap()
    }
    pub unsafe fn node_ptr_slice(&self) -> &[NonNull<RawNode>] {
        self.node_slice_ptr().as_ptr().as_ref().unwrap()
    }

    /// Returns a new node with the same contents as this one. This is marked as
    /// unsafe because many parts of this crate depend on nodes not implementing
    /// `Copy`/`Clone`, as there should generally only be one canonical instance
    /// of a node for any given NdTree.
    pub unsafe fn copy(&self) -> Self {
        // TODO: "safety" heading
        Self {
            layer: self.layer,
            residue: self.residue,
            flags: NodeFlags::with_empty(self.is_empty()),
            slice_len: self.slice_len,
            slice_ptr: self.slice_ptr,
            result_ptr: AtomicUsize::new(0),
        }
    }

    /// Mark this node and all nodes reachable from it to be kept during garbage
    /// collection. This should only be called at the beginning of GC.
    pub unsafe fn mark_gc_keep_recursive<D: Dim>(&self, node_repr: &NodeRepr<D>) {
        // TODO: mention safety
        let already_marked = self.flags().mark_gc_keep();
        if !already_marked {
            if let Some(res) = self.result().as_ref() {
                res.mark_gc_keep_recursive(node_repr);
            }
            if self.layer > node_repr.base_layer {
                for child in self.node_ptr_slice() {
                    child.as_ref().mark_gc_keep_recursive(node_repr);
                }
            }
        }
    }
}

const NODE_FLAG_REACHABLE: u8 = 1 << 0;
const NODE_FLAG_EMPTY: u8 = 1 << 1;

/// Flags, used for garbage collection and various other things.
///
/// TODO: explain exactly what each means, either here or on the consts
#[derive(Debug)]
pub struct NodeFlags(AtomicU8);
impl NodeFlags {
    pub fn with_empty(is_empty: bool) -> Self {
        Self(AtomicU8::from(if is_empty { NODE_FLAG_EMPTY } else { 0 }))
    }

    /// Mark this node as reachable for the purposes of garbage collection and
    /// returns the previous value of the flag.
    pub fn mark_gc_keep(&self) -> bool {
        self.0.fetch_or(NODE_FLAG_REACHABLE, Relaxed) & NODE_FLAG_REACHABLE != 0
    }
    /// Clear garbage collection flags from this node and returns the previous
    /// value of the flag.
    pub fn clear_gc(&self) -> bool {
        self.0.fetch_and(!NODE_FLAG_REACHABLE, Relaxed) & NODE_FLAG_REACHABLE != 0
    }

    pub fn is_empty(&self) -> bool {
        self.0.load(Relaxed) & NODE_FLAG_EMPTY != 0
    }
}
