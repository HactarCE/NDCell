//! Owned ND-tree node.
//!
//! This is based loosely on `hlife_algo.cpp` in Golly.

use itertools::Itertools;
use std::fmt;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicUsize, Ordering::Relaxed};

use super::Layer;
use crate::dim::Dim;
use crate::num::{BigUint, MaybeBigUint};

/// Estimated overhead from the set data structure that stores nodes and from
/// slop in the allocator.
const RAW_NODE_MEMORY_OVERHEAD: usize = 3 * std::mem::size_of::<usize>();

/// Owned ND-tree node.
///
/// Several methods of `RawNode` are `unsafe`, and it's usually better to use a
/// borrowing wrapper around one.
///
/// # Memory usage
///
/// Simulating large patterns will create a lot of `RawNode<D>`, so we want to
/// optimize the memory usage of that. Rust's default allocator is jemalloc,
/// which groups small allocations into "bins" based on size, so we would like
/// `RawNode<D>` to be the smallest power-of-2 size it can be in order to save
/// RAM.
///
/// ## 64-bit
///
/// - `slice_ptr` - 8 bytes
/// - `result_ptr` - 8 bytes
/// - `population` - 8 bytes
/// - `layer` - 4 bytes
/// - `single_state` - 2 bytes
/// - `residue` - 1 byte
/// - `gc_reachable` - 1 byte
///
/// Total: 32 bytes, which exactly matches one of jemalloc's size classes, so no
/// space is wasted.
///
/// ## 32-bit
///
/// - `slice_ptr` - 4 bytes
/// - `result_ptr` - 4 bytes
/// - `population` - 4 bytes
/// - `layer` - 4 bytes
/// - `single_state` - 2 bytes
/// - `residue` - 1 byte
/// - `gc_reachable` - 1 byte
///
/// Total: 20 bytes, which isn't great but still fits within jemalloc's 32-byte
/// size class and only wastes 6 bytes. Memory optimization on 32-bit targets
/// isn't a high priority.
///
/// ## Aren't slice pointers two `usize`s?
///
/// Generally, yes! That second `usize` is the length of the slice, but we can
/// infer the length of the slice from `layer` so we don't need to carry around
/// the extra `usize`.
#[derive(Debug)]
pub(super) struct RawNode<D: Dim> {
    /// Phantom data for type variance.
    _phantom: PhantomData<D>,

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
    /// TODO: may need some flag to indicate that the computation is in-progress
    /// and/or queued.
    result_ptr: AtomicPtr<RawNode<D>>,

    /// Number of live cells.
    ///
    /// If the lowest bit is 0, this is an owned pointer to a `BigUint`; if the
    /// lowest bit is 1, the other 63 bits encode an unsigned value. A null
    /// pointer indicates that the value has not yet been computed.
    population: AtomicUsize,

    /// Layer of this node.
    layer: Layer,

    /// Cell state, if all cells in this node are the same state; `None`
    /// otherwise.
    single_state: Option<u8>,

    /// Linear combination of spacetime residues of this node.
    ///
    /// Each spacetime residue is an arbitrary linear combination of spatial
    /// positions and temporal position (global generation count), modulo some
    /// value. The exact combinations used depend on the automaton.
    residue: u8,

    /// Whether this node is reachable and thus should be preserved during
    /// garbage collection.
    ///
    /// This field is only read/written during GC, which happens on one thread
    /// at a time (enforced by requiring a `&mut NodePool`) but we make it
    /// atomic because it's hard to get a mutable reference to an element in a
    /// `HashSet`, and for good reason.
    ///
    /// The initial value of this field doesn't matter.
    gc_reachable: AtomicBool,
}

impl<D: Dim> fmt::Display for RawNode<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Node(")?;
        write!(f, "layer={}, ", self.layer.to_u32())?;
        if let Some(children) = self.children_slice() {
            write!(f, "children=[")?;
            write!(f, "{:#p}", children[0])?;
            for &child in &children[1..] {
                write!(f, ", {:#p}", child)?;
            }
            write!(f, "]")?;
        } else {
            write!(f, "cells={:?}", self.cell_slice().unwrap())?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl<D: Dim> Drop for RawNode<D> {
    #[inline]
    fn drop(&mut self) {
        if self.layer.is_leaf::<D>() {
            // Drop cell slice.
            drop(unsafe { Box::from_raw(self.cell_slice_ptr()) });
        } else {
            // Drop children slice.
            drop(unsafe { Box::from_raw(self.children_slice_ptr()) });
        }

        let pop = *self.population.get_mut();
        // If the lowest bit is 0, then it's a pointer to a `BigUint`.
        if pop != 0 && pop & 1 == 0 {
            unsafe { std::ptr::drop_in_place(pop as *mut BigUint) };
        }
    }
}

impl<D: Dim> Hash for RawNode<D> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.layer.hash(state);
        self.residue.hash(state);
        self.cell_slice().hash(state);
        for &child in self.children_slice().into_iter().flatten() {
            std::ptr::hash(child, state);
        }
    }
}

impl<D: Dim> PartialEq for RawNode<D> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // The nodes must have the same layer ...
        if self.layer != other.layer {
            return false;
        }

        // ... and the same residue
        if self.residue != other.residue {
            return false;
        }

        // ... and the same contents, which implies the same value for
        // `single_state()`
        if self.single_state != other.single_state {
            return false;
        }

        // ... and the same contents, which we can check in several ways:

        // 1. If the nodes have different values for `single_state`, they are
        //    obviously unequal.
        if self.single_state != other.single_state {
            return false;
        }

        // 2. If the nodes have equal slice pointers, they are obviously equal.
        if self.slice_ptr == other.slice_ptr {
            return true;
        }

        // 3. If the nodes have the same non-`None` value for `single_state`,
        //    they must be equal.
        if self.single_state.is_some() && self.single_state == other.single_state {
            return true;
        }

        // 4. Check the children recursively. Either `cell_slice()` or
        //    `children_slice()` will be `None`, and the other will be
        //    `Some(...)`.
        return self.cell_slice() == other.cell_slice()
            && self.children_slice() == other.children_slice();
    }
}
impl<D: Dim> Eq for RawNode<D> {}

impl<D: Dim> RawNode<D> {
    /// Creates a new empty leaf node.
    ///
    /// # Panics
    ///
    /// This function panics if `layer` is not a leaf layer.
    pub(super) fn new_empty_leaf(layer: Layer) -> Self {
        assert!(layer.is_leaf::<D>(), "Leaf node layer too large");
        let cells_vec = vec![0_u8; layer.num_cells::<D>().unwrap()];
        Self::new_leaf(cells_vec.into_boxed_slice())
    }
    /// Creates a new leaf node with the given cell contents.
    ///
    /// The layer of the node is inferred based on the number of cells.
    ///
    /// # Panics
    ///
    /// This function panics if the number of cells does not match the inferred
    /// layer, or if the inferred layer is above the base layer.
    pub(super) fn new_leaf(cells: Box<[u8]>) -> Self {
        let layer = Layer::from_num_cells::<D>(cells.len()).expect("Invalid leaf node cell count");
        assert!(layer.is_leaf::<D>(), "Leaf node layer too large");
        let single_state = if cells.iter().all_equal() {
            Some(cells[0])
        } else {
            None
        };
        let start_of_cells = &Box::leak(cells)[0];

        Self {
            _phantom: PhantomData,

            slice_ptr: NonNull::from(start_of_cells),
            result_ptr: AtomicPtr::new(std::ptr::null_mut()),
            population: AtomicUsize::new(0),

            layer,
            single_state,
            residue: 0,
            gc_reachable: AtomicBool::new(true),
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
    ///
    /// # Safety
    ///
    /// All `children` must be valid nodes stored in the same pool that this one
    /// will be stored in.
    pub(super) unsafe fn new_non_leaf(children: Box<[&RawNode<D>]>) -> Self {
        assert_eq!(
            D::BRANCHING_FACTOR,
            children.len(),
            "Invalid non-leaf node child count",
        );
        let child_layer = children[0].layer();
        let layer = child_layer.parent_layer();
        assert!(layer.is_non_leaf::<D>(), "Non-leaf node layer too small");
        for child in &children[1..] {
            debug_assert_eq!(child_layer, child.layer());
        }
        let single_state = if children.iter().map(|child| child.single_state).all_equal() {
            children[0].single_state
        } else {
            None
        };
        let start_of_children = &Box::leak(children)[0];

        Self {
            _phantom: PhantomData,

            slice_ptr: NonNull::from(start_of_children).cast(),
            result_ptr: AtomicPtr::new(std::ptr::null_mut()),
            population: AtomicUsize::new(0),

            layer,
            single_state,
            residue: 0,
            gc_reachable: AtomicBool::new(true),
        }
    }

    // /// Returns the same node with a different residue.
    // #[inline]
    // pub(super) fn with_residue(mut self, residue: u8) -> Self {
    //     self.residue = residue;
    //     self
    // }

    /// Returns the layer of the node.
    #[inline]
    pub(super) fn layer(&self) -> Layer {
        self.layer
    }

    /// Returns `true` if all cells in the node are state #0.
    ///
    /// This is O(1) because each node tracks this information.
    #[inline]
    pub(super) fn is_empty(&self) -> bool {
        self.single_state() == Some(0_u8)
    }

    /// Returns the state of all cells in the node, if they are the same, or
    /// `None` otherwise.
    ///
    /// This is O(1) because each node tracks this information.
    #[inline]
    pub(super) fn single_state(&self) -> Option<u8> {
        self.single_state
    }

    /// Returns a pointer to the cell state slice if this is a leaf node, or
    /// null otherwise.
    fn cell_slice_ptr(&self) -> *mut [u8] {
        if self.layer().is_leaf::<D>() {
            std::ptr::slice_from_raw_parts_mut(
                self.slice_ptr.as_ptr(),
                self.layer().num_cells::<D>().unwrap(),
            )
        } else {
            std::ptr::slice_from_raw_parts_mut(std::ptr::null_mut(), 0)
        }
    }
    /// Returns a pointer to the children slice if this is a non-leaf
    /// node, or null otherwise.
    fn children_slice_ptr(&self) -> *mut [&RawNode<D>] {
        std::ptr::slice_from_raw_parts_mut(
            if self.layer().is_non_leaf::<D>() {
                self.slice_ptr.as_ptr().cast()
            } else {
                std::ptr::null_mut()
            },
            D::BRANCHING_FACTOR,
        )
    }

    /// Returns the cell state slice if this is a leaf node, or `None` if it is
    /// not.
    #[inline]
    pub(super) fn cell_slice(&self) -> Option<&[u8]> {
        unsafe { self.cell_slice_ptr().as_ref() }
    }
    /// Returns the children slice if this is a non-leaf node, or `None` if it
    /// is not.
    #[inline]
    pub(super) fn children_slice(&self) -> Option<&[&RawNode<D>]> {
        unsafe { self.children_slice_ptr().as_ref() }
    }

    /// Returns the node one layer below this one that results from simulating
    /// this node some fixed number of generations, or `None` if that result
    /// hasn't been computed yet.
    #[inline]
    pub(super) fn result<'a>(&'a self) -> Option<&'a RawNode<D>> {
        unsafe { self.result_ptr.load(Relaxed).as_ref() }
    }
    /// Atomically sets the result of simulating this node for some fixed number
    /// of generations.
    ///
    /// # Safety
    ///
    /// `result_ptr` must reside in the same pool as this node, so that it will
    /// not be dropped as long as this node has a pointer to it.
    #[inline]
    pub(super) unsafe fn set_result(&self, result_ptr: Option<&RawNode<D>>) {
        self.result_ptr.store(
            result_ptr
                .map(|r| r as *const _ as *mut _)
                .unwrap_or(std::ptr::null_mut()),
            Relaxed,
        );
    }

    /// Returns the population of the node, computing it if it has not yet been
    /// computed.
    pub(super) fn calc_population<'a>(&'a self) -> MaybeBigUint<'a> {
        // Return the population if we have already computed it.
        if let Some(p) = self.population() {
            return p;
        }

        let ret: Result<usize, BigUint> = if self.is_empty() {
            // Empty nodes have no nonzero cells, duh.
            Ok(0)
        } else if self.single_state().is_some() {
            // We already handled the zero case, so all cells in the node are
            // the same nonzero state.
            Err(self.layer().big_num_cells::<D>().into())
        } else if let Some(cells) = self.cell_slice() {
            // Count nonzero cells.
            Ok(cells.iter().filter(|&&x| x != 0).count())
        } else if let Some(children) = self.children_slice() {
            // Sum child population counts, converting from `usize` to `BigUint`
            // if necessary.
            let mut total = Ok(0);
            for child in children {
                total = total.as_ref().map_or_else(
                    |big| MaybeBigUint::Big(big),
                    |&small| MaybeBigUint::Small(small),
                ) + child.calc_population();
            }
            total
        } else {
            unreachable!("All nodes have children or cells")
        };
        match ret {
            Ok(small_pop) => self.set_pop_small(small_pop),
            Err(big_pop) => self.set_pop_big(big_pop),
        }
        // Under no circumstance is the population *erased*, so this `.unwrap()`
        // will never fail.
        self.population().unwrap()
    }
    /// Returns the population of the node, or `None` if it has not yet been
    /// computed.
    pub(super) fn population<'a>(&'a self) -> Option<MaybeBigUint<'a>> {
        let pop = self.population.load(Relaxed);
        if pop == 0 {
            // If the whole value is 0, then it hasn't been computed yet.
            None
        } else if pop & 1 == 0 {
            // If the lowest bit is 0, then it's a pointer to a `BigUint`.
            Some(unsafe { &*(pop as *mut BigUint) }.into())
        } else {
            // Otherwise it's a simple unsigned number, just shifted.
            Some((pop >> 1).into())
        }
    }
    /// Sets the population of the node if it has not already been computed.
    /// Automatically converts the argument to a `BigUint` if necessary.
    fn set_pop_small(&self, pop: usize) {
        let value_to_store = (pop << 1) | 1;
        // Does `pop` fit inside 63 bits? If not, we'll need to convert to
        // `BigUint`.
        if value_to_store >> 1 == pop {
            // If this fails, we don't care. That just means some other thread
            // computed the population before we did.
            let _ = self
                .population
                .compare_exchange(0, (pop << 1) | 1, Relaxed, Relaxed);
        } else {
            self.set_pop_big(pop.into());
        }
    }
    /// Sets the population of the node if it has not already been computed.
    fn set_pop_big(&self, pop: BigUint) {
        // Put it on the heap and leak it.
        let new_pop_ptr = Box::into_raw(Box::new(pop));
        if self
            .population
            .compare_exchange(0, new_pop_ptr as usize, Relaxed, Relaxed)
            .is_err()
        {
            // The swap was not successful, so drop `pop_ptr` because the
            // it's not in `self.population`.
            unsafe { std::ptr::drop_in_place(new_pop_ptr) };
        }
    }

    /// Returns an estimate for the amount of space on the heap used by this
    /// node.
    ///
    /// This does not take into account jemalloc's size classes.
    pub(super) fn heap_size(&self) -> usize {
        std::mem::size_of::<Self>()
            // Heap size of cells or children.
            + self.cell_slice().map(std::mem::size_of_val).unwrap_or(0)
            + self
                .children_slice()
                .map(std::mem::size_of_val)
                .unwrap_or(0)
            // Heap size of population.
            + if let Some(MaybeBigUint::Big(i)) = self.population() {
                std::mem::size_of::<BigUint>()
                // Ceiling division; assume that `BigUint` allocates 64 bits at
                // a time.
                + ((i.bits() - 1) / 64 + 1 * 8) as usize
            } else {
                0
            }
            // Estimate the overhead of storing this node in the node pool.
            + RAW_NODE_MEMORY_OVERHEAD
    }

    /// Marks the node as unreachable during garbage collection.
    pub(super) fn mark_gc_unreachable(&self) {
        self.gc_reachable.store(false, Relaxed);
    }
    /// Marks the node, its children, and its HashLife result (if any) as
    /// reachable during garbage collection.
    pub(super) fn mark_gc_reachable(&self) {
        if self.gc_reachable.load(Relaxed) {
            // We've already marked this node and its children/result as
            // reachable, so there is no need to do it again.
            return;
        }
        self.gc_reachable.store(true, Relaxed);
        if let Some(children) = self.children_slice() {
            for child in children {
                child.mark_gc_reachable();
            }
        }
        if let Some(res) = self.result() {
            res.mark_gc_reachable();
        }
    }
    /// Returns `true` if the node is reachable during garbage collection.
    pub(super) fn is_gc_reachable(&self) -> bool {
        self.gc_reachable.load(Relaxed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dim::Dim3D;

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn test_sizeof_raw_node() {
        // Verify my guesses about `RawNode` size.
        assert_eq!(32, std::mem::size_of::<RawNode<Dim3D>>());

        // Test cells slice.
        let l = Layer(2);
        let r1 = RawNode::<Dim3D>::new_empty_leaf(l);
        assert_eq!(
            RAW_NODE_MEMORY_OVERHEAD + 32 + l.num_cells::<Dim3D>().unwrap(),
            r1.heap_size()
        );

        // Test children slice.
        let r2 = unsafe { RawNode::new_non_leaf(vec![&r1; 8].into_boxed_slice()) };
        assert_eq!(
            RAW_NODE_MEMORY_OVERHEAD + 32 + 8 * std::mem::size_of::<usize>(),
            r2.heap_size()
        );
    }
}
