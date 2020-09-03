//! Owned ND-tree node.
//!
//! This is based loosely on `hlife_algo.cpp` in Golly.

use arc_swap::ArcSwapOption;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
use std::sync::Arc;

use super::Layer;
use crate::dim::Dim;
use crate::num::{BigUint, MaybeBigUint};

/// Owned ND-tree node.
///
/// Several methods of `RawNode` are `unsafe`, and it's usually better to use a
/// borrowing wrapper around one.
///
/// # Memory usage
///
/// Simulating large patterns will create a lot of `Arc<RawNode<D>>`, so we want
/// to optimize the memory usage of that. Rust's default allocator is jemalloc,
/// which groups small allocations into "bins" based on size, so we would like
/// `ArcInner<RawNode<D>>` to be the smallest power-of-2 size it can be in order
/// to save RAM.
///
/// ## 64-bit
///
/// - `strong_count` - 8 bytes (from `ArcInner<T>`)
/// - `weak_count` - 8 bytes (from `ArcInner<T>`)
/// - `slice_ptr` - 8 bytes
/// - `result_ptr` - 8 bytes
/// - `population` - 8 bytes
/// - `layer` - 4 bytes
/// - `residue` - 1 byte
/// - `is_empty` - 1 byte
/// - padding - 2 bytes
///
/// Total: 48 bytes, which exactly matches one of jemalloc's size classes, so no
/// space is wasted (except for the two padding bytes at the end).
///
/// ## 32-bit
///
/// - `strong_count` - 4 bytes (from `ArcInner<T>`)
/// - `weak_count` - 4 bytes (from `ArcInner<T>`)
/// - `slice_ptr` - 4 bytes
/// - `result_ptr` - 4 bytes
/// - `population` - 4 bytes
/// - `layer` - 4 bytes
/// - `residue` - 1 byte
/// - `is_empty` - 1 byte
/// - padding - 2 bytes
///
/// Total: 28 bytes, which isn't perfect but still fits within jemalloc's
/// 32-byte size class and only wastes 6 bytes.
///
/// ## Aren't slice pointers two `usize`s?
///
/// Generally, yes! That second `usize` is the length of the slice, but we can
/// infer the length of the slice from `layer` so we don't need to carry around
/// the extra `usize`.
#[derive(Debug)]
pub struct RawNode<D: Dim> {
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
    result_ptr: ArcSwapOption<RawNode<D>>,

    /// Number of live cells.
    ///
    /// If the lowest bit is 0, this is an owned pointer to a `BigUint`; if the
    /// lowest bit is 1, the other 63 bits encode an unsigned value. A null
    /// pointer indicates that the value has not yet been computed.
    population: AtomicUsize,

    /// Layer of this node.
    layer: Layer,

    /// Linear combination of spacetime residues of this node.
    ///
    /// Each spacetime residue is an arbitrary linear combination of spatial
    /// positions and temporal position (global generation count), modulo some
    /// value. The exact combinations used depend on the automaton.
    residue: u8,

    /// Whether the node contains only state #0.
    is_empty: bool,
}

impl<D: Dim> Drop for RawNode<D> {
    #[inline]
    fn drop(&mut self) {
        if self.layer.is_leaf::<D>() {
            // Drop cell slice.
            unsafe { std::ptr::drop_in_place(self.cell_slice_ptr()) };
        } else {
            // Drop children slice.
            unsafe { std::ptr::drop_in_place(self.children_slice_ptr()) };
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
        for child in self.children_slice().into_iter().flatten() {
            Arc::as_ptr(child).hash(state);
        }
    }
}

impl<D: Dim> PartialEq for RawNode<D> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.layer == other.layer
            && self.residue == other.residue
            && (self.is_empty() == other.is_empty()
                || self.slice_ptr == other.slice_ptr
                || self.cell_slice() == other.cell_slice())
    }
}
impl<D: Dim> Eq for RawNode<D> {}

impl<D: Dim> RawNode<D> {
    /// Creates a new empty leaf node.
    ///
    /// # Panics
    ///
    /// This function panics if `layer` is not a leaf layer.
    pub fn new_empty_leaf(layer: Layer) -> Self {
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
    pub fn new_leaf(cells: Box<[u8]>) -> Self {
        let layer = Layer::from_num_cells::<D>(cells.len()).expect("Invalid leaf node cell count");
        assert!(layer.is_leaf::<D>(), "Leaf node layer too large");
        let is_empty = cells.iter().all(|&x| x == 0);
        let start_of_cells = &Box::leak(cells)[0];

        Self {
            _phantom: PhantomData,

            slice_ptr: NonNull::from(start_of_cells),
            result_ptr: ArcSwapOption::new(None),
            population: AtomicUsize::new(0),

            layer,
            residue: 0,
            is_empty,
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
    pub fn new_non_leaf(children: Box<[Arc<RawNode<D>>]>) -> Self {
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
        let is_empty = children.iter().all(|child| child.is_empty());
        let start_of_children = &Box::leak(children)[0];

        Self {
            _phantom: PhantomData,

            slice_ptr: NonNull::from(start_of_children).cast(),
            result_ptr: ArcSwapOption::new(None),
            population: AtomicUsize::new(0),

            layer,
            residue: 0,
            is_empty,
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
        self.is_empty
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
    fn children_slice_ptr(&self) -> *mut [Arc<RawNode<D>>] {
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
    pub fn cell_slice(&self) -> Option<&[u8]> {
        unsafe { self.cell_slice_ptr().as_ref() }
    }
    /// Returns the children slice if this is a non-leaf node, or `None` if it
    /// is not.
    #[inline]
    pub fn children_slice(&self) -> Option<&[Arc<RawNode<D>>]> {
        unsafe { self.children_slice_ptr().as_ref() }
    }

    /// Returns the node one layer below this one that results from simulating
    /// this node some fixed number of generations, or `None` if that result
    /// hasn't been computed yet.
    #[inline]
    pub fn result<'a>(&'a self) -> arc_swap::Guard<'a, Option<Arc<RawNode<D>>>> {
        self.result_ptr.load()
    }
    /// Atomically sets the result of simulating this node for some fixed number
    /// of generations.
    #[inline]
    pub fn set_result(&self, result_ptr: Option<Arc<RawNode<D>>>) {
        self.result_ptr.store(result_ptr);
    }

    /// Returns the population of the node, or `None` if it has not yet been
    /// computed.
    pub fn population<'a>(&'a self) -> Option<MaybeBigUint<'a>> {
        let pop = self.population.load(Relaxed);
        if pop == 0 {
            // If the whole value is 0, then it hasn't been computed yet.
            None
        } else if pop & 1 == 0 {
            // If the lowest bit is 0, then it's a pointer to a `BigUint`.
            Some(unsafe { (pop as *mut BigUint).as_ref() }.unwrap().into())
        } else {
            // Otherwise it's a simple unsigned number, just shifted.
            Some((pop >> 1).into())
        }
    }
    /// Sets the population of the node if it has not already been computed.
    pub fn set_pop_small(&self, pop: usize) {
        self.population.compare_and_swap(0, pop, Relaxed);
    }
    /// Sets the population of the node if it has not already been computed.
    pub fn set_pop_big(&self, pop: BigUint) {
        // Put it on the heap and leak it.
        let new_pop_ptr = Box::leak(Box::new(pop)) as *mut _;
        let old = self
            .population
            .compare_and_swap(0, new_pop_ptr as usize, Relaxed);
        if old != 0 {
            // The swap was not successful, so drop `pop_ptr` because the
            // it's not in `self.population`.
            unsafe { std::ptr::drop_in_place(new_pop_ptr) };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dim::Dim2D;

    struct ArcInner<T> {
        _strong: usize,
        _weak: usize,
        _data: T,
    }

    #[test]
    fn test_sizeof_raw_node() {
        // Verify my guesses about `RawNode` size.

        #[cfg(target_pointer_width = "64")]
        assert_eq!(48, std::mem::size_of::<ArcInner<RawNode<Dim2D>>>());

        #[cfg(target_pointer_width = "32")]
        assert_eq!(48, std::mem::size_of::<ArcInner<RawNode<Dim2D>>>());
    }
}
