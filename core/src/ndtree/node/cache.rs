//! Cache for ND-tree (generalized N-dimensional quadtree) nodes.
//!
//! Reading from and adding to the cache only requires a &NodeCache, but garbage
//! collection requires a &mut NodeCache.

use itertools::Itertools;
use parking_lot::{Mutex, RwLock};
use seahash::SeaHasher;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{BuildHasher, BuildHasherDefault, Hash, Hasher};
use std::sync::{Arc, Weak};

use super::{CachedNodeRefTrait, Layer, NodeRef, NodeRefEnum, NodeRefTrait, RawNode};
use crate::dim::Dim;
use crate::ndvec::BigVec;
use crate::num::{BigUint, One, Zero};

/// Fast hasher used for nodes.
pub type NodeHasher = BuildHasherDefault<SeaHasher>;

type NodeShard<D> = Mutex<HashSet<Box<RawNode<D>>, NodeHasher>>;
// TODO: measure perf with different shard counts
const SHARD_COUNT: usize = 64;

/// Cache of ND-tree nodes for a single simulation.
pub struct NodeCache<D: Dim> {
    /// Pointer to the `Arc<RwLock<T>>` of this cache. This value should never
    /// be dropped until the `NodeCache` is.
    this: Weak<RwLock<Self>>,

    /// Set of canonical instances of nodes.
    ///
    /// A node must only be deleted from this set if there are no "strong"
    /// (`Arc<T>`) references to it, and this set must hold the only "weak"
    /// reference.
    ///
    /// This set holds `Weak<T>` references to nodes, and "owned" `Arc<T>`
    ///
    /// The set is split into many "shards" to reduce contention (a la
    /// https://docs.rs/crate/sharded/0.0.5). We use `Mutex` instead of `RwLock`
    /// because any reader could turn into a writer if the node it's looking for
    /// is not present.
    node_shards: Box<[NodeShard<D>]>,
    /// Map of pointers to nodes held by an `ArcNode` to number of references.
    ///
    /// Nodes with a refcount of zero are removed during GC. Nodes with a refcount less than zero
    ///
    /// These must be preserved during garbage collection.
    arc_nodes: Mutex<HashMap<*const RawNode<D>, usize>>,
    /// Cache of pointers to empty nodes at each layer. The index of the vector
    /// is the layer of the node.
    empty_nodes: Mutex<Vec<*const RawNode<D>>>,

    /// Simulation lock.
    ///
    /// Reading or writing HashLife results requires a read handle to this lock,
    /// and modifying parameters that determine the meaning of those results
    /// (such as simulation step size) requires a write handle to this lock.
    sim_lock: RwLock<HashLifeParams<D>>,
}
impl<D: Dim> fmt::Debug for NodeCache<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeCache({:#p})", self.this.as_ptr())
    }
}
impl<D: Dim> PartialEq for NodeCache<D> {
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.this, &other.this)
    }
}
impl<D: Dim> PartialEq<Weak<RwLock<NodeCache<D>>>> for NodeCache<D> {
    fn eq(&self, other: &Weak<RwLock<NodeCache<D>>>) -> bool {
        Weak::ptr_eq(&self.this, other)
    }
}
impl<D: Dim> PartialEq<Arc<RwLock<NodeCache<D>>>> for NodeCache<D> {
    fn eq(&self, other: &Arc<RwLock<NodeCache<D>>>) -> bool {
        Weak::as_ptr(&self.this) == Arc::as_ptr(other)
    }
}
impl<D: Dim> Eq for NodeCache<D> {}
impl<D: Dim> Hash for NodeCache<D> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.this.as_ptr().hash(state)
    }
}
impl<D: Dim> NodeCache<D> {
    /// Creates an empty node cache.
    pub fn new() -> Arc<RwLock<Self>> {
        let ret = Arc::new(RwLock::new(Self {
            this: Weak::new(),

            node_shards: std::iter::repeat_with(|| NodeShard::default())
                .take(SHARD_COUNT)
                .collect_vec()
                .into_boxed_slice(),
            arc_nodes: Mutex::new(HashMap::new()),
            empty_nodes: Mutex::new(vec![]),

            sim_lock: RwLock::new(HashLifeParams {
                log2_step_size: 0,
                cache: Weak::new(),
            }),
        }));

        {
            let mut ret_write = ret.write();
            ret_write.this = Arc::downgrade(&ret);
            ret_write.sim_lock.get_mut().cache = Arc::downgrade(&ret);
        }

        ret
    }

    /// Asserts that the two caches are the same (point to the same location).
    fn assert_same_as<T: fmt::Debug>(&self, other: &T)
    where
        Self: PartialEq<T>,
    {
        assert_eq!(
            *self, *other,
            "Attempt to operate on a node from a different cache",
        );
    }
    /// Asserts that the node is from this cache.
    fn assert_owns_node<'cache>(&self, node: impl CachedNodeRefTrait<'cache, D = D>) {
        self.assert_same_as(node.cache());
    }

    /// Does a "strong" garbage collection. This invalidates all HashLife
    /// results, deletes all unused empty nodes, and then removes all nodes not
    /// reachable.
    pub fn strong_gc(&mut self) {
        self.invalidate_results();
        self.clear_empty_node_cache();
        self.weak_gc();
    }
    /// Does a "weak" garbage collection. This removes all nodes that are not
    /// reachable.
    pub fn weak_gc(&mut self) {
        // Mark all nodes as unreachable.
        for shard in &mut self.node_shards[..] {
            for node in &*shard.get_mut() {
                node.mark_gc_unreachable();
            }
        }

        // Mark `ArcNode`s and their children/result as reachable.
        self.arc_nodes.get_mut().retain(|&node_ptr, &mut refcount| {
            let node = unsafe { &*node_ptr };
            if refcount > 0 {
                // If there are outstanding references, keep the node.
                node.mark_gc_reachable();
                true
            } else {
                // Otherwise, delete it from `arc_nodes`.
                false
            }
        });
        // Mark empty nodes as reachable.
        for &node_ptr in &*self.empty_nodes.get_mut() {
            let node = unsafe { &*node_ptr };
            node.mark_gc_reachable();
        }

        // Delete unreachable nodes.
        for shard in &mut self.node_shards[..] {
            shard.get_mut().retain(|node| node.is_gc_reachable());
        }
    }
    /// Clears the HashLife results cache from every node.
    ///
    /// Note that another thread may add HashLife results to a node while this
    /// function is running.
    pub fn invalidate_results(&self) {
        for shard in self.node_shards.iter() {
            for raw_node in shard.lock().iter() {
                unsafe { raw_node.set_result(None) };
            }
        }
    }
    /// Clears empty nodes that have no other references.
    pub fn clear_empty_node_cache(&self) {
        *self.empty_nodes.lock() = vec![];
    }

    /// Returns the largest possible canonical leaf node containing only state #0.
    pub fn get_empty_base<'cache>(&'cache self) -> NodeRef<'cache, D> {
        self.get_empty(Layer::base::<D>())
    }
    /// Returns the canonical node at the given layer containing only state #0.
    pub fn get_empty<'cache>(&'cache self, layer: Layer) -> NodeRef<'cache, D> {
        let mut empty_nodes = self.empty_nodes.lock();
        // Add empty nodes until we have enough.
        while empty_nodes.len() <= layer.to_usize() {
            let next_layer = Layer(empty_nodes.len() as u32);
            if next_layer.is_leaf::<D>() {
                empty_nodes.push(self.get(RawNode::new_empty_leaf(next_layer)).as_raw());
            } else {
                let empty_node_children =
                    vec![unsafe { &**empty_nodes.last().unwrap() }; D::BRANCHING_FACTOR]
                        .into_boxed_slice();
                let new_empty_node =
                    self.get(unsafe { RawNode::new_non_leaf(empty_node_children) });
                empty_nodes.push(new_empty_node.as_raw());
            }
        }
        let ret = unsafe { NodeRef::new(self, &*empty_nodes[layer.to_usize()]) };
        debug_assert_eq!(layer, ret.layer());
        ret
    }

    /// Returns the canonical instance of a node, adding it to the cache if one
    /// does not exist.
    pub fn get<'cache>(&'cache self, raw_node: RawNode<D>) -> NodeRef<'cache, D> {
        let mut hash = NodeHasher::default().build_hasher();
        raw_node.hash(&mut hash);
        // Hash a little extra, so that the hash here is distinct from the one used within the individual shard.
        SHARD_COUNT.hash(&mut hash);

        let shard_index = hash.finish() as usize % SHARD_COUNT;
        let mut shard = self.node_shards[shard_index].lock();

        // This is ugly, but there's just no API yet for inserting a key into a
        // `HashSet` and immediately getting a reference back.
        shard
            .get(&raw_node)
            .map(|n| unsafe { NodeRef::new(self, &**n) })
            .unwrap_or_else(|| {
                // Pin it on the heap before you take a reference to it!
                let raw_node = Box::new(raw_node);
                let ret = unsafe { NodeRef::new(self, &*raw_node) };
                // We should be inserting the node for the first time.
                assert!(shard.insert(raw_node));
                ret
            })
    }

    /// Creates a new node by joining `children` together.
    ///
    /// # Panics
    ///
    /// This method panics if the number of children is less than 2^NDIM. Extra
    /// children are ignored.
    ///
    /// This method panics in debug mode if the children are not all at the same
    /// layer.
    pub fn join_nodes<'cache, N: CachedNodeRefTrait<'cache, D = D>>(
        &'cache self,
        children: impl IntoIterator<Item = N>,
    ) -> NodeRef<'cache, D> {
        let children = children
            .into_iter()
            .inspect(|&node| self.assert_owns_node(node))
            .map(|node| node.as_raw())
            .collect_vec()
            .into_boxed_slice();
        assert_eq!(children.len(), D::BRANCHING_FACTOR);
        let child_layer = children[0].layer();
        for child in &*children {
            debug_assert_eq!(child_layer, child.layer());
        }

        if child_layer < Layer::base::<D>() {
            // Children are below the base layer, so make a leaf node by
            // combining the cells of the children.
            self.get_from_cells(super::cells::join::<D>(
                &children
                    .iter()
                    .map(|child| child.cell_slice().unwrap())
                    .collect_vec(),
            ))
        } else {
            // Children are at or above the base layer, so just make a non-leaf
            // node pointing to them.
            self.get(unsafe { RawNode::new_non_leaf(children) })
        }
    }
    /// Creates a node containing the given cells.
    pub fn get_from_cells<'cache>(&'cache self, cells: impl Into<Box<[u8]>>) -> NodeRef<'cache, D> {
        let cells = cells.into();
        let layer = Layer::from_num_cells::<D>(cells.len()).expect("Invalid cell count");

        if layer <= Layer::base::<D>() {
            // This node is at or below the base layer, so make a leaf node.
            self.get(RawNode::new_leaf(cells))
        } else {
            // This node is above the base layer, so subdivide the cells into
            // smaller leaf nodes and make this a non-leaf node.
            self.join_nodes(
                super::cells::subdivide::<D>(&cells)
                    .unwrap()
                    .map(|subcube| self.get_from_cells(subcube)),
            )
        }
    }

    /// Returns one corner of the node at one layer lower. If the node is only a
    /// single cell, returns `Err()` containing that cell state.
    ///
    /// The return value is borrowed if `node` is a non-leaf and owned if `node`
    /// is a leaf.
    ///
    /// This is equivalent to taking one element of the result of `subdivide()`.
    pub fn get_corner<'cache>(
        &'cache self,
        node: impl CachedNodeRefTrait<'cache, D = D>,
        index: usize,
    ) -> Result<NodeRef<'cache, D>, u8> {
        self.assert_owns_node(node);

        Ok(match node.as_enum() {
            NodeRefEnum::Leaf(n) => {
                self.get_from_cells(super::cells::get_corner::<D>(n.cells(), index)?)
            }
            NodeRefEnum::NonLeaf(n) => n.child_at_index(index),
        })
    }
    /// Subdivides the node into 2^NDIM smaller nodes at one layer lower. If the
    /// node is only a single cell, returns `Err()` containing that cell state.
    ///
    /// The return value is borrowed if `node` is a non-leaf and owned if `node`
    /// is a leaf.
    pub fn subdivide<'cache>(
        &'cache self,
        node: impl CachedNodeRefTrait<'cache, D = D>,
    ) -> Result<Vec<NodeRef<'cache, D>>, u8> {
        (0..D::BRANCHING_FACTOR)
            .map(|i| self.get_corner(node, i))
            .try_collect()
    }
    /// Creates a node one layer lower containing the contents of the center of
    /// the node.
    ///
    /// If the node is below `Layer(2)`, returns `Err(())`.
    pub fn centered_inner<'cache>(
        &'cache self,
        node: impl CachedNodeRefTrait<'cache, D = D>,
    ) -> Result<NodeRef<'cache, D>, ()> {
        self.assert_owns_node(node);

        Ok(match node.as_enum() {
            NodeRefEnum::Leaf(n) => {
                self.get_from_cells(super::cells::shrink_centered::<D>(n.cells())?)
            }
            NodeRefEnum::NonLeaf(n) => {
                let child_index_bitmask = D::BRANCHING_FACTOR - 1;
                let new_children = self
                    .subdivide(n)
                    .map_err(|_| ())?
                    .into_iter()
                    .enumerate()
                    .map(|(child_index, child)| {
                        // Invert the bits of the child index to get the index
                        // of the opposite child, which is the one closest to
                        // the center (e.g. SE child of NE child).
                        let opposite_child_index = child_index ^ child_index_bitmask;
                        // If any grandchild other than the one closest to the
                        // center is non-empty, then we can't shrink any more.
                        let grandchildren = child.subdivide().unwrap();
                        // Return the grandchild closest to the center.
                        grandchildren[opposite_child_index]
                    });
                self.join_nodes(new_children)
            }
        })
    }

    /// Creates an identical node except with the cell at the given position
    /// (modulo the node length along each axis) modified.
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn set_cell<'cache>(
        &'cache self,
        node: impl CachedNodeRefTrait<'cache, D = D>,
        pos: &BigVec<D>,
        cell_state: u8,
    ) -> NodeRef<'cache, D> {
        self.assert_owns_node(node);

        // Modulo the node length along each axis using bitwise AND.
        match node.as_enum() {
            NodeRefEnum::Leaf(n) => {
                // We use modulo_pos() because we have to convert the BigVec to
                // a UVec and we can't know for sure that it will fit otherwise.
                let cell_index = n.pos_to_cell_index(n.modulo_pos(pos).to_uvec());
                // Possible optimization: avoid unpacking cells; just copy the
                // packed cells and modify the cell there.
                let mut new_cells = n.cells().to_vec().into_boxed_slice();
                new_cells[cell_index] = cell_state;
                self.get_from_cells(new_cells)
            }
            NodeRefEnum::NonLeaf(n) => {
                let child_index = n.child_index_with_pos(pos);
                let new_child = self.set_cell(n.child_at_index(child_index), pos, cell_state);
                let mut new_children = n.children().collect_vec();
                new_children[child_index] = new_child;
                self.join_nodes(new_children)
            }
        }
    }

    /// Returns the simulation lock for this cache.
    ///
    /// Reading or writing HashLife results requires a read handle to this lock,
    /// and modifying parameters that determine the meaning of those results
    /// (such as simulation step size) requires a write handle to this lock.
    pub fn sim_lock(&self) -> &RwLock<HashLifeParams<D>> {
        &self.sim_lock
    }
}

// Conceptually, the `*const RawNode`s held by `NodeCache` are just references,
// with lifetimes dependent on the cache. `&RawNode` is `Send + Sync`, so it's
// safe to implement `Send` on `NodeCache` as well.
unsafe impl<D: Dim> Send for NodeCache<D> {}
unsafe impl<D: Dim> Sync for NodeCache<D> {}

/// HashLife parameters.
#[derive(Debug, Clone)]
pub struct HashLifeParams<D: Dim> {
    /// Base-2 log of the step size for the simulation.
    ///
    /// Results must be invalidated any time this is modified.
    log2_step_size: u32,

    /// Pointer to the `Arc<RwLock<T>>` of the parent cache. This value should
    /// never be dropped until the `HashLifeParams` is.
    cache: Weak<RwLock<NodeCache<D>>>,
}
impl<D: Dim> HashLifeParams<D> {
    /// Returns the simulation step size.
    #[inline]
    pub fn step_size(&self) -> BigUint {
        BigUint::one() << self.log2_step_size
    }
    /// Returns the base-2 log of the simulation step size.
    #[inline]
    pub fn log2_step_size(&self) -> u32 {
        self.log2_step_size
    }
    /// Sets the base-2 log of the simulation step size, invalidating the cache
    /// if it has changed.
    pub fn set_log2_step_size(&mut self, log2_step_size: u32) {
        if self.log2_step_size != log2_step_size {
            self.cache.upgrade().unwrap().read().invalidate_results();
            self.log2_step_size = log2_step_size;
        }
    }

    /// Returns the step size for a node at the given layer in a rule with the
    /// given radius.
    pub fn big_node_step_size(&self, layer: Layer, rule_radius: usize) -> BigUint {
        if let Some(pow) = self.log2_node_step_size(layer, rule_radius) {
            BigUint::one() << pow
        } else {
            BigUint::zero()
        }
    }
    /// Returns the step size for a node at the given layer in a rule with the
    /// given radius, or `None` if it does not fit within a `usize`.
    pub fn node_step_size(&self, layer: Layer, rule_radius: usize) -> Option<usize> {
        if let Some(pow) = self.log2_node_step_size(layer, rule_radius) {
            1_usize.checked_shl(pow)
        } else {
            Some(0)
        }
    }
    /// Returns the base-2 log of the step size for a node at the given layer in
    /// a rule with the given radius, or `None` if even a single generation
    /// cannot be simulated.
    pub fn log2_node_step_size(&self, layer: Layer, rule_radius: usize) -> Option<u32> {
        let log2_rule_radius = rule_radius
            .next_power_of_two()
            .max(1) // Treat r=0 rules like r=1.
            .trailing_zeros();
        // Compute the maximum number of  generations we could simulate this
        // node, regardless of the currently configured step size. We start with
        // the size of the layer, divide by 4 to get the "padding" between the
        // edge of the node and the edge of its centered inner result, and then
        // divide by the rule radius (which defines the rule's "speed of light")
        // to turn distance into time.
        let log2_layer_max = layer
            .to_u32()
            // If the result would be negative, then we can't even simulate a
            // single generation on this node.
            .checked_sub(2 + log2_rule_radius)?;
        Some(std::cmp::min(log2_layer_max, self.log2_step_size))
    }
}
#[cfg(test)]
impl HashLifeParams<crate::dim::Dim2D> {
    /// Constructor for testing.
    #[cfg(test)]
    pub(super) fn with_log2_step_size(log2_step_size: u32) -> Self {
        Self {
            log2_step_size,
            cache: Weak::new(),
        }
    }
}

/// Node reference that also holds an `Arc` reference to the cache instead of
/// borrowing it.
#[derive(Debug)]
pub struct ArcNode<D: Dim> {
    cache: Arc<RwLock<NodeCache<D>>>,
    raw_node_ptr: *const RawNode<D>,
}
impl<D: Dim> Drop for ArcNode<D> {
    fn drop(&mut self) {
        // Decrement the refcount.
        self.cache
            .read()
            .arc_nodes
            .lock()
            .entry(self.raw_node_ptr)
            .and_modify(|refcount| *refcount -= 1);
    }
}
impl<D: Dim> Clone for ArcNode<D> {
    fn clone(&self) -> Self {
        unsafe { Self::new(&self.cache().read(), self.raw_node_ptr) }
    }
}
impl<D: Dim> PartialEq for ArcNode<D> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.raw_node_ptr, other.raw_node_ptr)
    }
}
impl<D: Dim> Eq for ArcNode<D> {}
impl<'cache, D: Dim, N: NodeRefTrait<'cache, D = D>> PartialEq<N> for ArcNode<D> {
    fn eq(&self, other: &N) -> bool {
        std::ptr::eq(self.raw_node_ptr, other.as_raw())
    }
}
impl<D: Dim> ArcNode<D> {
    /// Creates an `ArcNode` from raw parts.
    ///
    /// # Saftey
    ///
    /// `raw_node_ptr` must point to a valid node obtained from `cache`.
    #[inline]
    pub unsafe fn new(cache: &NodeCache<D>, raw_node_ptr: *const RawNode<D>) -> Self {
        // Increment the refcount.
        cache
            .arc_nodes
            .lock()
            .entry(raw_node_ptr)
            .and_modify(|refcount| *refcount += 1)
            .or_insert(1);

        Self {
            cache: cache.this.upgrade().unwrap(),
            raw_node_ptr,
        }
    }

    /// Returns the cache that the node is stored in.
    pub fn cache(&self) -> &Arc<RwLock<NodeCache<D>>> {
        &self.cache
    }

    /// Returns a `NodeRef` of the node.
    ///
    /// # Panics
    ///
    /// This method panics if `cache` is not a reference to the cache that
    /// this node is stored in.
    pub fn as_ref<'cache>(&self, cache: &'cache NodeCache<D>) -> NodeRef<'cache, D> {
        cache.assert_same_as(self.cache());
        unsafe { NodeRef::new(cache, &*self.raw_node_ptr) }
    }
}
impl<'cache, D: Dim, N: CachedNodeRefTrait<'cache, D = D>> From<N> for ArcNode<D> {
    #[inline]
    fn from(n: N) -> Self {
        unsafe { Self::new(n.cache(), n.as_raw()) }
    }
}
impl<'node, D: Dim> NodeRefTrait<'node> for &'node ArcNode<D> {
    type D = D;

    fn as_raw(self) -> &'node RawNode<Self::D> {
        unsafe { &*self.raw_node_ptr }
    }
}

// Conceptually, the `*const RawNode` held by `ArcNode` is just a reference,
// with a lifetime dependent on the cache. `&RawNode` is `Send + Sync`, so it's
// safe to implement `Send` on `ArcNode` as well.
unsafe impl<D: Dim> Send for ArcNode<D> {}
unsafe impl<D: Dim> Sync for ArcNode<D> {}
