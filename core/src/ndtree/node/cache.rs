//! Cache for ND-tree (generalized N-dimensional quadtree) nodes.
//!
//! Reading from and adding to the cache only requires a &NodeCache, but garbage
//! collection requires a &mut NodeCache.

use itertools::Itertools;
use parking_lot::{Mutex, RwLock, RwLockReadGuard, RwLockUpgradableReadGuard, RwLockWriteGuard};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
use std::sync::{Arc, Weak};

use super::{
    CachedNodeRefTrait, HashLifeResultParams, Layer, NodeRef, NodeRefEnum, NodeRefTrait, RawNode,
    ShardedBoxedSet,
};
use crate::dim::Dim;
use crate::ndvec::{BigVec, UVec};
use crate::HashMap;

/// Cache of ND-tree nodes for a single simulation.
pub struct NodeCache<D: Dim> {
    /// Pointer to the `Arc<RwLock<T>>` of this cache. This value should never
    /// be dropped until the `NodeCache` is.
    this: Weak<RwLock<Self>>,

    /// Set of canonical instances of nodes.
    ///
    /// A node can only be deleted from this set using a `&mut NodeCache` and
    /// only if it is not reachable from any `ArcNode`.
    ///
    /// A node is only deleted from this set if there are no "strong" (`Arc<T>`)
    /// references to it, and this set holds the only "weak" reference.
    ///
    /// This set holds `Weak<T>` references to nodes, and "owned" `Arc<T>`
    ///
    /// The set is split into many "shards" to reduce contention (a la
    /// https://docs.rs/crate/sharded/0.0.5). We use `Mutex` instead of `RwLock`
    /// because any reader could turn into a writer if the node it's looking for
    /// is not present.
    nodes: ShardedBoxedSet<RawNode<D>>,
    /// Map of pointers to nodes held by an `ArcNode` to number of references.
    ///
    /// Nodes with a refcount of zero are removed during GC. Nodes with a
    /// refcount greater than zero are kept.
    ///
    /// These are preserved during garbage collection.
    arc_nodes: Mutex<HashMap<*const RawNode<D>, usize>>,
    /// Cache of pointers to empty nodes at each layer. The index of the vector
    /// is the layer of the node.
    empty_nodes: Mutex<Vec<*const RawNode<D>>>,

    /// Memory usage counter, in bytes.
    ///
    /// This value is updated any time a new node is added to the cache, an
    /// existing node is removed, or a node's memory usage changes (such as via
    /// `calc_population()`).
    pub(super) node_heap_size: AtomicUsize,

    /// Simulation lock.
    ///
    /// Reading or writing HashLife results requires a read handle to this lock,
    /// and modifying parameters that determine the meaning of those results
    /// (such as simulation step size) requires a write handle to this lock, and
    /// modifying this value may invalidate some or all results.
    sim_lock: RwLock<HashLifeResultParams>,
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

            nodes: ShardedBoxedSet::new(),
            arc_nodes: Mutex::new(HashMap::default()),
            empty_nodes: Mutex::new(vec![]),

            node_heap_size: AtomicUsize::new(0),

            sim_lock: RwLock::new(HashLifeResultParams::default()),
        }));

        {
            let mut ret_write = ret.write();
            ret_write.this = Arc::downgrade(&ret);
        }

        ret
    }

    /// Returns the `Arc<RwLock<_>>` containing the cache.
    pub fn arc(&self) -> Arc<RwLock<Self>> {
        self.this.upgrade().unwrap()
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
    pub(super) fn assert_owns_node<'cache>(&self, node: impl CachedNodeRefTrait<'cache, D = D>) {
        self.assert_same_as(node.cache());
    }

    /// Returns the amount of space used by nodes on the heap.
    pub fn memory_usage(&self) -> usize {
        self.node_heap_size.load(Relaxed)
    }
    /// Does a "strong" garbage collection. This invalidates all HashLife
    /// results, deletes all unused empty nodes, and then removes all nodes not
    /// reachable.
    ///
    /// Returns the tuple `(nodes_dropped, nodes_kept)`.
    pub fn strong_gc(&mut self) -> (usize, usize) {
        self.invalidate_results();
        self.clear_empty_node_cache();
        self.weak_gc()
    }
    /// Does a "weak" garbage collection. This removes all nodes that are not
    /// reachable.
    ///
    /// Returns the tuple `(nodes_dropped, nodes_kept)`.
    pub fn weak_gc(&mut self) -> (usize, usize) {
        // Mark all nodes as unreachable ("innocent until proven guilty").
        self.nodes.for_each(RawNode::mark_gc_unreachable);

        // Mark `ArcNode`s and their children/results as reachable. This
        // includes `self.empty_nodes` because they are all `ArcNodes`.
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

        let mut bytes_dropped = 0;
        let mut nodes_dropped = 0;
        let mut nodes_kept = 0;

        // Delete unreachable nodes.
        self.nodes.retain(true, |node| {
            if node.is_gc_reachable() {
                // Keep the node.
                nodes_kept += 1;
                true
            } else {
                // If the node is not reachable, we're going to throw it away.
                // Subtract its heap size from the counter.
                bytes_dropped += node.heap_size();
                nodes_dropped += 1;
                false
            }
        });

        self.node_heap_size.fetch_sub(bytes_dropped, Relaxed);
        (nodes_dropped, nodes_kept)
    }
    /// Clears the HashLife results cache from every node.
    ///
    /// Note that another thread may add HashLife results to a node while this
    /// function is running.
    pub fn invalidate_results(&self) {
        self.nodes.for_each(|node| unsafe { node.set_result(None) });
    }
    /// Clears the HashLife results cache from every node above a particular
    /// layer.
    ///
    /// Note that another thread may add HashLife results to a node while this
    /// function is running.
    pub fn invalidate_results_above(&self, layer: Layer) {
        self.nodes.for_each(|node| {
            if node.layer() > layer {
                unsafe { node.set_result(None) }
            }
        });
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
                let empty_child = unsafe { NodeRef::new(self, &**empty_nodes.last().unwrap()) };
                let empty_children = vec![empty_child; D::BRANCHING_FACTOR];
                let new_empty_node = self.join_nodes(empty_children);
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
        let (ret, already_present) = self.nodes.get_or_insert(raw_node);
        if !already_present {
            // Record the heap usage of this node.
            self.node_heap_size.fetch_add(ret.heap_size(), Relaxed);
        }
        unsafe { NodeRef::new(self, ret) }
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

    /// Creates a node by evaluating `generator` for each cell. Cells may be
    /// generated out of order. This is not recommended for large nodes.
    ///
    /// # Panics
    ///
    /// This method panics if the size of the node does not fit in a `usize`.
    pub fn get_from_fn<'cache>(
        &'cache self,
        layer: Layer,
        mut generator: impl FnMut(UVec<D>) -> u8,
    ) -> NodeRef<'cache, D> {
        self._get_from_fn(UVec::origin(), layer, &mut generator)
    }
    fn _get_from_fn<'cache>(
        &'cache self,
        offset: UVec<D>,
        layer: Layer,
        generator: &mut impl FnMut(UVec<D>) -> u8,
    ) -> NodeRef<'cache, D> {
        if layer.is_leaf::<D>() {
            let rect = layer
                .rect()
                .expect("Cannot construct a node at {:?} from individual cells");
            self.get_from_cells((rect + offset).iter().map(generator).collect_vec())
        } else {
            self.join_nodes((0..D::BRANCHING_FACTOR).map(|index| {
                self._get_from_fn(
                    offset.clone() + layer.big_child_offset(index).to_uvec(),
                    layer.child_layer(),
                    generator,
                )
            }))
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

    /// Creates a node identical to one from another cache.
    pub fn copy_from_other_cache<'cache, 'other>(
        &'cache self,
        node: impl CachedNodeRefTrait<'other, D = D>,
    ) -> NodeRef<'cache, D> {
        self._copy_from_other_cache(node.as_ref(), &mut HashMap::default())
    }
    fn _copy_from_other_cache<'cache, 'other>(
        &'cache self,
        node: NodeRef<'other, D>,
        convert_table: &mut HashMap<NodeRef<'other, D>, NodeRef<'cache, D>>,
    ) -> NodeRef<'cache, D> {
        if let Some(already_computed) = convert_table.get(&node) {
            return *already_computed;
        }
        let ret = match node.as_enum() {
            NodeRefEnum::Leaf(node) => self.get_from_cells(node.cells()),
            NodeRefEnum::NonLeaf(node) => self.join_nodes(
                node.children()
                    .map(|child| self._copy_from_other_cache(child, convert_table)),
            ),
        };
        convert_table.insert(node, ret);
        ret
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

    /// Returns the simulation lock, invalidating the results cache partially or
    /// fully if necessary.
    ///
    /// This method blocks if other parameters are being used for simulation at
    /// the same time.
    pub fn sim_with<'cache>(
        &'cache self,
        params: HashLifeResultParams,
    ) -> SimCacheGuard<'cache, D> {
        let lock = self.sim_lock.upgradable_read();
        let old_params = *lock;
        let lock = if old_params != params {
            let mut lock = RwLockUpgradableReadGuard::upgrade(lock);
            self.set_sim_params(params, &mut lock);
            RwLockWriteGuard::downgrade(lock)
        } else {
            RwLockUpgradableReadGuard::downgrade(lock)
        };
        SimCacheGuard {
            cache: self,
            sim_lock: lock,
        }
    }

    /// Returns the simulation lock, invalidating the results cache partially or
    /// fully if necessary.
    ///
    /// If the lock cannot be acquired, returns `Err` containing the current parameters.
    pub fn try_sim_with<'cache>(
        &'cache self,
        params: HashLifeResultParams,
    ) -> Option<SimCacheGuard<'cache, D>> {
        let lock = self.sim_lock.try_upgradable_read()?;
        let old_params = *lock;
        let lock = if old_params != params {
            let mut lock = RwLockUpgradableReadGuard::try_upgrade(lock).ok()?;
            self.set_sim_params(params, &mut lock);
            RwLockWriteGuard::downgrade(lock)
        } else {
            RwLockUpgradableReadGuard::downgrade(lock)
        };
        Some(SimCacheGuard {
            cache: self,
            sim_lock: lock,
        })
    }

    /// Sets the simulation parameters.
    fn set_sim_params<'guard>(
        &self,
        params: HashLifeResultParams,
        sim_lock: &mut RwLockWriteGuard<'guard, HashLifeResultParams>,
    ) {
        let old_params = **sim_lock;
        **sim_lock = params;
        // If the radius changes, that means the rule has changed, and the cache
        // should have been invalidated by whoever changed the rule. The only
        // thing we need to worry about here is the `log2_node_step_size` for
        // each node, which depends only on `sim_base_layer`. (See the
        // `HashLifeResultParams` documentation.)
        let old_base_layer = old_params.sim_base_layer();
        let new_base_layer = params.sim_base_layer();
        if old_base_layer != new_base_layer {
            // Nodes at or below the base layer are simulated for the full
            // amount possible, and the rule radius (presumably) hasn't changed
            // so that's still the same.
            self.invalidate_results_above(std::cmp::min(old_base_layer, new_base_layer))
        }
    }
}

// Conceptually, the `*const RawNode`s held by `NodeCache` are just references,
// with lifetimes dependent on the cache. `&RawNode` is `Send + Sync`, so it's
// safe to implement `Send` on `NodeCache` as well.
unsafe impl<D: Dim> Send for NodeCache<D> {}
unsafe impl<D: Dim> Sync for NodeCache<D> {}

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
            .read_recursive()
            .arc_nodes
            .lock()
            .entry(self.raw_node_ptr)
            .and_modify(|refcount| *refcount -= 1);
    }
}
impl<D: Dim> Clone for ArcNode<D> {
    fn clone(&self) -> Self {
        unsafe { Self::new(&self.cache().read_recursive(), self.raw_node_ptr) }
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
            cache: cache.arc(),
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

/// RAII wrapper around simulation results cache.
#[derive(Debug)]
pub struct SimCacheGuard<'cache, D: Dim> {
    cache: &'cache NodeCache<D>,
    /// Guard to make sure that no one else modifies the parameters.
    sim_lock: RwLockReadGuard<'cache, HashLifeResultParams>,
}
impl<'cache, D: Dim> SimCacheGuard<'cache, D> {
    /// Returns the cache that this guard gives access to.
    pub fn cache(&self) -> &'cache NodeCache<D> {
        self.cache
    }
    /// Returns the parameters defining simulation results.
    pub fn params(&self) -> &HashLifeResultParams {
        &*self.sim_lock
    }
}
