//! Cache for ND-tree (generalized N-dimensional quadtree) nodes.
//!
//! Reading from and adding to the pool only requires a &NodePool, but garbage
//! collection requires a &mut NodePool.

use itertools::Itertools;
use parking_lot::{Mutex, RwLock, RwLockReadGuard, RwLockUpgradableReadGuard, RwLockWriteGuard};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
use std::sync::{Arc, Weak};

use super::{
    HashLifeResultParams, Layer, LayerTooSmall, NodeRef, NodeRefEnum, NodeRefTrait,
    NodeRefWithGuard, RawNode, ShardedBoxedSet,
};
use crate::dim::Dim;
use crate::ndvec::{BigVec, UVec};
use crate::num::{BigInt, One};
use crate::HashMap;

/// Shared pool of interned ND-tree nodes for a single simulation.
///
/// Cloning this struct creates a new reference to the same pool; prefer
/// `.new_ref()` for clarity.
#[derive(Debug, Clone)]
pub struct SharedNodePool<D: Dim>(Arc<RwLock<NodePool<D>>>);
impl<D: Dim> SharedNodePool<D> {
    /// Creates an empty node pool.
    pub fn new() -> Self {
        let ret = Arc::new(RwLock::new(NodePool {
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

        Self(ret)
    }
    /// Creates a new reference to the same node pool.
    ///
    /// This is equivalent to `.clone()` but is clearer.
    pub fn new_ref(&self) -> Self {
        self.clone()
    }

    /// Acquires access to find nodes in the pool and add missing ones, blocking
    /// only if another thread has total access to the pool.
    #[inline]
    pub fn access<'a>(&'a self) -> RwLockReadGuard<'a, NodePool<D>> {
        self.0.read_recursive()
    }
    /// Acquires access to remove nodes from the pool, which is required for
    /// garbage collection, blocking if any threads have access to the pool.
    #[inline]
    pub fn total_access<'a>(&'a self) -> RwLockWriteGuard<'a, NodePool<D>> {
        self.0.write()
    }

    /// Possibly blocks if a garbage collection thread is trying to access the
    /// node pool; otherwise does nothing.
    #[inline]
    pub fn yield_to_gc(&self) {
        // TODO: this is kind of a hack. Develop a system where threads actually
        // communicate their intent, or do GC on the simulation thread(s).
        let _ = self.0.read();
    }
}

/// Pool of interned ND-tree nodes for a single simulation.
pub struct NodePool<D: Dim> {
    /// Pointer to the `Arc<RwLock<T>>` of this pool. This value should never be
    /// dropped until the `NodePool` is.
    this: Weak<RwLock<Self>>,

    /// Set of canonical instances of nodes.
    ///
    /// A node can only be deleted from this set using a `&mut NodePool` and
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
    /// This value is updated any time a new node is added to the pool, an
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
impl<D: Dim> fmt::Debug for NodePool<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodePool({:#p})", self.this.as_ptr())
    }
}
impl<D: Dim> PartialEq for NodePool<D> {
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.this, &other.this)
    }
}
impl<D: Dim> PartialEq<Weak<RwLock<NodePool<D>>>> for NodePool<D> {
    fn eq(&self, other: &Weak<RwLock<NodePool<D>>>) -> bool {
        Weak::ptr_eq(&self.this, other)
    }
}
impl<D: Dim> PartialEq<SharedNodePool<D>> for NodePool<D> {
    fn eq(&self, other: &SharedNodePool<D>) -> bool {
        Weak::as_ptr(&self.this) == Arc::as_ptr(&other.0)
    }
}
impl<D: Dim> Eq for NodePool<D> {}
impl<D: Dim> Hash for NodePool<D> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.this.as_ptr().hash(state)
    }
}
impl<D: Dim> NodePool<D> {
    /// Returns a new reference to the same node pool.
    pub fn new_ref(&self) -> SharedNodePool<D> {
        SharedNodePool(self.this.upgrade().unwrap())
    }

    /// Asserts that the two node pools are the same (identity).
    fn assert_same_as<T: fmt::Debug>(&self, other: &T)
    where
        Self: PartialEq<T>,
    {
        assert_eq!(
            *self, *other,
            "Attempt to operate on a node from a different pool",
        );
    }
    /// Asserts that the node is from this pool.
    pub(super) fn assert_owns_node<'pool>(&self, node: impl NodeRefTrait<'pool, D = D>) {
        self.assert_same_as(node.pool());
    }

    /// Returns the amount of space used by nodes on the heap.
    pub fn memory_usage(&self) -> usize {
        self.node_heap_size.load(Relaxed)
    }
    /// Does a "strong" garbage collection. This invalidates all HashLife
    /// results, deletes all unused empty nodes, and then removes all nodes not
    /// reachable from an `ArcNode`.
    ///
    /// Returns the tuple `(nodes_dropped, nodes_kept)`.
    pub fn strong_gc(&mut self) -> (usize, usize) {
        self.invalidate_results();
        self.clear_empty_node_cache();
        self.remove_unreachable()
    }
    /// Does a "weak" garbage collection. This removes all nodes that are not
    /// reachable from an `ArcNode`.
    ///
    /// Returns the tuple `(nodes_dropped, nodes_kept)`.
    pub fn weak_gc(&mut self) -> (usize, usize) {
        self.remove_unreachable()
    }
    /// Removes all nodes that are not recursively reachable from `ArcNode`s.
    ///
    /// Returns the tuple `(nodes_dropped, nodes_kept)`.
    fn remove_unreachable(&mut self) -> (usize, usize) {
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
    fn invalidate_results(&self) {
        self.nodes.for_each(|node| unsafe { node.set_result(None) });
    }
    /// Clears the HashLife results cache from every node above a particular
    /// layer.
    ///
    /// Note that another thread may add HashLife results to a node while this
    /// function is running.
    fn invalidate_results_above(&self, layer: Layer) {
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
    pub fn get_empty_base<'pool>(&'pool self) -> NodeRef<'pool, D> {
        self.get_empty(Layer::base::<D>())
    }
    /// Returns the canonical node at the given layer containing only state #0.
    pub fn get_empty<'pool>(&'pool self, layer: Layer) -> NodeRef<'pool, D> {
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

    /// Returns the canonical instance of a node, adding it to the pool if one
    /// does not exist.
    fn get<'pool>(&'pool self, raw_node: RawNode<D>) -> NodeRef<'pool, D> {
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
    pub fn join_nodes<'pool, 'children, N: NodeRefTrait<'children, D = D>>(
        &'pool self,
        children: impl IntoIterator<Item = N>,
    ) -> NodeRef<'pool, D> {
        let children = children
            .into_iter()
            .inspect(|&node| self.assert_owns_node(node))
            .map(|node| node.as_ref().as_raw())
            .take(D::BRANCHING_FACTOR)
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
    pub fn get_from_cells<'pool>(&'pool self, cells: impl Into<Box<[u8]>>) -> NodeRef<'pool, D> {
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
    pub fn get_from_fn<'pool>(
        &'pool self,
        layer: Layer,
        mut generator: impl FnMut(UVec<D>) -> u8,
    ) -> NodeRef<'pool, D> {
        self._get_from_fn(UVec::origin(), layer, &mut generator)
    }
    fn _get_from_fn<'pool>(
        &'pool self,
        offset: UVec<D>,
        layer: Layer,
        generator: &mut impl FnMut(UVec<D>) -> u8,
    ) -> NodeRef<'pool, D> {
        if layer.is_leaf::<D>() {
            let rect = layer
                .rect()
                .expect("Cannot construct a node at this layer from individual cells");
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
    pub fn get_corner<'pool>(
        &'pool self,
        node: impl NodeRefTrait<'pool, D = D>,
        index: usize,
    ) -> Result<NodeRef<'pool, D>, u8> {
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
    pub fn subdivide<'pool>(
        &'pool self,
        node: impl NodeRefTrait<'pool, D = D>,
    ) -> Result<Vec<NodeRef<'pool, D>>, u8> {
        (0..D::BRANCHING_FACTOR)
            .map(|i| self.get_corner(node, i))
            .try_collect()
    }
    /// Creates a node one layer lower containing the contents of the center of
    /// the node.
    ///
    /// If the node is below `Layer(2)`, returns `Err(LayerTooSmall)`.
    pub fn centered_inner<'pool>(
        &'pool self,
        node: impl NodeRefTrait<'pool, D = D>,
    ) -> Result<NodeRef<'pool, D>, LayerTooSmall> {
        self.assert_owns_node(node);

        Ok(match node.as_enum() {
            NodeRefEnum::Leaf(n) => {
                self.get_from_cells(super::cells::shrink_centered::<D>(n.cells())?)
            }
            NodeRefEnum::NonLeaf(n) => {
                let child_index_bitmask = D::BRANCHING_FACTOR - 1;
                let new_children = self
                    .subdivide(n)
                    .map_err(|_| LayerTooSmall)?
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
    /// Creates a node from a 2^NDIM block of nodes at the same layer, but the
    /// result is offset by a vector (modulo the size of a node at that layer).
    pub fn get_offset_child<'pool>(
        &'pool self,
        offset: &BigVec<D>,
        sources: Vec<NodeRef<'pool, D>>,
    ) -> NodeRef<'pool, D> {
        assert_eq!(D::BRANCHING_FACTOR, sources.len());
        let layer = sources[0].layer();
        let largest_aligned_layer = Layer::largest_aligned(offset).unwrap_or(layer);
        self._get_offset_child(offset, sources, largest_aligned_layer)
    }
    fn _get_offset_child<'pool>(
        &'pool self,
        offset: &BigVec<D>,
        sources: Vec<NodeRef<'pool, D>>,
        largest_aligned_layer: Layer,
    ) -> NodeRef<'pool, D> {
        assert_eq!(D::BRANCHING_FACTOR, sources.len());
        let layer = sources[0].layer();
        if layer <= largest_aligned_layer {
            sources[0]
        } else if layer.is_leaf::<D>() {
            let sources = sources
                .iter()
                .map(|n| n.as_leaf().unwrap())
                .take(D::BRANCHING_FACTOR)
                .collect_vec();
            let big_cell_offset: BigVec<D> = offset & &(layer.big_len() - 1);
            let cell_offset = big_cell_offset.to_uvec();
            self.get_from_fn(layer, move |pos| {
                let source_pos = pos.clone() + &cell_offset;
                let source_index = layer
                    .parent_layer()
                    .small_non_leaf_child_index(source_pos.clone());
                sources[source_index].leaf_cell_at_pos(source_pos)
            })
        } else {
            // These source nodes are conceptually the children of one combined
            // node, but we keep them separate for performance. (Finding nodes
            // in the pool isn't free!) Let's call this combined node the
            // "source node."
            let sources = sources
                .iter()
                .map(|n| n.as_non_leaf().unwrap())
                .take(D::BRANCHING_FACTOR)
                .collect_vec();
            // (optimization for nodes with a single cell state)
            if sources[0].single_state().is_some()
                && sources.iter().map(|n| n.single_state()).all_equal()
            {
                return sources[0].as_ref(); // All nodes are the same single state.
            }
            // The node we will return is somewhere inside the source node. The
            // source node has 4^NDIM grandchildren; there must be some 3^NDIM
            // block of those grandchildren that still contains the node to
            // return. And each 2^NDIM block of grandchildren within that 3^NDIM
            // block contains the corresponding child of the node to return. So
            // first, find the offset (in terms of grandchildren) of that 3^NDIM
            // block.
            let grandchild_offset_1 =
                ((offset >> layer.child_layer().to_u32()) & &BigInt::one()).to_uvec();
            self.join_nodes((0..D::BRANCHING_FACTOR).map(|i| {
                // For each child of the result, get the 2^NDIM block of
                // grandchildren that contains it.
                let grandchild_offset_2 = Layer(1).leaf_pos(i) + &grandchild_offset_1;
                let sources_for_new_child = (0..D::BRANCHING_FACTOR)
                    .map(|j| {
                        let grandchild_pos = Layer(1).leaf_pos(j) + &grandchild_offset_2;
                        let child_index = Layer(1).leaf_cell_index(grandchild_pos.clone() >> 1);
                        let grandchild_index = Layer(1).leaf_cell_index(grandchild_pos & 1);
                        sources[child_index].child_at_index(grandchild_index)
                    })
                    .collect_vec();
                self._get_offset_child(offset, sources_for_new_child, largest_aligned_layer)
            }))
        }
    }

    /// Safely extend the lifetime of a node reference from this pool.
    pub fn extend_lifetime<'this, 'other>(
        &'this self,
        node: impl NodeRefTrait<'other, D = D>,
    ) -> NodeRef<'this, D> {
        self.assert_owns_node(node);

        unsafe { std::mem::transmute::<NodeRef<'_, D>, NodeRef<'this, D>>(node.as_ref()) }
    }
    /// Creates a node in this pool identical to one from another pool.
    pub fn copy_from_other_pool<'this, 'other>(
        &'this self,
        node: impl NodeRefTrait<'other, D = D>,
    ) -> NodeRef<'this, D> {
        if self == node.pool() {
            // If the pools are actually the same, `extend_lifetime()` will succeed.
            self.extend_lifetime(node)
        } else {
            self._copy_from_other_pool(node.as_ref(), &mut HashMap::default())
        }
    }
    fn _copy_from_other_pool<'this, 'other>(
        &'this self,
        node: NodeRef<'other, D>,
        convert_table: &mut HashMap<NodeRef<'other, D>, NodeRef<'this, D>>,
    ) -> NodeRef<'this, D> {
        if let Some(already_computed) = convert_table.get(&node) {
            return *already_computed;
        }
        let ret = match node.as_enum() {
            NodeRefEnum::Leaf(node) => self.get_from_cells(node.cells()),
            NodeRefEnum::NonLeaf(node) => self.join_nodes(
                node.children()
                    .map(|child| self._copy_from_other_pool(child, convert_table)),
            ),
        };
        convert_table.insert(node, ret);
        ret
    }

    /// Creates an identical node except with the cell at the given position
    /// (modulo the node length along each axis) modified.
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn set_cell<'pool>(
        &'pool self,
        node: impl NodeRefTrait<'pool, D = D>,
        pos: &BigVec<D>,
        cell_state: u8,
    ) -> NodeRef<'pool, D> {
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

    /// Returns a simulation guard, invalidating the results cache partially or
    /// fully if necessary.
    ///
    /// This method blocks if other parameters are being used for simulation at
    /// the same time.
    pub fn sim_with<'pool>(&'pool self, params: HashLifeResultParams) -> SimCacheGuard<'pool, D> {
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
            pool: self,
            sim_lock: lock,
        }
    }

    /// Returns the simulation lock, invalidating the results cache partially or
    /// fully if necessary.
    ///
    /// If the lock cannot be acquired, returns `Err` containing the current parameters.
    pub fn try_sim_with<'pool>(
        &'pool self,
        params: HashLifeResultParams,
    ) -> Option<SimCacheGuard<'pool, D>> {
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
            pool: self,
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

// Conceptually, the `*const RawNode`s held by `NodePool` are just references,
// with lifetimes dependent on the node pool. `&RawNode` is `Send + Sync`, so
// it's safe to implement `Send` and `Sync` on `NodePool` as well.
unsafe impl<D: Dim> Send for NodePool<D> {}
unsafe impl<D: Dim> Sync for NodePool<D> {}

/// Node reference that also holds an `Arc` reference to the pool instead of
/// borrowing it.
#[derive(Debug)]
pub struct ArcNode<D: Dim> {
    pool: SharedNodePool<D>,
    raw_node_ptr: *const RawNode<D>,
}
impl<D: Dim> Drop for ArcNode<D> {
    fn drop(&mut self) {
        // Decrement the refcount.
        self.pool
            .access()
            .arc_nodes
            .lock()
            .entry(self.raw_node_ptr)
            .and_modify(|refcount| *refcount -= 1);
    }
}
impl<D: Dim> Clone for ArcNode<D> {
    fn clone(&self) -> Self {
        unsafe { Self::new(&self.pool().access(), self.raw_node_ptr) }
    }
}
impl<D: Dim> PartialEq for ArcNode<D> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.raw_node_ptr, other.raw_node_ptr)
    }
}
impl<D: Dim> Eq for ArcNode<D> {}
impl<'pool, D: Dim, N: NodeRefTrait<'pool, D = D>> PartialEq<N> for ArcNode<D> {
    fn eq(&self, other: &N) -> bool {
        std::ptr::eq(self.raw_node_ptr, other.as_ref().as_raw())
    }
}
impl<'pool, D: Dim, N: NodeRefTrait<'pool, D = D>> From<N> for ArcNode<D> {
    #[inline]
    fn from(n: N) -> Self {
        unsafe { Self::new(n.pool(), n.as_ref().as_raw()) }
    }
}
impl<D: Dim> ArcNode<D> {
    /// Creates an `ArcNode` from raw parts.
    ///
    /// # Saftey
    ///
    /// `raw_node_ptr` must point to a valid node obtained from `pool`.
    #[inline]
    unsafe fn new(pool: &NodePool<D>, raw_node_ptr: *const RawNode<D>) -> Self {
        // Increment the refcount for this node.
        pool.arc_nodes
            .lock()
            .entry(raw_node_ptr)
            .and_modify(|refcount| *refcount += 1)
            .or_insert(1);

        Self {
            pool: pool.new_ref(),
            raw_node_ptr,
        }
    }

    /// Returns the pool that the node is stored in.
    #[inline]
    pub fn pool(&self) -> &SharedNodePool<D> {
        &self.pool
    }

    /// Returns a reference to the raw node structure.
    #[inline]
    fn as_raw(&self) -> &RawNode<D> {
        unsafe { &*self.raw_node_ptr }
    }
    /// Returns the layer of the node.
    #[inline]
    pub fn layer(&self) -> Layer {
        self.as_raw().layer()
    }

    /// Returns a `NodeRef` of the node.
    ///
    /// # Panics
    ///
    /// This method panics if `pool` is not a reference to the pool that this
    /// node is stored in.
    #[inline]
    pub fn as_ref<'pool>(&self, pool: &'pool NodePool<D>) -> NodeRef<'pool, D> {
        assert_eq!(Arc::as_ptr(&self.pool.0), pool.this.as_ptr());
        unsafe { NodeRef::new(pool, &*self.raw_node_ptr) }
    }
    /// Returns a `NodeRefWithGuard` of the node.
    #[inline]
    pub fn as_ref_with_guard<'a>(&'a self) -> NodeRefWithGuard<'a, D> {
        unsafe { NodeRefWithGuard::from_ptr_with_guard(self.pool().access(), &*self.raw_node_ptr) }
    }
}

// Conceptually, the `*const RawNode` held by `ArcNode` is just a reference,
// with a lifetime dependent on the node pool. `&RawNode` is `Send + Sync`, so
// it's safe to implement `Send` and `Sync` on `ArcNode` as well.
unsafe impl<D: Dim> Send for ArcNode<D> {}
unsafe impl<D: Dim> Sync for ArcNode<D> {}

/// RAII wrapper around simulation results cache.
#[derive(Debug)]
pub struct SimCacheGuard<'pool, D: Dim> {
    pool: &'pool NodePool<D>,
    /// Guard to make sure that no one else modifies the parameters.
    sim_lock: RwLockReadGuard<'pool, HashLifeResultParams>,
}
impl<'pool, D: Dim> SimCacheGuard<'pool, D> {
    /// Returns the node pool that this guard gives access to.
    pub fn pool(&self) -> &'pool NodePool<D> {
        self.pool
    }
    /// Returns the parameters defining simulation results.
    pub fn params(&self) -> &HashLifeResultParams {
        &*self.sim_lock
    }
}
