//! The cache for NdTree (generalized N-dimensional quadtree) nodes.
//!
//! Reading from and adding to the cache only requires a &NodeCache, but garbage
//! collection requires a &mut NodeCache.

use dashmap::DashSet;
use itertools::Itertools;
use parking_lot::{RwLock, RwLockReadGuard};
use seahash::SeaHasher;
use std::collections::HashMap;
use std::fmt;
use std::hash::BuildHasherDefault;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
use std::sync::{Arc, Mutex};

use super::{utils, Layer, Node, NodeRef, NodeRefEnum, NodeRepr, RawNode};
use crate::dim::Dim;
use crate::ndvec::BigVec;
use crate::num::BigUint;

/// Fast hasher used for nodes.
pub type NodeHasher = BuildHasherDefault<SeaHasher>;

type NodeSet = DashSet<Box<RawNode>, NodeHasher>;
type ArcNodeMap = HashMap<NonNull<RawNode>, Box<AtomicUsize>, NodeHasher>;

/// Cache of NdTree nodes for a single simulation.
#[derive(Debug)]
pub struct NodeCache<D: Dim> {
    /// Information about the representation of a node.
    node_repr: NodeRepr<D>,

    /// Set of canonical instances of nodes.
    ///
    /// A node must not be deleted from this set if it is any of the following:
    /// - the child of another node in the set
    /// - the HashLife result of another node in the set
    /// - present in `arc_nodes`
    nodes: RwLock<NodeSet>,

    /// Map of nodes to atomic reference counts. This only includes "owned"
    /// `ArcNode`s, not references or other non-leaf nodes.
    ///
    /// If you need access to `nodes` and `arc_nodes` at the same time, always
    /// acquire `arc_nodes` *before* `nodes` to prevent deadlocks.
    arc_nodes: Mutex<ArcNodeMap>,
}
impl<D: Dim> NodeCache<D> {
    // TODO: document
    pub fn node_repr(&self) -> &NodeRepr<D> {
        &self.node_repr
    }

    /// Returns a guard which grants access to nodes in the cache.
    pub fn node_access(&self) -> NodeCacheAccess<D> {
        NodeCacheAccess {
            cache: self,
            nodes: self.nodes.read(),
        }
    }

    /// Does a "strong" garbage collection. This invalidates the cache and then
    /// removes all nodes that are not referenced by an ArcNode or reachable
    /// from one as children or HashLife results.
    ///
    /// (Another thread may compute the result of a node as the HashLife results
    /// are being invalidated.)
    pub fn strong_gc(&self) {
        self.invalidate_results();
        self.weak_gc();
    }
    /// Does a "weak" garbage collection. This removes all nodes that are not
    /// referenced by an ArcNode or reachable from one as children or HashLife
    /// results.
    pub fn weak_gc(&self) {
        // It's very important that no new ArcNodes are handed out during GC,
        // since we're relying on them to figure out which nodes to keep.
        // Existing nodes can have their reference count changed, and even be
        // dropped, as long as no *new* nodes are added.
        let mut arc_nodes = self.arc_nodes.lock().unwrap();

        // Step #1: Delete ArcNodes with no references.
        //
        // We don't have to worry about anyone else modifying the atomic value
        // while we're reading it, because if the reference count is zero then
        // obviously no one has a reference to it.
        arc_nodes.retain(|_k, v| v.load(Relaxed) > 0);

        // Step #2: Mark nodes that are reachable from the ArcNodes.
        //
        // We lock the nodes set to prevent anyone adding results to nodes while
        // we do this, because that might point from a node we've already marked
        // to keep to a node that we would otherwise throw away.
        let nodes = self.nodes.write();
        arc_nodes
            .keys()
            .for_each(|ptr| unsafe { ptr.as_ref().mark_gc_keep_recursive(&self.node_repr) });

        // Step #3: Drop all unmarked nodes and clear the marking. This is
        // really the unsafe part.
        nodes.retain(|raw_node| {
            let node_flags = unsafe { raw_node.flags() };
            // clear_gc() returns true if the node was marked and false if it
            // wasn't.
            !node_flags.clear_gc()
        });

        // All done! It is now safe to drop the guards.
        drop(nodes);
        drop(arc_nodes);
    }
    /// Clears the HashLife results cache from every node.
    ///
    /// Note that another thread may add HashLife results to a node while this
    /// function is running.
    pub fn invalidate_results(&self) {
        for raw_node in self.nodes.read().iter() {
            raw_node.set_result(std::ptr::null());
        }
    }
}
impl<D: Dim> Default for NodeCache<D> {
    fn default() -> Self {
        todo!("NodeCache should not implement Default")
    }
}

pub struct NodeCacheAccess<'cache, D: Dim> {
    // TODO: can this be removed?
    cache: &'cache NodeCache<D>,
    nodes: RwLockReadGuard<'cache, DashSet<Box<RawNode>, NodeHasher>>,
}
impl<D: Dim> fmt::Debug for NodeCacheAccess<'_, D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeCacheAccess")
    }
}
impl<D: Dim> Clone for NodeCacheAccess<'_, D> {
    fn clone(&self) -> Self {
        self.cache.node_access()
    }
}
impl<D: Dim> PartialEq for NodeCacheAccess<'_, D> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}
impl<D: Dim> Eq for NodeCacheAccess<'_, D> {}
impl<'cache, D: Dim> NodeCacheAccess<'cache, D> {
    pub fn node_repr(&self) -> &'cache NodeRepr<D> {
        &self.cache.node_repr
    }

    /// Returns the largest possible canonical leaf node containing only state #0.
    pub fn get_empty_base<'s>(&'s self) -> NodeRef<'s, D> {
        self.get_empty(self.node_repr().base_layer())
    }
    /// Returns the canonical node at the given layer containing only state #0.
    pub fn get_empty<'s>(&'s self, layer: Layer) -> NodeRef<'s, D> {
        // Try to do this in O(1) by just looking for an empty node at the right
        // layer.
        self.try_get(&unsafe { RawNode::new_empty_placeholder::<D>(layer) })
            .unwrap_or_else(|| {
                // If that fails, we have to actually make a new empty node.
                if layer <= self.node_repr().base_layer() {
                    // The node will be a leaf.
                    self.get_from_cells(
                        vec![0_u8; layer.num_cells::<D>().unwrap().get()].into_boxed_slice(),
                    )
                } else {
                    // The node is not a leaf, so recurse to get the empty node
                    // smaller than it and use that to form this one.
                    self.join_nodes(vec![
                        self.get_empty(layer.child_layer()).as_raw();
                        D::BRANCHING_FACTOR
                    ])
                }
            })
    }
    /// Returns a reference to the canonical instance of the given node if it
    /// already in the cache; otherwise returns `None`.
    pub fn try_get<'s, 'n>(&'s self, raw_node: &'n RawNode) -> Option<NodeRef<'s, D>> {
        let raw_node_ref: dashmap::setref::one::Ref<Box<RawNode>, _> = self.nodes.get(raw_node)?;
        // Internally, `DashMap` uses `RwLocks` to guard certain parts of the
        // HashMap, so once we throw away the `DashMap::setref::one::Ref` the
        // `RwLock` guard is released and the reference to the data inside there
        // expires. But we know that **as long as there is a read handle on the
        // whole `NodeSet`, no nodes will be deleted** (or modified in a way
        // that requires exclusive access). Even if the `DashMap` itself is
        // resized, the reference to the `RawNode` will remain valid because it
        // is behind a `Box`. That is why this node can live last as long as the
        // reference to this `NodeCacheAccess` lives, and why `NodeRef::new` can
        // safely extend the lifetime to `'s`.
        Some(unsafe {
            NodeRef::new(
                self.cache,
                self,
                NonNull::new(&**raw_node_ref as *const _ as *mut _).unwrap(),
            )
        })
    }
    /// Returns a reference to the canonical instance of the given node, adding
    /// it to the cache if one does not exist.
    pub fn get<'s>(&'s self, node: RawNode) -> NodeRef<'s, D> {
        // This will not overwrite the node already in the cache. (See the test
        // at the bottom of this file.) This call to Node::copy() is safe
        // because we are either creating or searching for the canonical
        // instance of the node -- we aren't going to let anyone else have this
        // one unless it becomes the canonical instance.
        self.nodes.insert(Box::new(unsafe { node.copy() }));

        self.try_get(&node)
            .expect("Node deleted without mutable access to cache")
    }

    pub fn join_nodes<'s, 'n>(&'s self, children: impl Into<Box<[&'n RawNode]>>) -> NodeRef<'s, D> {
        // TODO: note that all children MUST be from this cache
        let children = children.into();
        let node_repr = self.node_repr();
        assert_eq!(children.len(), D::BRANCHING_FACTOR);
        let child_layer = children[0].layer();
        for child in &children[1..] {
            debug_assert_eq!(child_layer, child.layer());
        }

        if child_layer < node_repr.base_layer() {
            // Children are below the base layer, so make a leaf node by
            // combining the cells of the children.
            self.get_from_cells(super::utils::join_cell_squares::<D>(
                &children
                    .iter()
                    .map(|raw_child| self.try_get(raw_child).unwrap().as_leaf().unwrap().cells())
                    .collect_vec(),
            ))
        } else {
            // Children are at or above the base layer, so just make a non-leaf
            // node pointing to them.
            self.get(RawNode::new_non_leaf(node_repr, children))
        }
    }
    pub fn get_from_cells<'s>(&'s self, cells: impl Into<Box<[u8]>>) -> NodeRef<'s, D> {
        let cells = cells.into();
        let node_repr = self.node_repr();
        let layer = Layer::from_num_cells::<D>(cells.len()).expect("Invalid cell count");

        if layer <= node_repr.base_layer() {
            // This node is at or below the base layer, so make a leaf node.
            self.get(RawNode::new_leaf(node_repr, cells))
        } else {
            // This node is above the base layer, so subdivide the cells into
            // smaller leaf nodes and make this a non-leaf node.
            self.join_nodes(
                utils::subdivide_cell_square::<D>(&cells)
                    .unwrap()
                    .map(|subcube| self.get_from_cells(subcube).as_raw())
                    .collect_vec(),
            )
        }
    }
    pub fn subdivide<'s: 'n, 'n>(
        &'s self,
        node: impl Node<'n, D>,
    ) -> Result<Vec<NodeRef<'n, D>>, u8> {
        // TODO: note that the node MUST be from this cache

        // TODO: should not need &self?
        todo!("Test this");

        Ok(match node.as_enum() {
            NodeRefEnum::Leaf(node) => utils::subdivide_cell_square::<D>(node.cells())?
                .map(|subcube| self.get_from_cells(subcube))
                .collect(),
            NodeRefEnum::NonLeaf(node) => node.children().collect(),
        })
    }
    pub fn centered_inner<'s, 'n>(&'s self, node: impl Node<'n, D>) -> Result<NodeRef<'s, D>, ()> {
        // TODO: note that the node MUST be from this cache

        todo!("Test this");

        // Cannot get the centered inner of a node smaller than 4x4.
        if node.layer() < Layer(2) {
            return Err(());
        }

        Ok(match node.as_enum() {
            NodeRefEnum::Leaf(node) => self.get_from_cells(utils::centered_cell_square::<D>(
                node.cells(),
                node.layer().child_layer().to_usize(),
            )),
            NodeRefEnum::NonLeaf(node) => {
                // TODO: write this better -- just grab the cells you need
                // directly instead of subdividing first
                let child_index_bitmask = D::BRANCHING_FACTOR - 1;
                let new_children = self
                    .subdivide(node)
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
                        let grandchildren = self.subdivide(child).unwrap();
                        // Return the grandchild closest to the center.
                        grandchildren[opposite_child_index].as_raw()
                    })
                    .collect_vec();
                self.join_nodes(new_children)
            }
        })
    }

    /// Returns a new node with a cell changed at the given position, modulo the
    /// node length along each axis.
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn set_cell<'s, 'n>(
        &'s self,
        node: impl Node<'n, D>,
        pos: &BigVec<D>,
        cell_state: u8,
    ) -> NodeRef<'s, D> {
        // Modulo the node length along each axis using bitwise AND.
        match node.as_enum() {
            NodeRefEnum::Leaf(node) => {
                // We use modulo_pos() because we have to convert the BigVec to
                // a UVec and we can't know for sure that it will fit otherwise.
                let cell_index = node.pos_to_cell_index(node.modulo_pos(pos).to_uvec());
                // Possible optimization: avoid unpacking cells; just copy the
                // packed cells and modify the cell there.
                let mut new_cells = node.cells().to_vec().into_boxed_slice();
                new_cells[cell_index] = cell_state;
                self.get_from_cells(new_cells)
            }
            NodeRefEnum::NonLeaf(node) => {
                let child_index = node.child_index_with_pos(pos);
                let new_child = self.set_cell(node.child_at_index(child_index), pos, cell_state);
                let mut new_children = node
                    .children()
                    .into_iter()
                    .map(|node| node.as_raw())
                    .collect_vec();
                new_children[child_index] = new_child.as_raw();
                self.join_nodes(new_children)
            }
        }
        .as_ref()
    }
}

/// Shared-ownership access to a node in the cache, which also prevents it from
/// being garbage-collected.
#[derive(Debug)]
pub struct ArcNode<D: Dim> {
    /// Access to the cache, to make sure the cache doesn't get dropped.
    cache: Arc<NodeCache<D>>,
    /// Pointer to the node.
    node_ptr: NonNull<RawNode>,
    /// Pointer to the refcount for the node.
    ref_count_ptr: NonNull<AtomicUsize>,
}
impl<D: Dim> Clone for ArcNode<D> {
    fn clone(&self) -> Self {
        // Increment the reference count.
        unsafe { self.ref_count_ptr.as_ref() }.fetch_add(1, Relaxed);
        Self {
            cache: Arc::clone(&self.cache),
            node_ptr: self.node_ptr,
            ref_count_ptr: self.ref_count_ptr,
        }
    }
}
impl<D: Dim> Drop for ArcNode<D> {
    fn drop(&mut self) {
        // Decrement the reference count.
        unsafe { self.ref_count_ptr.as_ref() }.fetch_sub(1, Relaxed);
        // We don't need to worry about actually removing this node from the
        // arc_nodes map; that's part of garbage collection.
    }
}
impl<D: Dim> PartialEq for ArcNode<D> {
    fn eq(&self, other: &Self) -> bool {
        self.node_ptr == other.node_ptr
    }
}
impl<D: Dim> Eq for ArcNode<D> {}
impl<D: Dim> ArcNode<D> {
    pub fn new<'n>(cache: Arc<NodeCache<D>>, node: impl Node<'n, D>) -> Self {
        let node_ptr = NonNull::from(node.as_raw());
        let ref_count_ptr = NonNull::from(
            &**cache
                // This conflicts with garbage collection, but not normal
                // reading of the cache, so we take exclusive (write) access to
                // the arc_nodes map. If DashMap had an atomic "get or insert"
                // method, we could use that instead of a Mutex<HashMap>, but
                // whatever. This shouldn't be called too frequently.
                .arc_nodes
                .lock()
                .unwrap()
                // Increment the reference counter for this node, or insert a
                // new reference counter with the value 1.
                .entry(node_ptr)
                .and_modify(|ref_count| {
                    ref_count.fetch_add(1, Relaxed);
                })
                .or_insert_with(|| Box::new(AtomicUsize::new(1))),
        );
        Self {
            cache,
            node_ptr,
            ref_count_ptr,
        }
    }
    pub fn cache(&self) -> &Arc<NodeCache<D>> {
        &self.cache
    }
    pub fn as_ref<'cache>(
        &self,
        node_access: &'cache NodeCacheAccess<'cache, D>,
    ) -> NodeRef<'cache, D> {
        unsafe { NodeRef::new(node_access.cache, node_access, self.node_ptr.as_ref()) }
    }
    pub fn as_raw(&self) -> &RawNode {
        unsafe { self.node_ptr.as_ref() }
    }

    pub fn population(&self) -> &BigUint {
        todo!("COUNT POPULATION")
    }
}

#[cfg(test)]
mod tests {

    /// Only the first member of this struct counts for equality and hashing.
    struct Pair(usize, usize);
    impl PartialEq for Pair {
        fn eq(&self, other: &Self) -> bool {
            self.0 == other.0
        }
    }
    impl Eq for Pair {}
    impl std::hash::Hash for Pair {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.0.hash(state);
        }
    }

    // Test that inserting an equal key into a DashSet does NOT update the key.
    // This isn't documented by Dashmap, but it's important for the NodeCache
    // and is true for now because DashSet uses DashMap which ultimately
    // delegates to std::collections::HashMap, which does make this promise.
    #[test]
    fn test_dashset_insert_eq() {
        let h = dashmap::DashSet::new();
        // These values should be kept.
        assert!(h.insert(Pair(10, 4)));
        assert!(h.insert(Pair(20, 4)));
        // These values should not be reachable.
        assert!(!h.insert(Pair(10, 8)));
        assert!(!h.insert(Pair(20, 8)));
        // Check that the first two inserts persisted while the second two did
        // not do anything.
        assert_eq!(4, h.get(&Pair(10, 0)).unwrap().1);
        assert_eq!(4, h.get(&Pair(20, 0)).unwrap().1);
    }
}
