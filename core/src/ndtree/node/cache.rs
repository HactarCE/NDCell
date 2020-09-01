//! Cache for ND-tree (generalized N-dimensional quadtree) nodes.
//!
//! Reading from and adding to the cache only requires a &NodeCache, but garbage
//! collection requires a &mut NodeCache.

use itertools::Itertools;
use seahash::SeaHasher;
use std::fmt;
use std::hash::{BuildHasher, BuildHasherDefault, Hash, Hasher};
use std::ops::Deref;
use std::sync::{Arc, Mutex, Weak};
use weak_table::WeakHashSet;

use super::{Layer, NodeRef, NodeRefEnum, NodeRefTrait, RawNode};
use crate::dim::Dim;
use crate::ndvec::BigVec;

/// Fast hasher used for nodes.
pub type NodeHasher = BuildHasherDefault<SeaHasher>;

type NodeShard<D> = Mutex<WeakHashSet<Weak<RawNode<D>>, NodeHasher>>;
// TODO: measure perf with different shard counts
const SHARD_COUNT: usize = 64;

const CACHE_UNLOCK_ERROR_MSG: &str = "Failed to unlock node cache mutex";

/// Cache of ND-tree nodes for a single simulation.
pub struct NodeCacheInner<D: Dim> {
    /// Set of canonical instances of nodes.
    ///
    /// A node must only be deleted from this set if there are no "strong"
    /// (`Arc<T>`) references to it, and this set must hold the only "weak"
    /// reference.
    ///
    /// The set is split into many "shards" to reduce contention (a la
    /// https://docs.rs/crate/sharded/0.0.5). We use `Mutex` instead of `RwLock`
    /// because any reader could turn into a writer if the node it's looking for
    /// is not present.
    node_shards: Box<[NodeShard<D>]>,
    /// Cache of empty nodes at each layer. The index of the vector is the layer
    /// of the node.
    empty_nodes: Mutex<Vec<Arc<RawNode<D>>>>,
}
impl<D: Dim> fmt::Debug for NodeCacheInner<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // On nightly, we could `#[derive(Debug)]`. :(
        write!(f, "node_shards=")?;
        fmt::Debug::fmt(&self.node_shards[..], f)?;
        Ok(())
    }
}
impl<D: Dim> Default for NodeCacheInner<D> {
    fn default() -> Self {
        // We should be able to `#[derive(Default)]` but `Default` isn't
        // implemented for arrays longer than 32 elements, because that needs
        // const generics. :(
        Self {
            node_shards: std::iter::repeat_with(|| NodeShard::default())
                .take(SHARD_COUNT)
                .collect_vec()
                .into_boxed_slice(),
            empty_nodes: Mutex::new(vec![]),
        }
    }
}

/// Atomic reference-counted pointer to ND-tree node cache.
#[derive(Default, Clone)]
pub struct NodeCache<D: Dim>(Arc<NodeCacheInner<D>>);
impl<D: Dim> fmt::Debug for NodeCache<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#p}", self.0)
    }
}
impl<D: Dim> Deref for NodeCache<D> {
    type Target = NodeCacheInner<D>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<D: Dim> PartialEq for NodeCache<D> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl<D: Dim> Eq for NodeCache<D> {}
impl<D: Dim> Hash for NodeCache<D> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::ptr::hash(Arc::as_ptr(&self.0), state)
    }
}
impl<D: Dim> NodeCache<D> {
    /// Does a "strong" garbage collection. This invalidates all HashLife
    /// results, deletes all unused empty nodes, and then removes all nodes not
    /// "strongly" referenced.
    pub fn strong_gc(&self) {
        self.invalidate_results();
        self.clear_unused_empty_nodes();
        self.weak_gc();
    }
    /// Does a "weak" garbage collection. This removes all nodes that are not
    /// "strongly" referenced and have already been dropped but still have an
    /// entry in the node set.
    ///
    /// Another thread may add more (unused) nodes while this method is running,
    /// but otherwise this operation is idempotent.
    pub fn weak_gc(&self) {
        for shard in self.node_shards.iter() {
            shard.lock().expect(CACHE_UNLOCK_ERROR_MSG).remove_expired();
        }
    }
    /// Clears the HashLife results cache from every node.
    ///
    /// Note that another thread may add HashLife results to a node while this
    /// function is running.
    pub fn invalidate_results(&self) {
        for shard in self.node_shards.iter() {
            for raw_node in shard.lock().expect(CACHE_UNLOCK_ERROR_MSG).iter() {
                raw_node.set_result(None);
            }
        }
    }
    /// Clears empty nodes that have no other references.
    pub fn clear_unused_empty_nodes(&self) {
        let mut empty_nodes = self.empty_nodes.lock().expect(CACHE_UNLOCK_ERROR_MSG);
        // Iterate in reverse so that we drop large nodes first (since larger
        // nodes contain references to smaller ones).
        while let Some(largest_empty_node) = empty_nodes.last() {
            if Arc::strong_count(&largest_empty_node) == 1 {
                // This is the only strong reference ... probably. It's ok if
                // another thread grabs a reference to this node right now,
                // because the node will still remain in `node_shards`, and it
                // will be added back to `empty_nodes` next time someone calls
                // `get_empty()` looking for it.

                // But chances are that this is the only strong reference, so
                // dropping it will drop the node and maybe free up some memory.
                empty_nodes.pop();
            } else {
                // Someone else is using this node, and transitively all the
                // ones below it (except maybe some leaf nodes, but those are
                // tiny and finite).
                break;
            }
        }
    }

    /// Asserts that the node is from this cache.
    fn assert_owns_node<'n>(&self, node: impl NodeRefTrait<'n, D = D>) {
        assert_eq!(
            self,
            node.cache(),
            "Attempt to operate on a node from a different cache",
        );
    }

    /// Returns the largest possible canonical leaf node containing only state #0.
    pub fn get_empty_base(&self) -> ArcNode<D> {
        self.get_empty(Layer::base::<D>())
    }
    /// Returns the canonical node at the given layer containing only state #0.
    pub fn get_empty(&self, layer: Layer) -> ArcNode<D> {
        let mut empty_nodes = self.empty_nodes.lock().expect(CACHE_UNLOCK_ERROR_MSG);
        // Add empty nodes until we have enough.
        while empty_nodes.len() <= layer.to_usize() {
            let next_layer = Layer(empty_nodes.len() as u32);
            if next_layer.is_leaf::<D>() {
                empty_nodes.push(self.get(RawNode::new_empty_leaf(next_layer)).into_raw());
            } else {
                let empty_node_children =
                    vec![Arc::clone(empty_nodes.last().unwrap()); D::BRANCHING_FACTOR];
                let new_empty_node = self.get(RawNode::new_non_leaf(
                    empty_node_children.into_boxed_slice(),
                ));
                empty_nodes.push(new_empty_node.into_raw());
            }
        }
        let ret = &empty_nodes[layer.to_usize()];
        debug_assert_eq!(layer, ret.layer());
        unsafe { NodeRef::new(self, ret) }.into()
    }

    /// Returns the canonical instance of a node, adding it to the cache if one
    /// does not exist.
    pub fn get(&self, raw_node: RawNode<D>) -> ArcNode<D> {
        let mut hash = NodeHasher::default().build_hasher();
        raw_node.hash(&mut hash);
        // Hash a little extra, so that the hash here is distinct from the one used within the individual shard.
        SHARD_COUNT.hash(&mut hash);

        let shard_index = hash.finish() as usize % SHARD_COUNT;
        let mut shard = self.node_shards[shard_index]
            .lock()
            .expect(CACHE_UNLOCK_ERROR_MSG);

        // This is ugly, but there's just no API yet for inserting a key into a
        // HashSet and immediately getting a reference back.
        let raw_node = shard.get(&raw_node).unwrap_or_else(|| {
            let n = Arc::new(raw_node);
            shard.insert(Arc::clone(&n));
            let x = shard.get(&n).unwrap();
            x
        });
        unsafe { NodeRef::new(self, &raw_node) }.into()
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
    pub fn join_nodes<'n, N: Into<NodeCow<'n, D>>>(
        &self,
        children: impl IntoIterator<Item = N>,
    ) -> ArcNode<D> {
        let children = children
            .into_iter()
            .map(|x| x.into())
            .inspect(|node| self.assert_owns_node(node))
            .map(|node| Arc::clone(node.as_raw()))
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
            self.get(RawNode::new_non_leaf(children))
        }
    }
    /// Creates a node containing the given cells.
    pub fn get_from_cells(&self, cells: impl Into<Box<[u8]>>) -> ArcNode<D> {
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
    pub fn get_corner<'s: 'n, 'n>(
        &'s self,
        node: impl NodeRefTrait<'n, D = D>,
        index: usize,
    ) -> Result<NodeCow<'n, D>, u8> {
        self.assert_owns_node(node);

        Ok(match node.as_enum() {
            NodeRefEnum::Leaf(n) => self
                .get_from_cells(super::cells::get_corner::<D>(n.cells(), index)?)
                .into(),
            NodeRefEnum::NonLeaf(n) => n.child_at_index(index).into(),
        })
    }
    /// Subdivides the node into 2^NDIM smaller nodes at one layer lower. If the
    /// node is only a single cell, returns `Err()` containing that cell state.
    ///
    /// The return value is borrowed if `node` is a non-leaf and owned if `node`
    /// is a leaf.
    pub fn subdivide<'s: 'n, 'n>(
        &'s self,
        node: impl NodeRefTrait<'n, D = D>,
    ) -> Result<Vec<NodeCow<'n, D>>, u8> {
        (0..D::BRANCHING_FACTOR)
            .map(|i| self.get_corner(node, i))
            .try_collect()
    }
    /// Creates a node one layer lower containing the contents of the center of
    /// the node.
    ///
    /// If the node is below `Layer(2)`, returns `Err(())`.
    pub fn centered_inner<'n>(&self, node: impl NodeRefTrait<'n, D = D>) -> Result<ArcNode<D>, ()> {
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
                        let mut grandchildren = child.subdivide().unwrap();
                        // Return the grandchild closest to the center.
                        grandchildren.remove(opposite_child_index).into_arc()
                    });
                self.join_nodes(new_children)
            }
        })
    }

    /// Creates an identical node except with the cell at the given position
    /// (modulo the node length along each axis) modified.
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn set_cell<'s, 'n>(
        &'s self,
        node: impl NodeRefTrait<'n, D = D>,
        pos: &BigVec<D>,
        cell_state: u8,
    ) -> ArcNode<D> {
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
                new_children[child_index] = new_child.as_ref();
                self.join_nodes(new_children)
            }
        }
    }
}

/// Node reference that also holds an `Arc` reference to the cache instead of
/// borrowing it.
#[derive(Debug, Clone)]
pub struct ArcNode<D: Dim> {
    cache: NodeCache<D>,
    raw_node: Arc<RawNode<D>>,
}
impl<D: Dim> PartialEq for ArcNode<D> {
    fn eq(&self, other: &Self) -> bool {
        Arc::eq(&self.raw_node, &other.raw_node)
    }
}
impl<D: Dim> Eq for ArcNode<D> {}
impl<D: Dim> Hash for ArcNode<D> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw_node.hash(state)
    }
}
impl<D: Dim> ArcNode<D> {
    /// Creates an `ArcNode` from raw parts.
    ///
    /// # Saftey
    ///
    /// `raw_node` must be a valid node obtained from `cache`.
    #[inline]
    pub unsafe fn new(cache: NodeCache<D>, raw_node: Arc<RawNode<D>>) -> Self {
        Self { cache, raw_node }
    }

    /// Returns the `RawNode` backing this node.
    #[inline]
    pub fn into_raw(self) -> Arc<RawNode<D>> {
        self.raw_node
    }
}
impl<'node, D: Dim> NodeRefTrait<'node> for &'node ArcNode<D> {
    type D = D;

    #[inline]
    fn as_raw(self) -> &'node Arc<RawNode<Self::D>> {
        &self.raw_node
    }
    #[inline]
    fn as_ref(self) -> NodeRef<'node, Self::D> {
        unsafe { NodeRef::new(&self.cache, &self.raw_node) }
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'node, Self::D> {
        self.as_ref().as_enum()
    }
    #[inline]
    fn cache(self) -> &'node NodeCache<Self::D> {
        &self.cache
    }
}
impl<'node, D: Dim, N: NodeRefTrait<'node, D = D>> From<N> for ArcNode<D> {
    #[inline]
    fn from(n: N) -> Self {
        Self {
            cache: n.cache().clone(),
            raw_node: Arc::clone(n.as_raw()),
        }
    }
}

/// `Arc`-on-write ND-tree node smart pointer.
#[derive(Debug, Clone)]
pub enum NodeCow<'node, D: Dim> {
    /// "Owned" `ArcNode`.
    Arc(ArcNode<D>),
    /// "Borrowed" `NodeRef`.
    Ref(NodeRef<'node, D>),
}
impl<D: Dim> PartialEq for NodeCow<'_, D> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}
impl<D: Dim> Eq for NodeCow<'_, D> {}
impl<D: Dim> Hash for NodeCow<'_, D> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}
impl<D: Dim> NodeCow<'_, D> {
    /// Returns the `ArcNode`, or creates one from a `NodeRef`.
    #[inline]
    pub fn into_arc(self) -> ArcNode<D> {
        match self {
            NodeCow::Arc(n) => n,
            NodeCow::Ref(n) => n.into(),
        }
    }
}
impl<'node, D: Dim> NodeRefTrait<'node> for &'node NodeCow<'node, D> {
    type D = D;

    #[inline]
    fn as_raw(self) -> &'node Arc<RawNode<Self::D>> {
        match self {
            NodeCow::Arc(n) => n.as_raw(),
            NodeCow::Ref(n) => n.as_raw(),
        }
    }
    #[inline]
    fn as_ref(self) -> NodeRef<'node, Self::D> {
        match self {
            NodeCow::Arc(n) => n.as_ref(),
            NodeCow::Ref(n) => *n,
        }
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'node, Self::D> {
        self.as_ref().as_enum()
    }
    #[inline]
    fn cache(self) -> &'node NodeCache<Self::D> {
        match self {
            NodeCow::Arc(n) => n.cache(),
            NodeCow::Ref(n) => n.cache(),
        }
    }
}
impl<D: Dim> From<ArcNode<D>> for NodeCow<'_, D> {
    #[inline]
    fn from(n: ArcNode<D>) -> Self {
        Self::Arc(n)
    }
}
impl<'node, D: Dim, T: NodeRefTrait<'node, D = D>> From<T> for NodeCow<'node, D> {
    #[inline]
    fn from(n: T) -> Self {
        Self::Ref(n.as_ref())
    }
}
