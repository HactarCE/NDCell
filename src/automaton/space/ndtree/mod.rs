use seahash::SeaHasher;
use std::hash::Hasher;
use std::marker::PhantomData;
use std::rc::Rc;

mod cache;

use super::*;
use cache::NdTreeCache;

/// An N-dimensional generalization of a quadtree.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
// TODO: custom Debug implementation
pub struct NdTree<T: CellType, D: Dim> {
    node: NdTreeNodeRef<T, D>,
}

/// A 1D grid represented as a bintree.
pub type NdTree1D<T> = NdTree<T, Vec1D>;
/// A 2D grid represented as a quadtree.
pub type NdTree2D<T> = NdTree<T, Vec2D>;
/// A 3D grid represented as an octree.
pub type NdTree3D<T> = NdTree<T, Vec3D>;
/// A 4D grid represented as a tree with nodes of degree 16.
pub type NdTree4D<T> = NdTree<T, Vec4D>;
/// A 5D grid represented as a tree with nodes of degree 32.
pub type NdTree5D<T> = NdTree<T, Vec5D>;
/// A 6D grid represented as a tree with nodes of degree 64.
pub type NdTree6D<T> = NdTree<T, Vec6D>;

/// An interned NdTreeNode.
pub type NdTreeNodeRef<T, D> = Rc<NdTreeNode<T, D>>;

/// A single node in the NdTree, which contains information about its layer
/// (base-2 logarithm of hypercube side length) and its children.
#[derive(Debug, Clone)]
pub struct NdTreeNode<T: CellType, D: Dim> {
    /// The "layer" of this node (base-2 logarithm of hypercube side length).
    layer: usize,

    /// The child of this node, which is either a single cell state or a 2^d
    /// hypercube of nodes.
    ///
    /// If layer == 0, then it must be a single cell state.
    child: NdTreeChild<T, D>,

    hash_code: u64,

    tree_set: NdTreeCache<T, D>,

    phantom: PhantomData<D>,
}

/// An NdTreeNode's child.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NdTreeChild<T: CellType, D: Dim> {
    /// All cells within this node are the same cell state.
    Leaf(T),

    /// A 2^d hypercube of interned subnodes.
    ///
    /// Until rust-lang #44580 (RFC 2000) is resolved, there's no way to use
    /// D::NDIM as the array size. It might be worth implementing a custom
    /// unsafe type for this, but at the time of writing such an optimization
    /// would be entirely premature.
    Branch(Vec<NdTree<T, D>>),
}

impl<T: CellType, D: Dim> Default for NdTree<T, D> {
    fn default() -> Self {
        Self::new()
    }
}
impl<T: CellType, D: Dim> NdTree<T, D> {
    /// Create a new empty N-dimensional tree stucture with an empty node cache.
    pub fn new() -> Self {
        let tree_set = NdTreeCache::default();
        let node = NdTreeNode::empty(tree_set, 1).intern();
        Self { node }
    }
}

impl<T: CellType, D: Dim> NdTreeNode<T, D> {
    fn empty(tree_set: NdTreeCache<T, D>, layer: usize) -> Self {
        Self::with_child(tree_set, layer, NdTreeChild::default())
    }
    fn with_child(tree_set: NdTreeCache<T, D>, layer: usize, child: NdTreeChild<T, D>) -> Self {
        let mut hasher = SeaHasher::new();
        layer.hash(&mut hasher);
        child.hash(&mut hasher);
        let ret = Self {
            layer,
            child,
            hash_code: hasher.finish(),
            tree_set,
            phantom: PhantomData,
        };
        ret
    }
    /// Checks whether an equivalent node is present in the cache. If it is,
    /// destroys this one and returns the equivalent node from the cache; if
    /// not, adds this node to the cache and returns it.
    fn intern(self) -> NdTreeNodeRef<T, D> {
        let existing_node = self.tree_set.borrow().get(&self).clone();
        existing_node.unwrap_or_else(|| {
            let ret = Rc::new(self);
            ret.tree_set.borrow_mut().insert(ret.clone());
            ret
        })
    }
}

impl<T: CellType, D: Dim> Default for NdTreeChild<T, D> {
    fn default() -> Self {
        Self::Leaf(T::default())
    }
}
