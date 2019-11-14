// use std::collections::HashMap;
use std::default::Default;
use std::marker::PhantomData;
use std::rc::Rc;

use super::*;

// const nodes: HashMap<>

// struct NdTreeHash

/// An N-dimensional generalization of a quadtree.
#[derive(Debug)]
pub struct NdTree<T: CellType, D: Dim>(NdSubTree<T, D>);
impl<T: CellType, D: Dim> NdTree<T, D> {
    /// Creates a new empty NdTree.
    pub fn new() -> Self {
        Self(NdTreeNode::<T, D>::default().intern())
    }

    /// Returns the cell at the given position (or the default background state,
    /// if the position is out of bounds).
    pub fn get_cell(&self, pos: NdVec<D>) -> T {
        self.0.get_cell(pos)
    }

    /// Returns a new NdTree with the cell at the given position set to the
    /// given value.
    pub fn set_cell(&self, pos: NdVec<D>, cell_value: T) -> Self {
        Self(self.0.set_cell(pos, cell_value).intern())
    }
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
pub type NdSubTree<T, D> = Rc<NdTreeNode<T, D>>;

/// A single node in the NdTree, which contains information about its layer
/// (base-2 logarithm of hypercube side length) and its children.
#[derive(Debug, Clone)]
pub struct NdTreeNode<T: CellType, D: Dim> {
    /// The "layer" of this node (base-2 logarithm of hypercube side length).
    layer: usize,

    /// The child of this node, which is either a single cell state or a 2^n
    /// hypercube of nodes.
    ///
    /// If layer == 0, then it must be a single cell state.
    child: NdTreeChild<T, D>,

    /// Phantom data to parametrize over dimension values.
    phantom: PhantomData<D>,
}

impl<T: CellType, D: Dim> Default for NdTreeNode<T, D> {
    fn default() -> Self {
        Self {
            layer: 1,
            child: NdTreeChild::default(),
            phantom: PhantomData,
        }
    }
}

/// An NdTreeNode's child.
#[derive(Debug, Clone)]
pub enum NdTreeChild<T: CellType, D: Dim> {
    /// All cells within this node are the same cell state.
    Leaf(T),

    /// A 2^n hypercube of interned subnodes.
    ///
    /// Until rust-lang #44580 (RFC 2000) is
    /// resolved, there's no way to use D::NDIM as the array size. It might be
    /// worth implementing a custom unsafe type for this, but at the time of writing such an optimization would be entirely premature.
    Branch(Vec<NdSubTree<T, D>>),
}
impl<T: CellType, D: Dim> Default for NdTreeChild<T, D> {
    fn default() -> Self {
        Self::Leaf(T::default())
    }
}

impl<T: CellType, D: Dim> NdTreeNode<T, D> {
    /// The number of branches for each node, which is 2 to the power of the
    /// number of dimensions.
    const BRANCHES: usize = 1 << D::NDIM;
    /// The bitmask for branch indices.
    const BRANCH_IDX_MASK: usize = Self::BRANCHES - 1;
    fn new(layer: usize, child: NdTreeChild<T, D>) -> Rc<Self> {
        Self {
            layer,
            child,
            phantom: PhantomData,
        }
        .intern()
    }
    fn intern(self) -> Rc<Self> {
        // TODO implement hashing and interning -- also simplify structures (e.g. simplify children and ensure that children are interned)
        Rc::new(self)
    }
    fn get_branch_index_top(&self, pos: NdVec<D>) -> usize {
        // If this is the top-level node, invert the branch index because
        // negative numbers should be less than ("southwest" of) positive
        // numbers, but their highest bits are 1, which would push them
        // "northeast."
        self.get_branch_index(pos) ^ Self::BRANCH_IDX_MASK
    }
    fn get_branch_index(&self, pos: NdVec<D>) -> usize {
        // Take the Nth bit (where N = self.layer) of each coordinate, and use
        // those to form an integer index.
        let mut index: usize = 0;
        for axis in D::axes() {
            index <<= 1;
            index |= ((pos[axis] >> (self.layer - 1)) & 1) as usize;
        }
        index
    }

    /// Returns the most negative cell coordinate of the hypercube this node
    /// represents if centered at the origin.
    ///
    /// A quadtree with N layers stores a hypercube of length 2 ** N. But half
    /// of those cells are negative, so the actual coordinates span the same
    /// size as a signed two's complement integer with N bits.
    fn min_coordinate(&self) -> isize {
        0 - (1 << (self.layer - 1))
    }

    /// Returns the most positive cell coordinate of the hypercube this node
    /// represents if centered at the origin.
    ///
    /// A quadtree with N layers stores a hypercube of length 2 ** N. But half
    /// of those cells are negative, so the actual coordinates span the same
    /// size as a signed two's complement integer with N bits.
    fn max_coordinate(&self) -> isize {
        1 + (1 << (self.layer - 1))
    }

    fn contains(&self, pos: NdVec<D>) -> bool {
        for ax in D::axes() {
            if pos[ax] < self.min_coordinate() || pos[ax] > self.max_coordinate() {
                return false;
            }
        }
        true
    }

    /// Expand the current tree as necessary until it is wide enough to include the given position.
    fn expand_to(&self, pos: NdVec<D>) -> Self {
        // If this tree is already large enough, just return it.
        if self.contains(pos) {
            self.clone()
        } else {
            // Expand this tree by one layer.
            Self {
                layer: self.layer + 1,
                child: match &self.child {
                    // If we just have a leaf node, we can reuse the same leaf node.
                    NdTreeChild::Leaf(_) => self.child.clone(),
                    // If we have a branch node, then it's a little more complicated.
                    NdTreeChild::Branch(children) => {
                        // Each child needs to be placed in the opposite corner of a new node;
                        // for example, the southwest child will be placed in a new, larger
                        // southwest node that contains the old southwest node as the northeast
                        // child. Inverting each axis of a branch index can be accomplished by
                        // flipping all the bits of that index.
                        let mut new_children = Vec::with_capacity(Self::BRANCHES);
                        for (branch_index, self_branch_child) in children.into_iter().enumerate() {
                            let mut grandchildren = vec![
                                Rc::new(Self {
                                    layer: self.layer - 1,
                                    child: NdTreeChild::Leaf(T::default()),
                                    phantom: PhantomData,
                                });
                                Self::BRANCHES
                            ];
                            *grandchildren
                                .get_mut(branch_index ^ Self::BRANCH_IDX_MASK)
                                .unwrap() = self_branch_child.clone();
                            new_children.push(
                                Self {
                                    layer: self.layer,
                                    child: NdTreeChild::Branch(grandchildren),
                                    phantom: PhantomData,
                                }
                                .intern(),
                            )
                        }
                        NdTreeChild::Branch(new_children)
                    }
                },
                phantom: PhantomData,
            }
            .expand_to(pos)
        }
    }
    fn expand_child(self) -> NdTreeChild<T, D> {
        if self.layer == 0 {
            self.child
        } else {
            match self.child {
                NdTreeChild::Leaf(cell_value) => NdTreeChild::Branch(vec![
                    NdTreeNode {
                        layer: self.layer - 1,
                        child: NdTreeChild::Leaf(cell_value),
                        phantom: PhantomData
                    }
                    .intern();
                    Self::BRANCHES
                ]),
                NdTreeChild::Branch(_) => self.child,
            }
        }
    }
    fn get_cell(&self, pos: NdVec<D>) -> T {
        if self.contains(pos) {
            match &self.child {
                NdTreeChild::Leaf(cell) => *cell,
                NdTreeChild::Branch(children) => {
                    children[self.get_branch_index_top(pos)].get_cell_inner(pos)
                }
            }
        } else {
            T::default()
        }
    }
    fn get_cell_inner(&self, pos: NdVec<D>) -> T {
        match &self.child {
            NdTreeChild::Leaf(cell) => *cell,
            NdTreeChild::Branch(children) => {
                children[self.get_branch_index(pos)].get_cell_inner(pos)
            }
        }
    }
    fn set_cell(&self, pos: NdVec<D>, cell_value: T) -> Self {
        let ret = self.expand_to(pos);
        Self {
            layer: ret.layer,
            child: match ret.clone().expand_child() {
                NdTreeChild::Leaf(_) => NdTreeChild::Leaf(cell_value),
                NdTreeChild::Branch(children) => {
                    let mut new_children = children.clone();
                    let child_to_modify =
                        new_children.get_mut(ret.get_branch_index_top(pos)).unwrap();
                    *child_to_modify = child_to_modify.set_cell_inner(pos, cell_value).intern();
                    NdTreeChild::Branch(new_children)
                }
            },
            phantom: PhantomData,
        }
    }
    fn set_cell_inner(&self, pos: NdVec<D>, cell_value: T) -> Self {
        NdTreeNode {
            child: match self.clone().expand_child() {
                NdTreeChild::Leaf(_) => NdTreeChild::Leaf(cell_value),
                NdTreeChild::Branch(children) => {
                    let mut ret = children.clone();
                    let branch_index = self.get_branch_index(pos);
                    ret[branch_index] = ret[branch_index].set_cell_inner(pos, cell_value).intern();
                    NdTreeChild::Branch(ret)
                }
            },
            ..*self
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use std::collections::HashMap;

    proptest! {
        // #![proptest_config(ProptestConfig {
        //     max_shrink_iters: 4096
        // })]
        /// Tests setting and getting arbitrary grid cells by comparing against
        /// a HashMap.
        #[test]
        fn test_ndtree_set_get(
            cells_to_set: Vec<(Vec3D, u8)>,
            cells_to_get: Vec<Vec3D>
        ) {
            let mut ndtree = NdTree::new();
            let mut hashmap = HashMap::new();
            for (pos, state) in cells_to_set {
                hashmap.insert(pos, state);
                ndtree = ndtree.set_cell(pos, state);
            }
            println!("{:?}", ndtree);
            for pos in cells_to_get {
                assert_eq!(hashmap.get(&pos).unwrap_or(&0), &ndtree.get_cell(pos));
            }
        }
    }
}
