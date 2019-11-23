use std::ops::Index;

use super::*;

/// An immutable view into an NdTree.
#[derive(Debug, Clone)]
pub struct NdTreeSlice<C: CellType, D: Dim> {
    /// The root NdTreeNode of this slice.
    pub root: NdCachedNode<C, D>,
    /// The position of the lower bound of the root node.
    pub offset: NdVec<D>,
}

/// A 1D grid represented as a bintree.
pub type NdTreeSlice1D<C> = NdTreeSlice<C, Dim1D>;
/// A 2D grid represented as a quadtree.
pub type NdTreeSlice2D<C> = NdTreeSlice<C, Dim2D>;
/// A 3D grid represented as an octree.
pub type NdTreeSlice3D<C> = NdTreeSlice<C, Dim3D>;
/// A 4D grid represented as a tree with nodes of degree 16.
pub type NdTreeSlice4D<C> = NdTreeSlice<C, Dim4D>;
/// A 5D grid represented as a tree with nodes of degree 32.
pub type NdTreeSlice5D<C> = NdTreeSlice<C, Dim5D>;
/// A 6D grid represented as a tree with nodes of degree 64.
pub type NdTreeSlice6D<C> = NdTreeSlice<C, Dim6D>;

impl fmt::Display for NdTreeSlice<bool, Dim2D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let rect = self.rect();
        write!(
            f,
            "{}",
            rect.axis_range(Axis::Y)
                .map(|y| rect
                    .axis_range(Axis::X)
                    .map(|x| if self[[x, y].into()] { "#" } else { "." })
                    .collect::<Vec<&str>>()
                    .join(" "))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl<C: CellType, D: Dim> Index<NdVec<D>> for NdTreeSlice<C, D> {
    type Output = C;
    fn index(&self, pos: NdVec<D>) -> &C {
        &self.root[pos - self.offset]
    }
}

impl<C: CellType, D: Dim> NdTreeSlice<C, D> {
    /// Constructs a new NdTreeSlice centered on a given node.
    pub fn centered(root: NdCachedNode<C, D>) -> Self {
        Self {
            offset: NdVec::origin() - root.len() as isize / 2,
            root,
        }
    }

    /// Returns the NdRect bounding this slice.
    pub fn rect(&self) -> NdRect<D> {
        self.root.rect() + self.offset
    }
    /// Returns the minimum position in this NdTree.
    pub fn min(&self) -> NdVec<D> {
        self.rect().min()
    }
    /// Returns the maximum position in this NdTree.
    pub fn max(&self) -> NdVec<D> {
        self.rect().max()
    }
    // /// Returns the number of non-default cells in this NdTree.
    // pub fn population(&mut self) -> usize {
    //     self.root.population
    // }

    /// Returns the cell at the given position, if it is within the bounds of the slice.
    pub fn get_cell(&self, pos: NdVec<D>) -> Option<C> {
        if self.rect().contains(pos) {
            Some(self.root.get_cell(pos - self.offset))
        } else {
            None
        }
    }

    // /// Returns an NdTreeSlice containing the root node's branch with the given
    // /// branch index.
    // pub fn get_branch_slice(&self, branch_idx: usize) -> Self {
    //     Self {
    //         root: match &self.root.branches[branch_idx] {
    //             NdTreeBranch::Leaf(_) => panic!("Cannot take slice of node at layer 1"),
    //             NdTreeBranch::Node(node) => node.clone(),
    //         },
    //         offset: self.offset + self.root.branch_offset(branch_idx),
    //     }
    // }
}
