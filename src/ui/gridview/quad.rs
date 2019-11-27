use super::*;

/// Anything that can act as a mutable quadtree of cells.
pub trait QuadTree<C: CellType>: QuadTreeSlice<C> + NdSimulate {
    fn set_view_pos_on_axis(&mut self, axis: Axis, pos: isize);
    fn set_h_axis(&mut self, axis: Axis);
    fn set_v_axis(&mut self, axis: Axis);
    fn set_cell(&mut self, pos: Vec2D, new_state: C);
}

/// Anything that can act as an immutable quadtree of cells.
pub trait QuadTreeSlice<C: CellType> {
    type Node: QuadTreeNode<C>;
    fn get_root(&self) -> Self::Node;
    fn get_cell(&self, pos: Vec2D) -> Option<C>;
    fn get_population(&self) -> usize {
        self.get_root().get_population()
    }
}

/// Anything that can act as an immutable node in a quadtree of cells.
pub trait QuadTreeNode<C: CellType>
where
    Self: std::marker::Sized,
{
    fn get_cell(&self, pos: Vec2D) -> C;
    fn get_branch(&self, branch_idx: usize) -> QuadTreeBranch<C, Self>;
    fn get_branches(&self) -> [QuadTreeBranch<C, Self>; 4];
    fn get_population(&self) -> usize;
}

/// Anything that can act as a branch of a node in a quadtree of cells.
pub enum QuadTreeBranch<C: CellType, Node: QuadTreeNode<C>> {
    Leaf(C),
    Node(Box<Node>),
}

/// Information describing how to slice an NdTree to get a 2D quadtree.
#[derive(Debug, Copy, Clone)]
struct NdTreeViewMeta2D<D: Dim> {
    /// A vector determining where to slice the NdTree.
    pub slice_pos: NdVec<D>,
    /// The axis of the NdTree to be displayed horizontally.
    h: Axis,
    /// The axis of the NdTree to be displayed vertically.
    v: Axis,
}
impl<D: Dim> NdTreeViewMeta<D> for NdTreeViewMeta2D<D> {
    fn get_slice_pos(&self) -> NdVec<D> {
        self.slice_pos
    }
    fn set_slice_pos(&mut self, slice_pos: NdVec<D>) {
        self.slice_pos = slice_pos
    }
}
impl<D: Dim> Default for NdTreeViewMeta2D<D> {
    fn default() -> Self {
        if D::NDIM < 2 {
            panic!("");
        }
        Self {
            slice_pos: NdVec::origin(),
            h: Axis::X,
            v: Axis::Y,
        }
    }
}
impl<D: Dim> NdTreeViewMeta2D<D> {
    /// Returns true if the given axes are different and within the
    /// dimensionality of this grid.
    fn check_axes(h: Axis, v: Axis) -> bool {
        h != v && (h as usize) < D::NDIM && (v as usize) < D::NDIM
    }
    /// Sets the axes displayed vertically and horizontally.
    pub fn set_display_axes(&mut self, horizontal: Axis, vertical: Axis) -> Result<(), ()> {
        if Self::check_axes(horizontal, vertical) {
            self.h = horizontal;
            self.v = vertical;
            Ok(())
        } else {
            Err(())
        }
    }
    /// Returns the axis displayed horizontally.
    pub fn get_horizontal_display_axis(&self) -> Axis {
        self.h
    }
    /// Returns the axis displayed vertically.
    pub fn get_vertical_display_axis(&self) -> Axis {
        self.v
    }
    // Fills out the extra dimensions of the position using slice_pos.
    pub fn get_ndim_pos(&self, pos: Vec2D) -> NdVec<D> {
        let mut ret = self.slice_pos;
        ret[self.h] = pos[Axis::X];
        ret[self.v] = pos[Axis::Y];
        ret
    }
}

impl<C: CellType, D: Dim> QuadTree<C> for NdTreeView<C, D, NdTreeViewMeta2D<D>> {
    fn set_view_pos_on_axis(&mut self, axis: Axis, pos: isize) {
        self.meta.slice_pos[axis] = pos;
    }
    fn set_h_axis(&mut self, axis: Axis) {
        self.meta.h = axis;
    }
    fn set_v_axis(&mut self, axis: Axis) {
        self.meta.v = axis;
    }
    fn set_cell(&mut self, pos: Vec2D, new_state: C) {
        self.tree.set_cell(self.meta.get_ndim_pos(pos), new_state);
    }
}

impl<C: CellType, D: Dim> QuadTreeSlice<C> for NdTreeView<C, D, NdTreeViewMeta2D<D>> {
    type Node = NdTreeNodeView<C, D, NdTreeViewMeta2D<D>>;
    fn get_root(&self) -> Self::Node {
        self.slice().get_root()
    }
    fn get_cell(&self, pos: Vec2D) -> Option<C> {
        self.slice().get_cell(pos)
    }
}

impl<C: CellType, D: Dim> QuadTreeSlice<C> for NdTreeSliceView<C, D, NdTreeViewMeta2D<D>> {
    type Node = NdTreeNodeView<C, D, NdTreeViewMeta2D<D>>;
    fn get_root(&self) -> Self::Node {
        NdTreeNodeView {
            node: self.slice.root.clone(),
            meta: self.meta,
        }
    }
    fn get_cell(&self, pos: Vec2D) -> Option<C> {
        self.slice.get_cell(self.meta.get_ndim_pos(pos))
    }
}

impl<C: CellType, D: Dim> QuadTreeNode<C> for NdTreeNodeView<C, D, NdTreeViewMeta2D<D>> {
    fn get_cell(&self, pos: Vec2D) -> C {
        self.node.get_cell(self.meta.get_ndim_pos(pos))
    }
    fn get_branch(&self, branch_idx: usize) -> QuadTreeBranch<C, Self> {
        // Get the index of the branch containing the slice position.
        let mut nd_branch_idx = self.node.branch_idx(self.meta.slice_pos);
        // For the horizontal and vertical axes, set or reset the proper bit in
        // the slice branch index.
        let h_bit = self.meta.h.branch_bit();
        nd_branch_idx &= !h_bit;
        if branch_idx & 1 != 0 {
            nd_branch_idx |= h_bit;
        }
        let v_bit = self.meta.h.branch_bit();
        nd_branch_idx &= !v_bit;
        if branch_idx & 2 != 0 {
            nd_branch_idx |= v_bit;
        }
        // Now that we have the real N-dimensional branch index, convert the NdTreeBranch into a QuadTreeBranch.
        match &self.node.branches[nd_branch_idx] {
            NdTreeBranch::Leaf(cell_state) => QuadTreeBranch::Leaf(*cell_state),
            NdTreeBranch::Node(node) => QuadTreeBranch::Node(Box::new(NdTreeNodeView {
                node: node.clone(),
                meta: self.meta,
            })),
        }
    }
    fn get_branches(&self) -> [QuadTreeBranch<C, Self>; 4] {
        [
            self.get_branch(0),
            self.get_branch(1),
            self.get_branch(2),
            self.get_branch(3),
        ]
    }
    fn get_population(&self) -> usize {
        self.node.population
    }
}
