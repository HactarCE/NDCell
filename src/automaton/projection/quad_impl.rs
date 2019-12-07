use super::*;

// The enum_delegate crate would be perfect for this, except for two issues:
//
//  1. enum_delegate currently doesn't support implementing multiple traits on
//     one enum.
//  2. I can't get enum_delegate to work anyway.
//
// In the future I should definitely write a custom macro to handle this,
// because this is just ridiculous the way it is right now.

impl<C: CellType> From<NdAutomaton<C, Dim1D, NdProjectionInfo2D<Dim1D>>> for QuadTreeAutomaton<C> {
    fn from(automaton: NdAutomaton<C, Dim1D, NdProjectionInfo2D<Dim1D>>) -> Self {
        Self::Automaton1D(automaton)
    }
}
impl<C: CellType> From<NdAutomaton<C, Dim2D, NdProjectionInfo2D<Dim2D>>> for QuadTreeAutomaton<C> {
    fn from(automaton: NdAutomaton<C, Dim2D, NdProjectionInfo2D<Dim2D>>) -> Self {
        Self::Automaton2D(automaton)
    }
}
impl<C: CellType> From<NdAutomaton<C, Dim3D, NdProjectionInfo2D<Dim3D>>> for QuadTreeAutomaton<C> {
    fn from(automaton: NdAutomaton<C, Dim3D, NdProjectionInfo2D<Dim3D>>) -> Self {
        Self::Automaton3D(automaton)
    }
}
impl<C: CellType> From<NdAutomaton<C, Dim4D, NdProjectionInfo2D<Dim4D>>> for QuadTreeAutomaton<C> {
    fn from(automaton: NdAutomaton<C, Dim4D, NdProjectionInfo2D<Dim4D>>) -> Self {
        Self::Automaton4D(automaton)
    }
}
impl<C: CellType> From<NdAutomaton<C, Dim5D, NdProjectionInfo2D<Dim5D>>> for QuadTreeAutomaton<C> {
    fn from(automaton: NdAutomaton<C, Dim5D, NdProjectionInfo2D<Dim5D>>) -> Self {
        Self::Automaton5D(automaton)
    }
}
impl<C: CellType> From<NdAutomaton<C, Dim6D, NdProjectionInfo2D<Dim6D>>> for QuadTreeAutomaton<C> {
    fn from(automaton: NdAutomaton<C, Dim6D, NdProjectionInfo2D<Dim6D>>) -> Self {
        Self::Automaton6D(automaton)
    }
}

impl<C: CellType> QuadTreeAutomatonTrait<C> for QuadTreeAutomaton<C> {
    fn slice(&self) -> QuadTreeSlice<C> {
        match self {
            Self::Automaton1D(inner) => inner.slice(),
            Self::Automaton2D(inner) => inner.slice(),
            Self::Automaton3D(inner) => inner.slice(),
            Self::Automaton4D(inner) => inner.slice(),
            Self::Automaton5D(inner) => inner.slice(),
            Self::Automaton6D(inner) => inner.slice(),
        }
    }
    fn get_root(&self) -> QuadTreeNode<C> {
        match self {
            Self::Automaton1D(inner) => inner.get_root(),
            Self::Automaton2D(inner) => inner.get_root(),
            Self::Automaton3D(inner) => inner.get_root(),
            Self::Automaton4D(inner) => inner.get_root(),
            Self::Automaton5D(inner) => inner.get_root(),
            Self::Automaton6D(inner) => inner.get_root(),
        }
    }
    fn get_cell(&self, pos: Vec2D) -> C {
        match self {
            Self::Automaton1D(inner) => inner.get_cell(pos),
            Self::Automaton2D(inner) => inner.get_cell(pos),
            Self::Automaton3D(inner) => inner.get_cell(pos),
            Self::Automaton4D(inner) => inner.get_cell(pos),
            Self::Automaton5D(inner) => inner.get_cell(pos),
            Self::Automaton6D(inner) => inner.get_cell(pos),
        }
    }
    fn set_view_pos_on_axis(&mut self, axis: Axis, pos: isize) {
        match self {
            Self::Automaton1D(inner) => inner.set_view_pos_on_axis(axis, pos),
            Self::Automaton2D(inner) => inner.set_view_pos_on_axis(axis, pos),
            Self::Automaton3D(inner) => inner.set_view_pos_on_axis(axis, pos),
            Self::Automaton4D(inner) => inner.set_view_pos_on_axis(axis, pos),
            Self::Automaton5D(inner) => inner.set_view_pos_on_axis(axis, pos),
            Self::Automaton6D(inner) => inner.set_view_pos_on_axis(axis, pos),
        }
    }
    fn set_display_axes(&mut self, horizontal: Axis, vertical: Axis) -> Result<(), ()> {
        match self {
            Self::Automaton1D(inner) => inner.set_display_axes(horizontal, vertical),
            Self::Automaton2D(inner) => inner.set_display_axes(horizontal, vertical),
            Self::Automaton3D(inner) => inner.set_display_axes(horizontal, vertical),
            Self::Automaton4D(inner) => inner.set_display_axes(horizontal, vertical),
            Self::Automaton5D(inner) => inner.set_display_axes(horizontal, vertical),
            Self::Automaton6D(inner) => inner.set_display_axes(horizontal, vertical),
        }
    }
    fn set_cell(&mut self, pos: Vec2D, new_state: C) {
        match self {
            Self::Automaton1D(inner) => inner.set_cell(pos, new_state),
            Self::Automaton2D(inner) => inner.set_cell(pos, new_state),
            Self::Automaton3D(inner) => inner.set_cell(pos, new_state),
            Self::Automaton4D(inner) => inner.set_cell(pos, new_state),
            Self::Automaton5D(inner) => inner.set_cell(pos, new_state),
            Self::Automaton6D(inner) => inner.set_cell(pos, new_state),
        }
    }
    fn get_slice_containing(&mut self, rect: Rect2D) -> QuadTreeSlice<C> {
        match self {
            Self::Automaton1D(inner) => inner.get_slice_containing(rect),
            Self::Automaton2D(inner) => inner.get_slice_containing(rect),
            Self::Automaton3D(inner) => inner.get_slice_containing(rect),
            Self::Automaton4D(inner) => inner.get_slice_containing(rect),
            Self::Automaton5D(inner) => inner.get_slice_containing(rect),
            Self::Automaton6D(inner) => inner.get_slice_containing(rect),
        }
    }
    fn expand_to(&mut self, pos: Vec2D) {
        match self {
            Self::Automaton1D(inner) => inner.expand_to(pos),
            Self::Automaton2D(inner) => inner.expand_to(pos),
            Self::Automaton3D(inner) => inner.expand_to(pos),
            Self::Automaton4D(inner) => inner.expand_to(pos),
            Self::Automaton5D(inner) => inner.expand_to(pos),
            Self::Automaton6D(inner) => inner.expand_to(pos),
        }
    }
    fn shrink(&mut self) {
        match self {
            Self::Automaton1D(inner) => inner.shrink(),
            Self::Automaton2D(inner) => inner.shrink(),
            Self::Automaton3D(inner) => inner.shrink(),
            Self::Automaton4D(inner) => inner.shrink(),
            Self::Automaton5D(inner) => inner.shrink(),
            Self::Automaton6D(inner) => inner.shrink(),
        }
    }
}

impl<C: CellType> NdSimulate for QuadTreeAutomaton<C> {
    fn get_ndim(&self) -> usize {
        match self {
            Self::Automaton1D(inner) => inner.get_ndim(),
            Self::Automaton2D(inner) => inner.get_ndim(),
            Self::Automaton3D(inner) => inner.get_ndim(),
            Self::Automaton4D(inner) => inner.get_ndim(),
            Self::Automaton5D(inner) => inner.get_ndim(),
            Self::Automaton6D(inner) => inner.get_ndim(),
        }
    }
    fn step(&mut self) {
        match self {
            Self::Automaton1D(inner) => inner.step(),
            Self::Automaton2D(inner) => inner.step(),
            Self::Automaton3D(inner) => inner.step(),
            Self::Automaton4D(inner) => inner.step(),
            Self::Automaton5D(inner) => inner.step(),
            Self::Automaton6D(inner) => inner.step(),
        }
    }
    fn step_single(&mut self) {
        match self {
            Self::Automaton1D(inner) => inner.step_single(),
            Self::Automaton2D(inner) => inner.step_single(),
            Self::Automaton3D(inner) => inner.step_single(),
            Self::Automaton4D(inner) => inner.step_single(),
            Self::Automaton5D(inner) => inner.step_single(),
            Self::Automaton6D(inner) => inner.step_single(),
        }
    }
    fn push_to_undo_history(&mut self) {
        match self {
            Self::Automaton1D(inner) => inner.push_to_undo_history(),
            Self::Automaton2D(inner) => inner.push_to_undo_history(),
            Self::Automaton3D(inner) => inner.push_to_undo_history(),
            Self::Automaton4D(inner) => inner.push_to_undo_history(),
            Self::Automaton5D(inner) => inner.push_to_undo_history(),
            Self::Automaton6D(inner) => inner.push_to_undo_history(),
        }
    }
    fn push_to_redo_history(&mut self) {
        match self {
            Self::Automaton1D(inner) => inner.push_to_redo_history(),
            Self::Automaton2D(inner) => inner.push_to_redo_history(),
            Self::Automaton3D(inner) => inner.push_to_redo_history(),
            Self::Automaton4D(inner) => inner.push_to_redo_history(),
            Self::Automaton5D(inner) => inner.push_to_redo_history(),
            Self::Automaton6D(inner) => inner.push_to_redo_history(),
        }
    }
    fn get_step_size(&self) -> usize {
        match self {
            Self::Automaton1D(inner) => inner.get_step_size(),
            Self::Automaton2D(inner) => inner.get_step_size(),
            Self::Automaton3D(inner) => inner.get_step_size(),
            Self::Automaton4D(inner) => inner.get_step_size(),
            Self::Automaton5D(inner) => inner.get_step_size(),
            Self::Automaton6D(inner) => inner.get_step_size(),
        }
    }
    fn set_step_size(&mut self, new_step_size: usize) {
        match self {
            Self::Automaton1D(inner) => inner.set_step_size(new_step_size),
            Self::Automaton2D(inner) => inner.set_step_size(new_step_size),
            Self::Automaton3D(inner) => inner.set_step_size(new_step_size),
            Self::Automaton4D(inner) => inner.set_step_size(new_step_size),
            Self::Automaton5D(inner) => inner.set_step_size(new_step_size),
            Self::Automaton6D(inner) => inner.set_step_size(new_step_size),
        }
    }
    fn has_undo(&self) -> bool {
        match self {
            Self::Automaton1D(inner) => inner.has_undo(),
            Self::Automaton2D(inner) => inner.has_undo(),
            Self::Automaton3D(inner) => inner.has_undo(),
            Self::Automaton4D(inner) => inner.has_undo(),
            Self::Automaton5D(inner) => inner.has_undo(),
            Self::Automaton6D(inner) => inner.has_undo(),
        }
    }
    fn has_redo(&self) -> bool {
        match self {
            Self::Automaton1D(inner) => inner.has_redo(),
            Self::Automaton2D(inner) => inner.has_redo(),
            Self::Automaton3D(inner) => inner.has_redo(),
            Self::Automaton4D(inner) => inner.has_redo(),
            Self::Automaton5D(inner) => inner.has_redo(),
            Self::Automaton6D(inner) => inner.has_redo(),
        }
    }
    fn undo(&mut self) -> bool {
        match self {
            Self::Automaton1D(inner) => inner.undo(),
            Self::Automaton2D(inner) => inner.undo(),
            Self::Automaton3D(inner) => inner.undo(),
            Self::Automaton4D(inner) => inner.undo(),
            Self::Automaton5D(inner) => inner.undo(),
            Self::Automaton6D(inner) => inner.undo(),
        }
    }
    fn redo(&mut self) -> bool {
        match self {
            Self::Automaton1D(inner) => inner.redo(),
            Self::Automaton2D(inner) => inner.redo(),
            Self::Automaton3D(inner) => inner.redo(),
            Self::Automaton4D(inner) => inner.redo(),
            Self::Automaton5D(inner) => inner.redo(),
            Self::Automaton6D(inner) => inner.redo(),
        }
    }
    fn reset(&mut self) -> bool {
        match self {
            Self::Automaton1D(inner) => inner.reset(),
            Self::Automaton2D(inner) => inner.reset(),
            Self::Automaton3D(inner) => inner.reset(),
            Self::Automaton4D(inner) => inner.reset(),
            Self::Automaton5D(inner) => inner.reset(),
            Self::Automaton6D(inner) => inner.reset(),
        }
    }
    fn get_generation_count(&self) -> usize {
        match self {
            Self::Automaton1D(inner) => inner.get_generation_count(),
            Self::Automaton2D(inner) => inner.get_generation_count(),
            Self::Automaton3D(inner) => inner.get_generation_count(),
            Self::Automaton4D(inner) => inner.get_generation_count(),
            Self::Automaton5D(inner) => inner.get_generation_count(),
            Self::Automaton6D(inner) => inner.get_generation_count(),
        }
    }
    fn set_generation_count(&mut self, new_generation_count: usize) {
        match self {
            Self::Automaton1D(inner) => inner.set_generation_count(new_generation_count),
            Self::Automaton2D(inner) => inner.set_generation_count(new_generation_count),
            Self::Automaton3D(inner) => inner.set_generation_count(new_generation_count),
            Self::Automaton4D(inner) => inner.set_generation_count(new_generation_count),
            Self::Automaton5D(inner) => inner.set_generation_count(new_generation_count),
            Self::Automaton6D(inner) => inner.set_generation_count(new_generation_count),
        }
    }
}

impl<C: CellType> From<NdProjectedTreeSlice<C, Dim1D, NdProjectionInfo2D<Dim1D>>>
    for QuadTreeSlice<C>
{
    fn from(tree_slice: NdProjectedTreeSlice<C, Dim1D, NdProjectionInfo2D<Dim1D>>) -> Self {
        Self::Slice1D(tree_slice)
    }
}
impl<C: CellType> From<NdProjectedTreeSlice<C, Dim2D, NdProjectionInfo2D<Dim2D>>>
    for QuadTreeSlice<C>
{
    fn from(tree_slice: NdProjectedTreeSlice<C, Dim2D, NdProjectionInfo2D<Dim2D>>) -> Self {
        Self::Slice2D(tree_slice)
    }
}
impl<C: CellType> From<NdProjectedTreeSlice<C, Dim3D, NdProjectionInfo2D<Dim3D>>>
    for QuadTreeSlice<C>
{
    fn from(tree_slice: NdProjectedTreeSlice<C, Dim3D, NdProjectionInfo2D<Dim3D>>) -> Self {
        Self::Slice3D(tree_slice)
    }
}
impl<C: CellType> From<NdProjectedTreeSlice<C, Dim4D, NdProjectionInfo2D<Dim4D>>>
    for QuadTreeSlice<C>
{
    fn from(tree_slice: NdProjectedTreeSlice<C, Dim4D, NdProjectionInfo2D<Dim4D>>) -> Self {
        Self::Slice4D(tree_slice)
    }
}
impl<C: CellType> From<NdProjectedTreeSlice<C, Dim5D, NdProjectionInfo2D<Dim5D>>>
    for QuadTreeSlice<C>
{
    fn from(tree_slice: NdProjectedTreeSlice<C, Dim5D, NdProjectionInfo2D<Dim5D>>) -> Self {
        Self::Slice5D(tree_slice)
    }
}
impl<C: CellType> From<NdProjectedTreeSlice<C, Dim6D, NdProjectionInfo2D<Dim6D>>>
    for QuadTreeSlice<C>
{
    fn from(tree_slice: NdProjectedTreeSlice<C, Dim6D, NdProjectionInfo2D<Dim6D>>) -> Self {
        Self::Slice6D(tree_slice)
    }
}

impl<C: CellType> QuadTreeSliceTrait<C> for QuadTreeSlice<C> {
    fn get_root(&self) -> QuadTreeNode<C> {
        match self {
            Self::Slice1D(inner) => inner.get_root(),
            Self::Slice2D(inner) => inner.get_root(),
            Self::Slice3D(inner) => inner.get_root(),
            Self::Slice4D(inner) => inner.get_root(),
            Self::Slice5D(inner) => inner.get_root(),
            Self::Slice6D(inner) => inner.get_root(),
        }
    }
    fn get_cell(&self, pos: Vec2D) -> Option<C> {
        match self {
            Self::Slice1D(inner) => inner.get_cell(pos),
            Self::Slice2D(inner) => inner.get_cell(pos),
            Self::Slice3D(inner) => inner.get_cell(pos),
            Self::Slice4D(inner) => inner.get_cell(pos),
            Self::Slice5D(inner) => inner.get_cell(pos),
            Self::Slice6D(inner) => inner.get_cell(pos),
        }
    }
    fn get_rect(&self) -> Rect2D {
        match self {
            Self::Slice1D(inner) => inner.get_rect(),
            Self::Slice2D(inner) => inner.get_rect(),
            Self::Slice3D(inner) => inner.get_rect(),
            Self::Slice4D(inner) => inner.get_rect(),
            Self::Slice5D(inner) => inner.get_rect(),
            Self::Slice6D(inner) => inner.get_rect(),
        }
    }
    fn get_branch(&self, branch_idx: usize) -> QuadTreeSliceBranch<C> {
        match self {
            Self::Slice1D(inner) => inner.get_branch(branch_idx),
            Self::Slice2D(inner) => inner.get_branch(branch_idx),
            Self::Slice3D(inner) => inner.get_branch(branch_idx),
            Self::Slice4D(inner) => inner.get_branch(branch_idx),
            Self::Slice5D(inner) => inner.get_branch(branch_idx),
            Self::Slice6D(inner) => inner.get_branch(branch_idx),
        }
    }
    fn get_branches(&self) -> [QuadTreeSliceBranch<C>; 4] {
        match self {
            Self::Slice1D(inner) => inner.get_branches(),
            Self::Slice2D(inner) => inner.get_branches(),
            Self::Slice3D(inner) => inner.get_branches(),
            Self::Slice4D(inner) => inner.get_branches(),
            Self::Slice5D(inner) => inner.get_branches(),
            Self::Slice6D(inner) => inner.get_branches(),
        }
    }
}

impl<C: CellType> From<NdProjectedTreeNode<C, Dim1D, NdProjectionInfo2D<Dim1D>>>
    for QuadTreeNode<C>
{
    fn from(tree_slice: NdProjectedTreeNode<C, Dim1D, NdProjectionInfo2D<Dim1D>>) -> Self {
        Self::Node1D(tree_slice)
    }
}
impl<C: CellType> From<NdProjectedTreeNode<C, Dim2D, NdProjectionInfo2D<Dim2D>>>
    for QuadTreeNode<C>
{
    fn from(tree_slice: NdProjectedTreeNode<C, Dim2D, NdProjectionInfo2D<Dim2D>>) -> Self {
        Self::Node2D(tree_slice)
    }
}
impl<C: CellType> From<NdProjectedTreeNode<C, Dim3D, NdProjectionInfo2D<Dim3D>>>
    for QuadTreeNode<C>
{
    fn from(tree_slice: NdProjectedTreeNode<C, Dim3D, NdProjectionInfo2D<Dim3D>>) -> Self {
        Self::Node3D(tree_slice)
    }
}
impl<C: CellType> From<NdProjectedTreeNode<C, Dim4D, NdProjectionInfo2D<Dim4D>>>
    for QuadTreeNode<C>
{
    fn from(tree_slice: NdProjectedTreeNode<C, Dim4D, NdProjectionInfo2D<Dim4D>>) -> Self {
        Self::Node4D(tree_slice)
    }
}
impl<C: CellType> From<NdProjectedTreeNode<C, Dim5D, NdProjectionInfo2D<Dim5D>>>
    for QuadTreeNode<C>
{
    fn from(tree_slice: NdProjectedTreeNode<C, Dim5D, NdProjectionInfo2D<Dim5D>>) -> Self {
        Self::Node5D(tree_slice)
    }
}
impl<C: CellType> From<NdProjectedTreeNode<C, Dim6D, NdProjectionInfo2D<Dim6D>>>
    for QuadTreeNode<C>
{
    fn from(tree_slice: NdProjectedTreeNode<C, Dim6D, NdProjectionInfo2D<Dim6D>>) -> Self {
        Self::Node6D(tree_slice)
    }
}

impl<C: CellType> QuadTreeNodeTrait<C> for QuadTreeNode<C> {
    fn get_cell(&self, pos: Vec2D) -> C {
        match self {
            Self::Node1D(inner) => inner.get_cell(pos),
            Self::Node2D(inner) => inner.get_cell(pos),
            Self::Node3D(inner) => inner.get_cell(pos),
            Self::Node4D(inner) => inner.get_cell(pos),
            Self::Node5D(inner) => inner.get_cell(pos),
            Self::Node6D(inner) => inner.get_cell(pos),
        }
    }
    fn get_layer(&self) -> usize {
        match self {
            Self::Node1D(inner) => inner.get_layer(),
            Self::Node2D(inner) => inner.get_layer(),
            Self::Node3D(inner) => inner.get_layer(),
            Self::Node4D(inner) => inner.get_layer(),
            Self::Node5D(inner) => inner.get_layer(),
            Self::Node6D(inner) => inner.get_layer(),
        }
    }
    fn get_branch(&self, branch_idx: usize) -> QuadTreeBranch<C> {
        match self {
            Self::Node1D(inner) => inner.get_branch(branch_idx),
            Self::Node2D(inner) => inner.get_branch(branch_idx),
            Self::Node3D(inner) => inner.get_branch(branch_idx),
            Self::Node4D(inner) => inner.get_branch(branch_idx),
            Self::Node5D(inner) => inner.get_branch(branch_idx),
            Self::Node6D(inner) => inner.get_branch(branch_idx),
        }
    }
    fn get_branches(&self) -> [QuadTreeBranch<C>; 4] {
        match self {
            Self::Node1D(inner) => inner.get_branches(),
            Self::Node2D(inner) => inner.get_branches(),
            Self::Node3D(inner) => inner.get_branches(),
            Self::Node4D(inner) => inner.get_branches(),
            Self::Node5D(inner) => inner.get_branches(),
            Self::Node6D(inner) => inner.get_branches(),
        }
    }
    fn get_population(&self) -> usize {
        match self {
            Self::Node1D(inner) => inner.get_population(),
            Self::Node2D(inner) => inner.get_population(),
            Self::Node3D(inner) => inner.get_population(),
            Self::Node4D(inner) => inner.get_population(),
            Self::Node5D(inner) => inner.get_population(),
            Self::Node6D(inner) => inner.get_population(),
        }
    }
}
