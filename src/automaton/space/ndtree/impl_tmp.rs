impl<T: CellType, D: Dim> NdTree<T, D> {
    /// Creates a new empty NdTree.
    pub fn new(&self) -> Self {
        Self(NdTreeNode::<T, D>::empty(self))
    }

    /// Returns the cell at the given position (or the default background state,
    /// if the position is out of bounds).
    pub fn get_cell(&self, pos: NdVec<D>) -> T {
        self.0.get_cell(pos)
    }

    /// Returns a new NdTree with the cell at the given position set to the
    /// given value.
    pub fn set_cell(&self, pos: NdVec<D>, cell_value: T) -> Self {
        self.0.set_cell(pos, cell_value)
    }
}
