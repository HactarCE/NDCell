use super::*;

/// A description of how a D-dimensional grid is displayed as a P-dimensional
/// projection.
pub trait NdProjectionInfo<D: Dim>: Default + Copy + Clone {
    /// The dimensionality of the result of the projection. For example, a 4D
    /// grid cannot be displayed directly; only a 2D or 3D slice of it can be
    /// displayed at a time. In the case of a displaying a 4D grid as a 3D
    /// slice, `D = Dim4D` and `PDim = Dim3D`.
    type PDim: Dim;
    /// Returns the slice position of the projection.
    ///
    /// The slice position is the N-dimensional point in the automaton that is
    /// always visible regardless of which axes are being displayed. For
    /// example, when viewing a 2D slice along the X and Y axes of a 3D
    /// automaton, the slice position would determine the Z coordinate of the 2D
    /// slice being displayed.
    fn get_slice_pos(&self) -> NdVec<D>;
    /// Sets the slice position of the projection.
    fn set_slice_pos(&mut self, new_slice_pos: NdVec<D>);
    // Given a cell position in the projection, returns the corresponding cell
    // position in the original grid.
    fn pdim_to_ndim(&self, pos: NdVec<Self::PDim>) -> NdVec<D>;
    /// Given a cell position in the original grid, returns the corresponding
    /// cell position in the projection.
    fn ndim_to_pdim(&self, pos: NdVec<D>) -> NdVec<Self::PDim>;
}
