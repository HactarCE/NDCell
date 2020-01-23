use super::*;

/// A projection that takes a 3D slice out of a D-dimensional automaton where D
/// >= 3.
#[derive(Debug, Clone)]
pub struct SliceProjection3D<D: Dim> {
    /// The position at which to slice the NdTree.
    pub slice_pos: BigVec<D>,
    /// The axis displayed horizontally.
    h: Axis,
    /// The axis displayed vertically.
    v: Axis,
    /// The axis displayed along the "normal" (i.e. depthwise/front-back).
    n: Axis,
}

impl<C: CellType, D: Dim> NdProjector<C, D, Dim3D> for SliceProjection3D<D> {
    fn project(&self, _tree: &NdTree<C, D>) -> NdTree<C, Dim3D> {
        unimplemented!()
    }
    fn overwrite_projected(&self, _destination: &mut NdTree<C, D>, _source: &NdTree<C, Dim3D>) {
        unimplemented!()
    }
    fn get_params(&self) -> ProjectionParams {
        ProjectionParams::Slice3D(self.slice_pos.clone().into(), (self.h, self.v, self.n))
    }
}
