use super::*;

/// A projection that takes a 3D slice out of a D-dimensional automaton where D
/// >= 3.
#[derive(Debug, Clone)]
pub struct SliceProjection3D<D: Dim> {
    /// The 3D node pool.
    projected_node_pool: SharedNodePool<Dim3D>,
    /// The position at which to slice the NdTree.
    pub slice_pos: BigVec<D>,
    /// The axis displayed horizontally.
    h: Axis,
    /// The axis displayed vertically.
    v: Axis,
    /// The axis displayed along the "normal" (i.e. depthwise/front-back).
    n: Axis,
}

impl<D: Dim> NdProjector<D, Dim3D> for SliceProjection3D<D> {
    fn projected_node_pool<'a>(&'a self, _tree: &'a NdTree<D>) -> &'a SharedNodePool<Dim3D> {
        &self.projected_node_pool
    }
    fn project_tree(&self, _tree: &NdTree<D>) -> NdTree<Dim3D> {
        unimplemented!()
    }
    fn unproject_pos(&self, _pos: &BigVec<Dim3D>) -> BigVec<D> {
        unimplemented!()
    }
    fn overwrite_projected(&self, _destination: &mut NdTree<D>, _source: &NdTree<Dim3D>) {
        unimplemented!()
    }
    fn params(&self) -> ProjectionParams {
        ProjectionParams::Slice3D(self.slice_pos.clone().into(), (self.h, self.v, self.n))
    }
}
