use super::*;

#[derive(Debug, Clone)]
pub struct SliceProjection3D<D: Dim> {
    pub slice_pos: NdVec<D>,
    pub h: Axis,
    pub v: Axis,
    pub n: Axis,
}

impl<C: CellType, D: Dim> NdProjector<C, D, Dim3D> for SliceProjection3D<D> {
    fn project(&self, _tree: &NdTree<C, D>) -> NdTree<C, Dim3D> {
        unimplemented!()
    }
    fn overwrite_projected(&self, _destination: &mut NdTree<C, D>, _source: &NdTree<C, Dim3D>) {
        unimplemented!()
    }
    fn get_params(&self) -> ProjectionParams {
        ProjectionParams::Slice(
            self.slice_pos.into(),
            AxesSelectEnum::Axes3D {
                h: self.h,
                v: self.v,
                n: self.n,
            },
        )
    }
}
