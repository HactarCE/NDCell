use super::*;

#[derive(Debug, Clone)]
pub struct SliceProjection2D<D: Dim> {
    pub slice_pos: NdVec<D>,
    h: Axis,
    v: Axis,
}

impl<C: CellType, D: Dim> NdProjector<C, D, Dim2D> for SliceProjection2D<D> {
    fn project(&self, _tree: &NdTree<C, D>) -> NdTree<C, Dim2D> {
        unimplemented!()
    }
    fn overwrite_projected(&self, destination: &mut NdTree<C, D>, source: &NdTree<C, Dim2D>) {
        unimplemented!()
    }
    fn get_params(&self) -> ProjectionParams {
        ProjectionParams::Slice(
            self.slice_pos.into(),
            AxesSelectEnum::Axes2D {
                h: self.h,
                v: self.v,
            },
        )
    }
}

impl<D: Dim> Default for SliceProjection2D<D> {
    fn default() -> Self {
        if D::NDIM < 2 {
            panic!("Cannot create 2D projection from less than 2 dimensions.");
        }
        Self {
            slice_pos: NdVec::default(),
            h: Axis::X,
            v: Axis::Y,
        }
    }
}

impl<D: Dim> SliceProjection2D<D> {
    pub fn new(slice_pos: NdVec<D>, h: Axis, v: Axis) -> Self {
        let mut ret = Self::default();
        ret.slice_pos = slice_pos;
        ret.set_display_axes(h, v);
        ret
    }

    pub fn try_new(slice_pos: NdVec<D>, h: Axis, v: Axis) -> Result<Self, ()> {
        let mut ret = Self::default();
        ret.slice_pos = slice_pos;
        if let Ok(()) = ret.try_set_display_axes(h, v) {
            Ok(ret)
        } else {
            Err(())
        }
    }

    pub fn get_display_axes(&self) -> (Axis, Axis) {
        (self.h, self.v)
    }

    pub fn set_display_axes(&mut self, h: Axis, v: Axis) {
        self.try_set_display_axes(h, v).unwrap_or_else(|_| {
            panic!(
                "Axes {:?} and {:?} are not compatible with SliceProjection2D",
                h, v,
            )
        })
    }

    pub fn try_set_display_axes(&mut self, h: Axis, v: Axis) -> Result<(), ()> {
        if D::contains(h) && D::contains(v) && h != v {
            self.h = h;
            self.v = v;
            Ok(())
        } else {
            Err(())
        }
    }
}
