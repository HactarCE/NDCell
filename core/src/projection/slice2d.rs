use super::*;

/// A projection that takes a 2D slice out of a D-dimensional automaton where D
/// >= 2.
#[derive(Debug, Clone)]
pub struct SliceProjection2D<D: Dim> {
    /// The position at which to slice the NdTree.
    pub slice_pos: BigVec<D>,
    /// The axis displayed horizontally.
    h: Axis,
    /// The axis displayed vertically.
    v: Axis,
}

impl<D: Dim> NdProjector<D, Dim2D> for SliceProjection2D<D> {
    fn project_tree(&self, _tree: &NdTree<D>) -> NdTree<Dim2D> {
        unimplemented!()
    }
    fn unproject_pos(&self, _pos: &BigVec<Dim2D>) -> BigVec<D> {
        unimplemented!()
    }
    fn overwrite_projected(&self, _destination: &mut NdTree<D>, _source: &NdTree<Dim2D>) {
        unimplemented!()
    }
    fn params(&self) -> ProjectionParams {
        ProjectionParams::Slice2D(self.slice_pos.clone().into(), (self.h, self.v))
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
    /// Constructs a new SliceProjection2D, panicking if the display axes are
    /// incompatible.
    pub fn new(slice_pos: BigVec<D>, h: Axis, v: Axis) -> Self {
        let mut ret = Self::default();
        ret.slice_pos = slice_pos;
        ret.set_display_axes(h, v);
        ret
    }

    /// Attempts to construct a new SliceProjection2D, returning `Err(())` if the
    /// display axes are incompatible.
    pub fn try_new(slice_pos: BigVec<D>, h: Axis, v: Axis) -> Result<Self, ()> {
        let mut ret = Self::default();
        ret.slice_pos = slice_pos;
        if let Ok(()) = ret.try_set_display_axes(h, v) {
            Ok(ret)
        } else {
            Err(())
        }
    }

    /// Returns the display axes as a tuple, `(horizontal, vertical)`.
    pub fn display_axes(&self) -> (Axis, Axis) {
        (self.h, self.v)
    }

    /// Sets the display axes, panicking if they are incompatible.
    pub fn set_display_axes(&mut self, h: Axis, v: Axis) {
        self.try_set_display_axes(h, v).unwrap_or_else(|_| {
            panic!(
                "Axes {:?} and {:?} are not compatible with SliceProjection2D",
                h, v,
            )
        })
    }

    /// Attempts to set the display axes, returning `Err(())` if they are
    /// incompatible.
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
