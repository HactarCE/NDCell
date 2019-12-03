use super::*;

/// Information describing how to slice an NdTree to get a 2D quadtree.
#[derive(Debug, Clone)]
pub struct NdProjectionInfo2D<D: Dim> {
    /// A vector determining where to slice the NdTree.
    pub slice_pos: NdVec<D>,
    /// The axis of the NdTree to be displayed horizontally.
    h: Axis,
    /// The axis of the NdTree to be displayed vertically.
    v: Axis,
}

impl<D: Dim> NdProjectionInfo<D> for NdProjectionInfo2D<D> {
    type PDim = Dim2D;
    fn get_slice_pos(&self) -> NdVec<D> {
        self.slice_pos
    }
    fn with_slice_pos(&self, slice_pos: NdVec<D>) -> Self {
        Self::new(slice_pos, self.h, self.v)
            .expect("Changing the slice position invalidated an NdProjectionInfo")
    }
    fn pdim_to_ndim(&self, pos: Vec2D) -> NdVec<D> {
        let mut ret = self.slice_pos;
        ret[self.h] = *pos.x();
        ret[self.v] = *pos.y();
        ret
    }
    fn ndim_to_pdim(&self, pos: NdVec<D>) -> Vec2D {
        [pos[self.h], pos[self.v]].into()
    }
}

impl<D: Dim> Default for NdProjectionInfo2D<D> {
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

impl<D: Dim> NdProjectionInfo2D<D> {
    /// Returns true if the given axes are different and within the
    /// dimensionality of this grid.
    fn check_axes(h: Axis, v: Axis) -> bool {
        h != v && (h as usize) < D::NDIM && (v as usize) < D::NDIM
    }
    /// Constructs a new projection with the given slice position and display
    /// axes. Returns Err(()) if the display axes are not valid.
    pub fn new(
        slice_pos: NdVec<D>,
        horizontal_display_axis: Axis,
        vertical_display_axis: Axis,
    ) -> Result<Self, ()> {
        let h = horizontal_display_axis;
        let v = vertical_display_axis;
        if Self::check_axes(h, v) {
            Ok(Self { slice_pos, h, v })
        } else {
            Err(())
        }
    }
    /// Returns a new identical projection but with the given axes displayed
    /// vertically and horizontally.
    pub fn with_display_axes(&self, horizontal: Axis, vertical: Axis) -> Result<Self, ()> {
        Self::new(self.slice_pos, horizontal, vertical)
    }
    /// Returns the axis displayed horizontally.
    pub fn get_horizontal_display_axis(&self) -> Axis {
        self.h
    }
    /// Returns the axis displayed vertically.
    pub fn get_vertical_display_axis(&self) -> Axis {
        self.v
    }
}
