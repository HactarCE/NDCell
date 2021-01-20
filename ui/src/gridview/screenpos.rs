use ndcell_core::prelude::*;

use super::algorithms::raycast;
use super::viewpoint::{CellTransform3D, Viewpoint3D};
use crate::commands::DrawMode;
use crate::Face;

/// Convenient representation of a pixel position on the screen.
pub trait ScreenPos {
    type D: Dim;

    /// Returns the original pixel location.
    fn pixel(&self) -> FVec2D;
    /// Returns the render cell layer.
    fn layer(&self) -> Layer;
    /// Returns the cell to draw on, which is automatically `None` if zoomed out
    /// too far to draw.
    fn draw_cell(&self, mode: DrawMode) -> Option<BigVec<Self::D>>;
}

#[derive(Debug, Clone)]
pub struct ScreenPos2D {
    pub(super) pixel: FVec2D,
    pub(super) layer: Layer,

    /// Global cell position.
    pub pos: FixedVec2D,
}
impl ScreenPos for ScreenPos2D {
    type D = Dim2D;

    fn pixel(&self) -> FVec2D {
        self.pixel
    }
    fn layer(&self) -> Layer {
        self.layer
    }
    fn draw_cell(&self, _mode: DrawMode) -> Option<BigVec<Self::D>> {
        if self.layer == Layer(0) {
            Some(self.pos.floor())
        } else {
            None
        }
    }
}
impl ScreenPos2D {
    /// Returns the global cell position of the mouse.
    pub fn pos(&self) -> FixedVec2D {
        self.pos.clone()
    }
    /// Returns the global cell coordinates at the pixel.
    pub fn cell(&self) -> BigVec2D {
        self.pos.floor()
    }
    /// Returns the global cell rectangle of cells inside the pixel.
    pub fn rect(&self) -> BigRect2D {
        let render_cell_len = self.layer.big_len();
        // Round to render cell.
        let base_pos = self.cell().div_floor(&render_cell_len) * render_cell_len;
        self.layer.big_rect() + base_pos
    }
}

#[derive(Debug, Clone)]
pub struct ScreenPos3D {
    pub(super) pixel: FVec2D,
    pub(super) layer: Layer,
    pub(super) xform: CellTransform3D,

    /// Nearest raycast hit.
    pub raycast: Option<RaycastHit>,
    /// Ray intersection with nearest cell.
    pub raycast_octree_hit: Option<RaycastHit>,
    /// Ray intersection with gridline plane.
    pub raycast_gridlines_hit: Option<RaycastHit>,
    /// Ray intersection with selection boundary.
    pub raycast_selection_hit: Option<RaycastHit>,
}
impl ScreenPos for ScreenPos3D {
    type D = Dim3D;

    fn pixel(&self) -> FVec2D {
        self.pixel
    }
    fn layer(&self) -> Layer {
        self.layer
    }
    fn draw_cell(&self, mode: DrawMode) -> Option<BigVec<Self::D>> {
        if self.layer == Layer(0) {
            todo!()
        } else {
            None
        }
    }
}
impl ScreenPos3D {
    /// Returns the global cell position at the mouse cursor in the plane parallel
    /// to the screen at the distance of the viewpoint pivot.
    pub fn global_pos_at_pivot_depth(&self) -> FixedVec3D {
        self.xform
            .pixel_to_global_pos(self.pixel, Viewpoint3D::DISTANCE_TO_PIVOT)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RaycastHit {
    /// Point of intersection between ray and cell.
    pub pos: FixedVec3D,
    /// Position of intersected cell.
    pub cell: BigVec3D,
    /// Face of cell intersected.
    pub face: Face,
    /// Type of thing hit.
    pub thing: RaycastHitThing,
    /// Abstract distance to intersection.
    distance: R64,
}
impl PartialOrd for RaycastHit {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for RaycastHit {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.distance.cmp(&other.distance)
    }
}
impl RaycastHit {
    pub fn new(xform: &CellTransform3D, hit: raycast::Hit, thing: RaycastHitThing) -> Self {
        Self {
            pos: xform.local_to_global_float(hit.pos_float),
            cell: xform.local_to_global_int(hit.pos_int),
            face: hit.face,
            thing,
            distance: hit.t0,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RaycastHitThing {
    Cell,
    Gridlines,
    Selelction,
}
