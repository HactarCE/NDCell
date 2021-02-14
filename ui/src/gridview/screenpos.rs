use ndcell_core::prelude::*;

use super::algorithms::raycast;
use super::viewpoint::{CellTransform3D, Viewpoint3D};
use crate::{commands::DrawMode, ext::FVecConvertExt};
use crate::{Face, Plane};
use cgmath::InnerSpace;

/// Convenient representation of a pixel position on the screen.
pub trait ScreenPos: Sized {
    type D: Dim;
    type DrawState: 'static;

    /// Returns the original pixel location.
    fn pixel(&self) -> FVec2D;
    /// Returns the render cell layer.
    fn layer(&self) -> Layer;

    /// Returns the cell to draw on, which is automatically `None` if zoomed out
    /// too far to draw.
    fn cell_to_draw(&self, mode: DrawMode, initial: Option<&Self>) -> Option<BigVec<Self::D>>;
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
    type DrawState = ();

    fn pixel(&self) -> FVec2D {
        self.pixel
    }
    fn layer(&self) -> Layer {
        self.layer
    }

    fn cell_to_draw(&self, _mode: DrawMode, _initial: Option<&Self>) -> Option<BigVec2D> {
        if self.layer != Layer(0) {
            return None;
        }

        Some(self.pos.floor())
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
    type DrawState = (BigVec3D, Plane);

    fn pixel(&self) -> FVec2D {
        self.pixel
    }
    fn layer(&self) -> Layer {
        self.layer
    }

    fn cell_to_draw(&self, mode: DrawMode, initial: Option<&Self>) -> Option<BigVec3D> {
        if self.layer != Layer(0) {
            return None;
        }

        let (cell, _face) = self.draw_cell_and_face(mode, initial)?;
        Some(cell)
    }
}
impl ScreenPos3D {
    /// Returns the global cell position at the mouse cursor in the plane parallel
    /// to the screen at the distance of the viewpoint pivot.
    pub fn global_pos_at_pivot_depth(&self) -> FixedVec3D {
        self.xform
            .pixel_to_global_pos(self.pixel, Viewpoint3D::DISTANCE_TO_PIVOT)
    }

    pub fn draw_cell_and_face(
        &self,
        mode: DrawMode,
        initial: Option<&Self>,
    ) -> Option<(BigVec3D, Face)> {
        if self.layer != Layer(0) {
            return None;
        }

        // Determine the plane to draw in.
        let initial_raycast = initial.unwrap_or(&self).raycast.as_ref()?;
        let draw_plane = match mode {
            DrawMode::Place => initial_raycast.outside_plane_face(),
            DrawMode::Replace => initial_raycast.inside_plane_face(),
            DrawMode::Erase => initial_raycast.inside_plane_face(),
        };

        // Raycast from the cursor to that plane.
        let cell_to_draw = self.raycast_to_plane_face(&draw_plane)?;

        Some((cell_to_draw, draw_plane.face))
    }

    fn raycast_to_plane_face(&self, plane: &PlaneFace) -> Option<BigVec3D> {
        let mut ret = self.raycast_to_plane(&plane.fixedpoint_plane())?.floor();

        // Account for possible off-by-one error during the raycast.
        ret[plane.face.normal_axis()] = plane.coord.clone();

        Some(ret)
    }
    pub fn raycast_to_plane(&self, plane: &Plane) -> Option<FixedVec3D> {
        self.xform.pixel_to_global_pos_in_plane(self.pixel, plane)
    }

    pub fn nearest_global_pos_on_line(
        &self,
        line_start: &FixedVec3D,
        line_delta: FVec3D,
    ) -> Option<FixedVec3D> {
        let line_start = self.xform.global_to_local_float(line_start)?;
        let local_result = self.nearest_local_pos_on_line(line_start, line_delta)?;
        Some(self.xform.local_to_global_float(local_result))
    }
    pub fn nearest_local_pos_on_line(
        &self,
        line_start: FVec3D,
        line_delta: FVec3D,
    ) -> Option<FVec3D> {
        // https://math.stackexchange.com/questions/1414285/

        let p1 = line_start.to_cgmath_point3();
        let d1 = line_delta.to_cgmath_vec3();
        let (pixel_ray_start, pixel_ray_delta) = self.xform.pixel_to_local_ray(self.pixel);
        let p2 = pixel_ray_start.to_cgmath_point3();
        let d2 = pixel_ray_delta.to_cgmath_vec3();

        let n = d1.cross(d2);
        let n1 = d1.cross(n);
        let n2 = d2.cross(n);

        let t2 = (p1 - p2).dot(n1) / d2.dot(n1);
        if t2.is_finite() && t2 > 0.0 {
            let t1 = (p2 - p1).dot(n2) / d1.dot(n2);
            let c1 = p1 + t1 * d1;
            Some(NdVec([
                r64(c1.x as f64),
                r64(c1.y as f64),
                r64(c1.z as f64),
            ]))
        } else {
            None
        }
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
        if self.cell == other.cell && self.face == other.face {
            // Z fighting! Sort by thing instead.
            self.thing.cmp(&other.thing)
        } else {
            self.distance.cmp(&other.distance)
        }
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

    /// Returns the plane face of the cell hit by the ray, facing toward the
    /// camera. If the ray hits the gridline plane, then the result is the same
    /// as `outside_plane_face()`.
    fn inside_plane_face(&self) -> PlaneFace {
        match self.thing {
            RaycastHitThing::Selection => todo!(),
            RaycastHitThing::Gridlines => self.outside_plane_face(),
            RaycastHitThing::Cell => self._inside_plane_face(),
        }
    }
    /// Returns the plane face of the cell in front of the one hit by the ray,
    /// facing toward the cell hit.
    fn outside_plane_face(&self) -> PlaneFace {
        let mut ret = self._inside_plane_face();
        ret.coord += match self.face.sign() {
            Sign::Minus => -1,
            Sign::NoSign => unreachable!(),
            Sign::Plus => 1,
        };
        ret.face = self.face.opposite();
        ret
    }

    fn _inside_plane_face(&self) -> PlaneFace {
        let face = self.face;
        let coord = self.cell[face.normal_axis()].clone();
        PlaneFace { face, coord }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum RaycastHitThing {
    Selection, // selection in front
    Gridlines, // then gridlines
    Cell,      // then cells
}

/// A global cell plane and one of its two faces.
#[derive(Debug, Clone, PartialEq, Eq)]
struct PlaneFace {
    pub face: Face,
    pub coord: BigInt,
}
impl PlaneFace {
    fn fixedpoint_plane(&self) -> Plane {
        Plane {
            axis: self.face.normal_axis(),
            coordinate: if self.face.sign() == Sign::Plus {
                FixedPoint::from(&self.coord + 1)
            } else {
                FixedPoint::from(self.coord.clone())
            },
        }
    }
}
