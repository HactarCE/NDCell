use cgmath::InnerSpace;
use log::warn;
use std::fmt;

use ndcell_core::prelude::*;

use super::algorithms::raycast;
use super::generic::GridViewDimension;
use super::viewpoint::{CellTransform3D, Viewpoint3D};
use crate::commands::{DragCmd, DrawMode};
use crate::ext::FVecConvertExt;
use crate::mouse::MouseDisplayMode;
use crate::{Face, Plane};

pub type OperationPos<D> = <<D as GridViewDimension>::ScreenPos as ScreenPosTrait>::OperationPos;

pub trait OperationPosTrait: fmt::Debug + Sized {
    type D: Dim;

    fn cell(&self) -> &BigVec<Self::D>;
    fn into_cell(self) -> BigVec<Self::D>;
}

/// Convenient representation of a pixel position on the screen.
pub trait ScreenPosTrait: Sized {
    type D: Dim;
    type DrawState: 'static + fmt::Debug;
    type OperationPos: OperationPosTrait<D = Self::D>;

    /// Returns the original pixel location.
    fn pixel(&self) -> FVec2D;
    /// Returns the render cell layer.
    fn layer(&self) -> Layer;

    /// Returns the cell to highlight.
    fn op_pos_for_mouse_display_mode(
        &self,
        mouse_display_mode: MouseDisplayMode,
    ) -> Option<Self::OperationPos> {
        match mouse_display_mode {
            MouseDisplayMode::Draw(draw_mode) => {
                self.op_pos_for_drag_command(&DragCmd::DrawFreeform(draw_mode))
            }

            MouseDisplayMode::Select => self.op_pos_for_drag_command(&DragCmd::SelectNewRect),
            MouseDisplayMode::ResizeSelectionToCursor => {
                self.op_pos_for_drag_command(&DragCmd::ResizeSelectionToCursor)
            }

            _ => None,
        }
    }
    /// Returns the cell that a drag command operates on initially.
    fn op_pos_for_drag_command(&self, command: &DragCmd) -> Option<Self::OperationPos> {
        self.op_pos_for_continue_drag_command(command, self)
    }
    /// Returns the cell that a drag command operates on, given the starting
    /// position of the drag.
    fn op_pos_for_continue_drag_command(
        &self,
        command: &DragCmd,
        start: &Self,
    ) -> Option<Self::OperationPos> {
        match command {
            DragCmd::DrawFreeform(draw_mode) => self.pos_to_draw(*draw_mode, start),

            DragCmd::SelectNewRect | DragCmd::ResizeSelectionToCursor => self.pos_to_select(start),

            _ => None,
        }
    }

    /// Returns the cell that a draw drag command operates on.
    fn pos_to_draw(&self, draw_mode: DrawMode, start: &Self) -> Option<Self::OperationPos>;
    /// Returns the cell that a selection drag command operates on.
    fn pos_to_select(&self, start: &Self) -> Option<Self::OperationPos>;

    /// Returns the position that most view commands operate on.
    fn scale_invariant_pos(&self) -> Option<FixedVec<Self::D>>;

    /// Returns the delta between `initial` and `self` when resizing
    /// `initial_rect` along `resize_vector`.
    fn rect_resize_delta(
        &self,
        initial_rect: &FixedRect<Self::D>,
        initial_screenpos: &Self,
        resize_vector: &IVec<Self::D>,
    ) -> Option<FixedVec<Self::D>>;
    /// Returns the delta between `initial` and `self` along `parallel_face` of
    /// `initial_rect`. `parallel_face` is ignored in 2D but must be `Some` in
    /// 3D.
    fn rect_move_delta(
        &self,
        initial_rect: &FixedRect<Self::D>,
        initial_screenpos: &Self,
        parallel_face: Option<Face>,
    ) -> Option<FixedVec<Self::D>>;
    /// Returns the initial position for absolute resizing of the selection.
    fn absolute_selection_resize_start_pos(&self) -> Option<FixedVec<Self::D>>;
}

#[derive(Debug, Clone)]
pub struct ScreenPos2D {
    pub(super) pixel: FVec2D,
    pub(super) layer: Layer,

    /// Global cell position.
    pub pos: FixedVec2D,
}
impl ScreenPosTrait for ScreenPos2D {
    type D = Dim2D;
    type DrawState = ();
    type OperationPos = OperationPos2D;

    fn pixel(&self) -> FVec2D {
        self.pixel
    }
    fn layer(&self) -> Layer {
        self.layer
    }

    fn pos_to_draw(&self, _draw_mode: DrawMode, _start: &Self) -> Option<Self::OperationPos> {
        if self.layer != Layer(0) {
            return None;
        }

        Some(OperationPos2D { cell: self.cell() })
    }
    fn pos_to_select(&self, _start: &Self) -> Option<Self::OperationPos> {
        Some(OperationPos2D { cell: self.cell() })
    }

    fn scale_invariant_pos(&self) -> Option<FixedVec2D> {
        Some(self.pos())
    }

    fn rect_resize_delta(
        &self,
        _initial_rect: &FixedRect2D,
        initial_screenpos: &Self,
        _resize_vector: &IVec2D,
    ) -> Option<FixedVec2D> {
        Some(self.pos() - &initial_screenpos.pos)
    }
    fn rect_move_delta(
        &self,
        _initial_rect: &FixedRect2D,
        initial_screenpos: &Self,
        _parallel_face: Option<Face>,
    ) -> Option<FixedVec2D> {
        Some(self.pos() - &initial_screenpos.pos)
    }
    fn absolute_selection_resize_start_pos(&self) -> Option<FixedVec2D> {
        Some(self.pos())
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
pub struct OperationPos2D {
    pub cell: BigVec2D,
}
impl OperationPosTrait for OperationPos2D {
    type D = Dim2D;

    fn cell(&self) -> &BigVec2D {
        &self.cell
    }
    fn into_cell(self) -> BigVec2D {
        self.cell
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
impl ScreenPosTrait for ScreenPos3D {
    type D = Dim3D;
    type DrawState = (BigVec3D, Plane);
    type OperationPos = OperationPos3D;

    fn pixel(&self) -> FVec2D {
        self.pixel
    }
    fn layer(&self) -> Layer {
        self.layer
    }

    fn pos_to_draw(&self, draw_mode: DrawMode, initial: &Self) -> Option<OperationPos3D> {
        if self.layer != Layer(0) {
            return None;
        }

        // Determine the plane to draw in.
        let initial_raycast = initial.raycast.as_ref()?;
        let draw_plane = match draw_mode {
            DrawMode::Place => initial_raycast.outside_plane_face(),
            DrawMode::Replace => initial_raycast.inside_plane_face(),
            DrawMode::Erase => initial_raycast.inside_plane_face(),
        };

        // Raycast from the cursor to that plane.
        let cell_to_draw = self.raycast_to_plane_face(&draw_plane)?;

        Some(OperationPos3D {
            cell: cell_to_draw,
            face: draw_plane.face,
        })
    }
    fn pos_to_select(&self, start: &Self) -> Option<OperationPos3D> {
        // First, do a normal raycast. If we hit something, use that.
        if let Some(hit) = &self.raycast {
            return Some(OperationPos3D {
                cell: hit.inside_cell(),
                face: hit.inside_plane_face().face,
            });
        }

        // Determine the plane to select in.
        let initial_raycast = start.raycast.as_ref()?;
        let select_plane = initial_raycast.inside_plane_face();
        // Raycast from the cursor to that plane.
        let cell_to_select = self.raycast_to_plane_face(&select_plane)?;

        Some(OperationPos3D {
            cell: cell_to_select,
            face: select_plane.face,
        })
    }

    fn scale_invariant_pos(&self) -> Option<FixedVec<Self::D>> {
        None
    }

    fn rect_resize_delta(
        &self,
        initial_rect: &FixedRect<Self::D>,
        initial_screenpos: &Self,
        resize_vector: &IVec<Self::D>,
    ) -> Option<FixedVec<Self::D>> {
        // Assume `resize_vector` is a unit vector along one axis. If it isn't,
        // throw a warning and move on.
        let axis = resize_vector.abs().max_axis();
        let face = if resize_vector[axis].is_positive() {
            Face::positive(axis)
        } else {
            Face::negative(axis)
        };
        if face.normal_ivec() != *resize_vector {
            warn!(
                "rect_resize_delta() received weird resize_vector {}; assuming it is {}",
                resize_vector, face,
            );
        }

        let selection_face_plane = face.plane_of(initial_rect);
        let line_start = initial_screenpos.raycast_to_plane(&selection_face_plane)?;
        let line_delta = face.normal_fvec();
        let line_end = self.nearest_global_pos_on_line(&line_start, line_delta)?;

        Some(line_end - line_start)
    }
    fn rect_move_delta(
        &self,
        initial_rect: &FixedRect<Self::D>,
        initial_screenpos: &Self,
        parallel_face: Option<Face>,
    ) -> Option<FixedVec<Self::D>> {
        if parallel_face.is_none() {
            warn!("ScreenPos3D::rect_move_delta() received `parallel_face = None`");
        }

        let selection_face_plane = parallel_face?.plane_of(initial_rect);
        let start_pos = initial_screenpos.raycast_to_plane(&selection_face_plane)?;
        let end_pos = self.raycast_to_plane(&selection_face_plane)?;

        Some(end_pos - start_pos)
    }
    fn absolute_selection_resize_start_pos(&self) -> Option<FixedVec<Self::D>> {
        self.raycast.as_ref().map(|hit| hit.pos.clone())
    }
}
impl ScreenPos3D {
    /// Returns the global cell position at the mouse cursor in the plane parallel
    /// to the screen at the distance of the viewpoint pivot.
    pub fn global_pos_at_pivot_depth(&self) -> FixedVec3D {
        self.xform
            .pixel_to_global_pos(self.pixel, Viewpoint3D::DISTANCE_TO_PIVOT)
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

    /// Returns the cell hit by the ray. If the ray hits the gridline plane,
    /// then the cell in front of the gridlines is considered hit.
    pub fn inside_cell(&self) -> BigVec3D {
        match self.thing {
            RaycastHitThing::Gridlines => self.outside_cell(),
            RaycastHitThing::Cell => self.cell.clone(),
        }
    }
    /// Returns the cell in front of the one hit by the ray.
    pub fn outside_cell(&self) -> BigVec3D {
        self.face.normal_bigvec() + &self.cell
    }

    /// Returns the face of the cell hit by the ray, facing toward the camera.
    /// If the ray hits the gridline plane, then the cell in front of the
    /// gridlines is considered hit, and the face will be opposite.
    pub fn inside_face(&self) -> Face {
        match self.thing {
            RaycastHitThing::Gridlines => self.outside_face(),
            RaycastHitThing::Cell => self.face,
        }
    }
    /// Returns the face of the cell in front of the one hit by the ray, facing
    /// away from the camera. This face has a normal vector in the same
    /// direction as the ray.
    pub fn outside_face(&self) -> Face {
        self.face.opposite()
    }

    /// Returns the plane face of the cell hit by the ray, facing toward the
    /// camera. If the ray hits the gridline plane, then the result is the same
    /// as `outside_plane_face()`.
    fn inside_plane_face(&self) -> PlaneFace {
        match self.thing {
            RaycastHitThing::Gridlines => self.outside_plane_face(),
            RaycastHitThing::Cell => {
                let face = self.inside_face();
                let coord = self.inside_cell()[face.normal_axis()].clone();
                PlaneFace { face, coord }
            }
        }
    }
    /// Returns the plane face of the cell in front of the one hit by the ray,
    /// facing toward the cell hit.
    fn outside_plane_face(&self) -> PlaneFace {
        let face = self.outside_face();
        let coord = self.outside_cell()[face.normal_axis()].clone();
        PlaneFace { face, coord }
    }
}

#[derive(Debug, Clone)]
pub struct OperationPos3D {
    pub cell: BigVec3D,
    pub face: Face,
}
impl OperationPosTrait for OperationPos3D {
    type D = Dim3D;

    fn cell(&self) -> &BigVec3D {
        &self.cell
    }
    fn into_cell(self) -> BigVec3D {
        self.cell
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum RaycastHitThing {
    Gridlines, // gridlines in front
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
