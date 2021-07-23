use cgmath::{Matrix4, SquareMatrix};
use log::warn;

use ndcell_core::prelude::*;
use Axis::{X, Y};

use super::{
    CellTransform2D, DragOutcome, DragUpdateViewFn, ProjectionType, Scale, Viewpoint,
    MIN_TARGET_SIZE,
};
use crate::commands::{Cmd, DragViewCmd, Move2D, Move3D};
use crate::CONFIG;

macro_rules! ignore_command {
    ($c:expr) => {
        warn!("Ignoring {:?} in Viewpoint2D", $c);
    };
}

#[derive(Debug, Clone, PartialEq)]
pub struct Viewpoint2D {
    /// Width and height of the render target.
    target_dimensions: (u32, u32),
    /// Display scaling factor.
    dpi: f32,

    /// Cell coordinates at the center of the viewpoint.
    center: FixedVec2D,
    /// The scale factor.
    scale: Scale,
}

impl Default for Viewpoint2D {
    fn default() -> Self {
        Self {
            target_dimensions: (MIN_TARGET_SIZE, MIN_TARGET_SIZE),
            dpi: 1.0,

            center: FixedVec::origin(),
            scale: Scale::default(),
        }
    }
}

impl Viewpoint<Dim2D> for Viewpoint2D {
    fn target_dimensions(&self) -> (u32, u32) {
        self.target_dimensions
    }
    fn set_target_dimensions(&mut self, (target_w, target_h): (u32, u32)) {
        self.target_dimensions = (
            std::cmp::max(MIN_TARGET_SIZE, target_w),
            std::cmp::max(MIN_TARGET_SIZE, target_h),
        );
    }
    fn dpi(&self) -> f32 {
        self.dpi
    }
    fn set_dpi(&mut self, dpi: f32) {
        self.dpi = dpi;
    }

    fn center(&self) -> &FixedVec<Dim2D> {
        &self.center
    }
    fn set_center(&mut self, pos: FixedVec<Dim2D>) {
        self.center = pos;
    }
    fn snap_center(&mut self) {
        if CONFIG.lock().ctrl.snap_center_2d {
            self.center = self.center.floor().to_fixedvec() + 0.5;
        } else {
            self.center = self.center.round().to_fixedvec();
        }
    }

    fn scale(&self) -> Scale {
        self.scale
    }
    fn set_scale(&mut self, scale: Scale) {
        self.scale = scale.clamp();
    }

    fn lerp(a: &Self, b: &Self, t: R64) -> Self {
        let mut ret = a.clone();

        // When interpolating position and scale together, we would want the
        // following constraints:
        //
        // 1. Finish scaling and panning at the same time.
        // 2. Keep scaling "speed" consistent -- scale by the same factor each
        //    frame by using lerping the logarithm of the scale factor.
        // 3. Keep panning "speed" consistent -- pan by the same number of
        //    PIXELS each frame (not necessarilly the same number of CELLS).
        //
        // All of these together have the nice property of maintaining the fixed
        // point of the transformation throughout the transformation. (See
        // https://www.youtube.com/watch?v=csInNn6pfT4 for more on fixed
        // points.) Scaling using the scroll wheel uses the mouse position on
        // the grid as a fixed point, so this point stays still, which gives a
        // smooth experience for the user.
        //
        // #1 is trivial -- just start both transformations at t=0 and end them
        // both at t=1. The hard part is finding the difference in pixels, and
        // panning that many pixels (integrated over the change in scale).

        // Interpolate scale factor logarithmically.
        let delta_scale_factor = b.scale / a.scale;
        ret.scale_by_factor(delta_scale_factor.powf(t), None);

        // Read the comments in `average_lerped_scale()` before proceeding.
        let avg_scale = super::average_lerped_scale(a.scale, b.scale);
        // Convert the cell distance to the total number of pixels to travel.
        let total_pixels_delta = avg_scale.cells_to_units(&b.center - &a.center);

        // Now that we know the number of pixels to travel in the whole timestep
        // of 0 <= t <= 1, we have to figure out how many cells to travel during
        // 0 <= t <= T, where T is the "destination" time (argument to this
        // function). We can compute the average scale of this smaller
        // interpolation just ranging from 0 to T using `average_lerped_scale`
        // again, but using s(T) instead of s₂.
        let zt = super::average_lerped_scale(a.scale, ret.scale);
        // Multiply the total number of pixels to travel by T to get the number
        // of pixels to travel on 0 <= t <= T.
        let pixels_delta = &total_pixels_delta * &FixedPoint::from(t);
        // Finally, divide by the new scale factor to get the number of cells to
        // travel on 0 <= t <= T.
        let cells_delta = zt.units_to_cells(pixels_delta);
        ret.center += cells_delta;

        ret
    }

    fn cell_transform(&self) -> CellTransform2D {
        let (render_cell_layer, render_cell_scale) = self.render_cell_layer_and_scale();
        let camera_transform = Matrix4::identity();
        let viewpoint_center = if render_cell_scale.log2_factor().fract().is_zero() {
            // When the scale factor is an exact power of two, round to the
            // nearest pixel to make the final image more crisp. This is
            // disabled otherwise because it causes noticeable jiggling during
            // interpolation.
            let mut center_in_pixel_units = self.scale.cells_to_units(&self.center);
            center_in_pixel_units = center_in_pixel_units.round().to_fixedvec();
            // Offset by half a pixel if the target dimensions are odd, so that
            // cells boundaries line up with pixel boundaries.
            let (target_w, target_h) = self.target_dimensions();
            if target_w % 2 == 1 {
                center_in_pixel_units[X] += 0.5_f64;
            }
            if target_h % 2 == 1 {
                center_in_pixel_units[Y] += 0.5_f64;
            }
            self.scale.units_to_cells(center_in_pixel_units)
        } else {
            self.center.clone()
        };

        CellTransform2D::new(
            viewpoint_center,
            render_cell_layer,
            render_cell_scale,
            camera_transform,
            ProjectionType::Orthographic,
            self.target_dimensions(),
        )
    }

    fn global_visible_rect(&self) -> BigRect2D {
        // Compute the width and height of individual cells that fit on the
        // screen.
        let (target_w, target_h) = self.target_dimensions();
        let target_pixels_size: IVec2D = NdVec([target_w as isize, target_h as isize]);
        let target_cells_size: FixedVec2D = self
            .scale()
            .units_to_cells(target_pixels_size.to_fixedvec());
        // Compute the cell vector pointing from the center of the screen to the
        // top right corner; i.e., the "half diagonal."
        let half_diag: FixedVec2D = target_cells_size / 2.0;

        // Round to render cell boundaries.
        let render_cell_layer = self.render_cell_layer();
        render_cell_layer.round_rect(&BigRect2D::centered(
            self.center().floor(),
            &half_diag.ceil(),
        ))
    }

    fn drag_orbit_3d(&self, _cursor_start: FVec2D) -> Option<DragUpdateViewFn<Self>> {
        ignore_command!(DragViewCmd::Orbit3D);
        None
    }
    fn drag_pan(&self, cursor_start: FVec2D) -> Option<DragUpdateViewFn<Self>> {
        let start = self.cell_transform().pixel_to_global_pos(cursor_start);
        Some(Box::new(move |this, cursor_end| {
            let end = this.cell_transform().pixel_to_global_pos(cursor_end);
            this.center += start.clone() - end;
            Ok(DragOutcome::Continue)
        }))
    }
    fn drag_pan_horizontal_3d(&self, cursor_start: FVec2D) -> Option<DragUpdateViewFn<Self>> {
        self.drag_pan(cursor_start)
    }

    fn apply_move_2d(&mut self, movement: Move2D) {
        let Move2D { dx, dy } = movement;
        let delta: FVec2D = NdVec([r64(dx), r64(dy)]);
        self.center += self.scale.units_to_cells(delta.to_fixedvec());
    }
    fn apply_move_3d(&mut self, movement: Move3D) {
        ignore_command!(Cmd::Move3D(movement));
    }
}
