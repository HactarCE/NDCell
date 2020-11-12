use anyhow::{Context, Result};
use cgmath::Matrix4;
use log::warn;

use ndcell_core::axis::{X, Y};
use ndcell_core::prelude::*;

use super::{Camera, CellTransform2D, DragHandler, DragOutcome, Scale, MIN_TARGET_SIZE};
use crate::commands::{ViewCommand, ViewDragCommand};
use crate::config::Config;

#[derive(Debug, Clone, PartialEq)]
pub struct Camera2D {
    /// Width and height of the viewport.
    target_dimensions: (u32, u32),
    /// Display scaling factor.
    dpi: f32,

    /// Cell coordinates at the center of the camera.
    center: FixedVec2D,
    /// The scale factor.
    scale: Scale,
}

impl Default for Camera2D {
    fn default() -> Self {
        Self {
            target_dimensions: (MIN_TARGET_SIZE, MIN_TARGET_SIZE),
            dpi: 1.0,

            center: FixedVec::origin(),
            scale: Scale::default(),
        }
    }
}

impl Camera2D {
    pub fn pixel_to_screen_pos(&self, pixel: FVec2D) -> Option<ScreenPos2D> {
        let transform = self.cell_transform();
        let cell = transform.pixel_to_global_cell(pixel)?;
        let int_cell = cell.floor().0;
        let render_cell = transform.pixel_to_local_render_cell(pixel)?;
        let int_render_cell = render_cell.floor().to_ivec();

        Some(ScreenPos2D {
            pixel,
            cell,
            int_cell,
            render_cell,
            int_render_cell,
        })
    }

    // Compute the position of the camera in render cell space, given a base
    // position near the camera center.
    pub fn render_cell_pos(&self, base_cell_pos: &BigVec2D) -> FVec2D {
        // Compute the layer of each "render cell."
        let (render_cell_layer, render_cell_scale) = self.render_cell_layer_and_scale();

        let cell_offset = self.pos() - base_cell_pos.to_fixedvec();
        let mut render_cell_pos = (cell_offset >> render_cell_layer.to_u32()).to_fvec();

        // This is the width of a pixel, measured in render cells.
        let one_pixel = render_cell_scale.cells_per_unit();

        // Round to the nearest pixel (disabled because it causes jiggling).
        // render_cell_pos = (render_cell_pos / one_pixel).round() * one_pixel;

        // Offset by half a pixel if the target dimensions are odd, so that
        // cells boundaries always line up with pixel boundaries.
        let (target_w, target_h) = self.target_dimensions();
        if target_w % 2 == 1 {
            render_cell_pos[X] += one_pixel / 2.0;
        }
        if target_h % 2 == 1 {
            render_cell_pos[Y] += one_pixel / 2.0;
        }

        render_cell_pos
    }
}

impl Camera<Dim2D> for Camera2D {
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

    fn pos(&self) -> &FixedVec<Dim2D> {
        &self.center
    }
    fn set_pos(&mut self, pos: FixedVec<Dim2D>) {
        self.center = pos;
    }
    fn snap_pos(&mut self, config: &Config) {
        if config.ctrl.snap_center_2d {
            self.center = self.center.floor().0.to_fixedvec() + 0.5;
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

    fn cell_transform_with_base(&self, base_cell_pos: BigVec2D) -> Result<CellTransform2D> {
        // Compute the layer of each "render cell."
        let (render_cell_layer, render_cell_scale) = self.render_cell_layer_and_scale();

        // Compute the render cell translation matrix.
        let mut render_cell_pos = self.render_cell_pos(&base_cell_pos);
        // Offset by a quarter of a pixel so that OpenGL rounds consistently
        // when drawing things that are right on a cell boundary, such as
        // 1-pixel-wide gridlines.
        render_cell_pos -= render_cell_scale.cells_per_unit() / 4.0;
        // This sub-pixel offest is in the opposite direction of the in
        // render_cell_pos() so that they partially cancel, and never offset the
        // whole image by more than half a pixel.

        let render_cell_translate_matrix = Matrix4::from_translation(-cgmath::vec3(
            render_cell_pos[X]
                .to_f32()
                .context("Base cell position is too far from camera position")?,
            render_cell_pos[Y]
                .to_f32()
                .context("Base cell position is too far from camera position")?,
            0.0,
        ));
        // Compute scale factor for render cells, relative to pixels.
        let render_cell_scale = render_cell_scale.units_per_cell().raw() as f32;
        let render_cell_scale_matrix =
            Matrix4::from_nonuniform_scale(render_cell_scale, render_cell_scale, 1.0);
        // Compute the render cell transform; see `NdCellTransform` docs. The
        // offset is measured in render cells, so we must apply the offset
        // before scaling.
        let render_cell_transform = render_cell_scale_matrix * render_cell_translate_matrix;

        let (target_w, target_h) = self.target_dimensions();

        Ok(CellTransform2D::new_ortho(
            base_cell_pos,
            render_cell_layer,
            render_cell_transform,
            (target_w as f32, target_h as f32),
        ))
    }

    fn do_move_command(
        &mut self,
        command: ViewCommand,
        config: &Config,
    ) -> Result<Option<DragHandler<Self>>> {
        match command {
            ViewCommand::Drag(c, cursor_start) => match c {
                ViewDragCommand::Orbit => {
                    warn!("Ignoring {:?} in Camera2D", command);
                    Ok(None)
                }

                ViewDragCommand::Pan
                | ViewDragCommand::PanAligned
                | ViewDragCommand::PanAlignedVertical
                | ViewDragCommand::PanHorizontal => {
                    let start = self.cell_transform().pixel_to_global_cell(cursor_start);
                    Ok(Some(Box::new(move |cam, cursor_end| {
                        let end = cam.cell_transform().pixel_to_global_cell(cursor_end);
                        if let (Some(start), Some(end)) = (&start, &end) {
                            cam.center += start - end;
                        }
                        Ok(DragOutcome::Continue)
                    })))
                }

                ViewDragCommand::Scale => todo!("Scale using click & drag"),
            },

            ViewCommand::GoTo2D {
                mut x,
                mut y,
                relative,
                scaled,
            } => {
                if scaled {
                    x = x.map(|x| self.scale.units_to_cells(x));
                    y = y.map(|y| self.scale.units_to_cells(y));
                }
                if relative {
                    self.center[X] += x.unwrap_or_default();
                    self.center[Y] += y.unwrap_or_default();
                } else {
                    if let Some(x) = x {
                        self.center[X] = x;
                    }
                    if let Some(y) = y {
                        self.center[Y] = y;
                    }
                }
                Ok(None)
            }
            ViewCommand::GoToScale(scale) => {
                self.set_scale(scale);
                Ok(None)
            }

            ViewCommand::Scale {
                log2_factor,
                invariant_pos,
            } => {
                self.scale_by_log2_factor(
                    R64::try_new(log2_factor).context("Invalid scale factor")?,
                    invariant_pos.and_then(|pixel_pos| {
                        self.cell_transform().pixel_to_global_cell(pixel_pos)
                    }),
                );
                Ok(None)
            }

            ViewCommand::SnapPos => {
                self.snap_pos(config);
                Ok(None)
            }
            ViewCommand::SnapScale { invariant_pos } => {
                self.snap_scale(
                    invariant_pos.and_then(|pixel_pos| {
                        self.cell_transform().pixel_to_global_cell(pixel_pos)
                    }),
                );
                Ok(None)
            }

            ViewCommand::GoTo3D { .. } => {
                warn!("Ignoring {:?} in Camera2D", command);
                Ok(None)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScreenPos2D {
    pixel: FVec2D,
    cell: FixedVec2D,
    int_cell: BigVec2D,
    render_cell: FVec2D,
    int_render_cell: IVec2D,
}
impl ScreenPos2D {
    pub fn pixel(&self) -> FVec2D {
        self.pixel
    }
    pub fn cell(&self) -> &FixedVec2D {
        &self.cell
    }
    pub fn int_cell(&self) -> &BigVec2D {
        &self.int_cell
    }
    pub fn render_cell(&self) -> FVec2D {
        self.render_cell
    }
    pub fn int_render_cell(&self) -> IVec2D {
        self.int_render_cell
    }
}
