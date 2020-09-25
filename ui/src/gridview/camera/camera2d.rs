use anyhow::{Context, Result};
use log::warn;

use ndcell_core::axis::{X, Y};
use ndcell_core::prelude::*;

use super::{Camera, CellTransform, Scale};
use crate::config::Config;
use crate::gridview::commands::MoveCommand;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Camera2D {
    /// Cell coordinates at the center of the camera.
    center: FixedVec2D,
    /// The scale factor.
    scale: Scale,
}

impl Camera<Dim2D> for Camera2D {
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
        let total_pixels_delta = avg_scale.cells_to_pixels(&b.center - &a.center);

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
        let cells_delta = zt.pixels_to_cells(pixels_delta);
        ret.center += cells_delta;

        ret
    }

    fn do_move_command(
        &mut self,
        command: MoveCommand,
        config: &Config,
        cell_transform: &CellTransform,
    ) -> Result<()> {
        match command {
            MoveCommand::GoTo2D {
                mut x,
                mut y,
                relative,
                scaled,
            } => {
                if scaled {
                    x = x.map(|x| self.scale.pixels_to_cells(x));
                    y = y.map(|y| self.scale.pixels_to_cells(y));
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
            }
            MoveCommand::GoToScale(scale) => {
                self.set_scale(scale);
            }
            MoveCommand::Pan { start, end } => {
                let delta = (start - end) * NdVec([r64(1.0), r64(-1.0)]);
                self.center += self.scale.pixels_to_cells(delta.to_fixedvec());
            }
            MoveCommand::Scale {
                log2_factor,
                invariant_pos,
            } => {
                self.scale_by_log2_factor(
                    R64::try_new(log2_factor).context("Invalid scale factor")?,
                    invariant_pos.and_then(|pixel_pos| cell_transform.pixel_to_cell_2d(pixel_pos)),
                );
            }
            MoveCommand::SnapPos => {
                self.snap_pos(config);
            }
            MoveCommand::SnapScale { invariant_pos } => {
                self.snap_scale(
                    invariant_pos.and_then(|pixel_pos| cell_transform.pixel_to_cell_2d(pixel_pos)),
                );
            }

            MoveCommand::GoTo3D { .. } | MoveCommand::Orbit { .. } => {
                warn!("Ignoring {:?} in Camera2D", command)
            }
        }

        Ok(())
    }
}
