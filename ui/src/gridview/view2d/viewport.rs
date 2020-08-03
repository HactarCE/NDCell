use noisy_float::prelude::r64;
use num::traits::*;
use std::convert::TryInto;

use super::*;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Viewport2D {
    /// Cell position that is at the center of the viewport.
    pub center: FracVec2D,
    /// The zoom level.
    pub zoom: Zoom2D,
}

impl Viewport2D {
    /// Pan the viewport by the given number of pixels along each axis.
    pub fn pan_pixels(&mut self, delta: FVec2D) {
        self.center.frac += delta * r64(self.zoom.cells_per_pixel());
        self.center.normalize();
    }
    /// Pan the viewport by the given number of cells along each axis.
    pub fn pan_cells(&mut self, delta: &FracVec2D) {
        self.center += delta;
        self.center.normalize();
    }
    /// Snap to the nearest integer cell position.
    pub fn snap_pos(&mut self) {
        self.center.frac = self.center.frac.round();
        self.center.normalize();
    }
    /// Snap to the nearest power-of-2 zoom level.
    pub fn snap_zoom(&mut self, fixed_point: Option<FracVec2D>) {
        self.zoom_by(self.zoom.round() / self.zoom, fixed_point);
        self.zoom = self.zoom.round(); // Fix any potential rounding error.
    }
    /// Zoom into or out of the given "fixed-point" position by the given
    /// factor.
    pub fn zoom_by(&mut self, factor: f64, fixed_point: Option<FracVec2D>) {
        assert!(
            factor > 0.0,
            "Zoom factor must be a positive number, not {}",
            factor
        );
        let fixed_point_offset = fixed_point
            .map(|pos| (pos - &self.center).as_fvec())
            .unwrap_or_default();
        let fixed_point_old_pos_pixel_offset =
            fixed_point_offset * r64(self.zoom.pixels_per_cell());
        self.zoom = (self.zoom * factor).clamp();
        let fixed_point_new_pos_pixel_offset =
            fixed_point_offset * r64(self.zoom.pixels_per_cell());
        let delta_pixel_offset =
            fixed_point_new_pos_pixel_offset - fixed_point_old_pos_pixel_offset;
        self.center.frac += delta_pixel_offset * r64(self.zoom.cells_per_pixel());
        self.center.normalize();
    }

    /// Return the abstract "distance" between two viewports.
    pub fn distance(a: &Self, b: &Self) -> f64 {
        // Divide the pixel distance by 400 because pixels are a very small unit
        // compared to zoom powers, and traveling 400 pixels feels about
        // equivalent to zooming in or out by a factor of 2 to me. (Obviously
        // this depends on DPI and probably window size, but it's a good enough
        // approximation.)
        let pixel_distance = Self::lerp_total_pixel_delta(a, b).mag().raw() / 400.0;
        let zoom_distance = a.zoom.power() - b.zoom.power();
        // Use euclidean distance.
        (pixel_distance.powf(2.0) + zoom_distance.powf(2.0)).sqrt()
    }

    /// Return a viewport that is some fraction 0.0 <= t <= 1.0 of the distance
    /// between two viewports using linear interpolation that preserves the
    /// fixed point of the transformation from one viewport to the other.
    ///
    /// This function linearly interpolates panning speed in terms of pixels
    /// rather than cells, and linearly interpolates zoom power rather than zoom
    /// factor (equivalent to an exponential interpolation of zoom power).
    #[must_use]
    pub fn lerp(a: &Self, b: &Self, t: f64) -> Self {
        let mut ret = a.clone();

        // When interpolating position and zoom together, we would want the
        // following constraints:
        //
        // 1. Finish zooming and panning at the same time.
        // 2. Keep zooming "speed" consistent -- zoom by the same FACTOR each
        //    frame, or use linear interpolation on the zoom POWER.
        // 3. Keep panning "speed" consistent -- pan by the same number of
        //    PIXELS each frame (not necessarilly the same number of CELLS).
        //
        // All of these together have the nice property of maintaining the fixed
        // point of the transformation throughout the transformation. (See
        // https://www.youtube.com/watch?v=csInNn6pfT4 for more on fixed
        // points.) Zooming in or out using the scroll wheel uses the mouse
        // position on the grid as a fixed point, so this point stays still,
        // which gives a smooth experience for the user.
        //
        // #1 is trivial -- just multiply all the deltas by the portion. It's
        // trivial to find the difference in zoom power and lerp that
        // separately. The hard part is finding the difference in pixels, and
        // panning that many pixels (integrated over the change in zoom power).

        // Interpolate zoom level using exponentiation.
        let delta_zoom_factor = b.zoom / a.zoom;
        if delta_zoom_factor.log2().abs() < 0.01 {
            // If there's less than 1% of a power of two left, snap to the final
            // zoom level.
            ret.zoom = b.zoom;
        } else {
            ret.zoom_by(delta_zoom_factor.powf(t), None);
        }

        // Read the comments in lerp_total_pixel_delta() before proceeding.
        let total_pixels_delta = Self::lerp_total_pixel_delta(a, b);

        // Now that we know the number of pixels to travel in the whole timestep
        // of 0 <= t <= 1, we have to figure out how many cells to travel during
        // 0 <= t <= T, where T is the "destination" time (argument to this
        // function). We can use the same integral we calculated in
        // lerp_total_pixel_delta (solved for the number of cells), but using
        // z(T) instead of z₂:
        //
        //           pixels * ( 2^(-z₁) - 2^(-z(T)) )
        // cells = - --------------------------------
        //                  ln(2) * (z₁ - z(T))
        //
        // Once again this equation is undefined if z₁ = z₂, but the limit is
        // pixels*(2^z). Compute z(T).
        let z1 = a.zoom.power();
        let z_final = ret.zoom.power();
        let k = ((2.0.powf(-z1) - 2.0.powf(-z_final)) / (-2.0.ln() * (z1 - z_final)))
            .try_into()
            .unwrap_or(r64(2.0.powf(-z1)));
        // Multiply the total number of pixels to travel by T to get the number
        // of pixels to travel on 0 <= t <= T.
        let pixels_delta = total_pixels_delta * r64(t);
        // Finally, multiply by the new k to get the number of cells to travel
        // on 0 <= t <= T.
        let cells_delta = pixels_delta * k;

        if total_pixels_delta.mag() < 0.01 {
            // If there's less than 1% of a pixel left, snap into position.
            ret.center = b.center.clone();
        } else {
            ret.pan_cells(&cells_delta.into());
        }

        ret
    }

    fn lerp_total_pixel_delta(a: &Self, b: &Self) -> FVec2D {
        // Read the comments in the first half of lerp() before proceeding.
        //
        // Find the total number of pixels to travel. The zoom power is a linear
        // function z(t) = z₁ + (z₂ - z₁) t for 0 <= t <= 1, where z₁ and z₂ are
        // the inital and final zoom powers respectively. The number of pixels
        // to travel P is a constant value for that range as well. The number of
        // cells per pixel is 1/(2^z(t)), so the total number of cells to travel
        // is the integral of pixels/(2^z(t)) dt from t=0 to t=1. That integral
        // comes out to the following:
        //
        //           pixels * ( 2^(-z₁) - 2^(-z₂) )
        // cells = - ------------------------------
        //                 ln(2) * (z₁ - z₂)
        //
        // (Note the negative sign in front!) We know how many cells to travel;
        // that's just b.pos - a.pos. So to find P, the total number of pixels,
        // we solve the above equation for P:
        //
        //            cells * ln(2) * (z₁ - z₂)
        // pixels = - -------------------------
        //                2^(-z₁) - 2^(-z₂)
        //
        // This equation is undefined at z₁ = z₂, but the limit is just
        // cells/(2^z), so we'll use that if the floating-point expression
        // fails.

        // Compute k, the whole right-hand side of that equation except the
        // number of cells.
        let z1 = a.zoom.power();
        let z2 = b.zoom.power();
        let k = (-2.0.ln() * (z1 - z2) / (2.0.powf(-z1) - 2.0.powf(-z2)))
            .try_into()
            .unwrap_or(r64(2.0.powf(z1)));
        // Now we can just multiply k by the number of cells to travel get the
        // total number of pixels to travel for the entire interpolation.
        (b.center.clone() - &a.center).as_fvec() * k
    }
}
