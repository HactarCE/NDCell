use super::*;

/// Number of pixels to translate that feels equivalent zooming in or out by a
/// factor of 2.
///
/// Pixels are a very small unit compared to zoom powers, and traveling 400
/// pixels feels about equivalent to zooming in or out by a factor of 2 to me.
/// (Obviously this depends on DPI and probably window size, but it's a good
/// enough approximation. Ideally this should be based on the average pixel
/// motion of the whole screen which shouldn't be hard to derive a formula for
/// but that would REALLY be over-engineering this.)
const PIXELS_PER_ZOOM_LEVEL: f64 = 400.0;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Viewport2D {
    /// Cell position that is at the center of the viewport.
    pub center: FixedVec2D,
    /// The zoom level.
    pub zoom: Zoom2D,
}

impl Viewport2D {
    /// Pan the viewport by the given number of pixels along each axis.
    pub fn pan_pixels(&mut self, delta: FVec2D) {
        self.center += (delta * r64(self.zoom.cells_per_pixel())).to_fixedvec();
    }
    /// Pan the viewport by the given number of cells along each axis.
    pub fn pan_cells(&mut self, delta: FixedVec2D) {
        self.center += delta;
    }
    /// Snap to the nearest integer cell position.
    pub fn snap_pos(&mut self) {
        self.center = self.center.round().to_fixedvec();
    }
    /// Snap to the nearest power-of-2 zoom level, keeping one invariant point
    /// at the same location on the screen.
    ///
    /// If `invariant_pos` is `None`, then the center of the viewport is invariant.
    pub fn snap_zoom(&mut self, invariant: Option<FixedVec2D>) {
        self.zoom_by(self.zoom.round() / self.zoom, invariant);
        self.zoom = self.zoom.round(); // Fix any potential rounding error.
    }
    /// Zoom in or out by the given factor, keeping one invariant point at the
    /// same location on the screen.
    ///
    /// If `invariant_pos` is `None`, then the center of the viewport is invariant.
    pub fn zoom_by(&mut self, factor: f64, invariant_pos: Option<FixedVec2D>) {
        assert!(
            factor > 0.0,
            "Zoom factor must be a positive number, not {}",
            factor,
        );

        let cells_per_pixel = self.zoom.cells_per_pixel();

        // Compute cell offset of `invariant_pos` from center of viewport.
        let invariant_pos_offset = invariant_pos
            .map(|pos| pos - &self.center)
            .unwrap_or_default();
        // Compute pixel offset of `invariant_pos` from the center of viewport
        // using the original zoom level.
        let invariant_pos_old_pixel_offset = &invariant_pos_offset / cells_per_pixel;

        // Zoom into the center of the viewport.
        self.zoom = (self.zoom * factor).clamp();

        let cells_per_pixel = self.zoom.cells_per_pixel();

        // Compute pixel offset of `invariant_pos` from the center of viewport
        // using the new zoom level.
        let invariant_pos_new_pixel_offset = &invariant_pos_offset / cells_per_pixel;
        // Compute the difference between those pixel offsets.
        let delta_pixel_offset = invariant_pos_new_pixel_offset - invariant_pos_old_pixel_offset;
        // Apply that offset so that the point goes back to the same pixel
        // location as before.
        self.center += delta_pixel_offset * cells_per_pixel;
    }

    /// Return the abstract "distance" between two viewports.
    pub fn distance(a: &Self, b: &Self) -> FixedPoint {
        // Divide by a constant factor to bring translation to zoom into the
        // same arbitrary units of perceived motion.
        let pixel_distance: FixedPoint =
            Self::lerp_total_pixel_delta(a, b).mag() / PIXELS_PER_ZOOM_LEVEL;
        let zoom_distance = a.zoom.power() - b.zoom.power();
        // Use euclidean distance.
        let squared_pixel_distance = &pixel_distance * &pixel_distance;
        let squared_zoom_distance: FixedPoint = r64(zoom_distance.powf(2.0)).into();
        let squared_distance: FixedPoint = squared_pixel_distance + squared_zoom_distance;
        squared_distance.sqrt()
    }

    /// Return a viewport that is some fraction 0.0 <= t <= 1.0 of the distance
    /// between two viewports using linear interpolation that preserves the
    /// fixed point of the transformation from one viewport to the other.
    ///
    /// This function linearly interpolates panning speed in terms of pixels
    /// rather than cells, and linearly interpolates zoom power rather than zoom
    /// factor (equivalent to an exponential interpolation of zoom power).
    #[must_use = "This method returns a new value instead of mutating its input"]
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
        let mut k = (2.0.powf(-z1) - 2.0.powf(-z_final)) / (-2.0.ln() * (z1 - z_final));
        if !k.is_finite() {
            k = 2.0.powf(-z1);
        }
        // Multiply the total number of pixels to travel by T to get the number
        // of pixels to travel on 0 <= t <= T.
        let pixels_delta = &total_pixels_delta * t;
        // Finally, multiply by the new k to get the number of cells to travel
        // on 0 <= t <= T.
        let cells_delta = pixels_delta * k;

        if total_pixels_delta.mag() < r64(0.01).into() {
            // If there's less than 1% of a pixel left, snap into position.
            ret.center = b.center.clone();
        } else {
            ret.pan_cells(cells_delta);
        }

        ret
    }

    fn lerp_total_pixel_delta(a: &Self, b: &Self) -> FixedVec2D {
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
        let mut k = -2.0.ln() * (z1 - z2) / (2.0.powf(-z1) - 2.0.powf(-z2));
        if !k.is_finite() {
            k = 2.0.powf(z1);
        }
        // Now we can just multiply k by the number of cells to travel get the
        // total number of pixels to travel for the entire interpolation.
        (&b.center - &a.center) * k
    }
}
