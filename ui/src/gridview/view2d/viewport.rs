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
    pub fn snap_zoom(&mut self, invariant_pos: Option<FixedVec2D>) {
        self.zoom_by_factor(self.zoom.round() / self.zoom, invariant_pos);
        self.zoom = self.zoom.round(); // Fix any potential rounding error.
    }
    /// Zoom in or out by the given factor, keeping one invariant point at the
    /// same location on the screen.
    ///
    /// If `invariant_pos` is `None`, then the center of the viewport is
    /// invariant.
    pub fn zoom_by_factor(&mut self, factor: f64, invariant_pos: Option<FixedVec2D>) {
        assert!(
            factor > 0.0,
            "Zoom factor must be a positive number, not {}",
            factor,
        );
        self.zoom_by_power(factor.log2(), invariant_pos)
    }
    /// Zoom in or out by the given power, keeping one invariant point at the
    /// same location on the screen.
    ///
    /// If `invariant_pos` is `None`, then the center of the viewport is
    /// invariant.
    pub fn zoom_by_power(&mut self, power: f64, invariant_pos: Option<FixedVec2D>) {
        // Compute cell offset of `invariant_pos` from center of viewport.
        let invariant_pos_offset = invariant_pos
            .map(|pos| pos - &self.center)
            .unwrap_or_default();
        // Compute pixel offset of `invariant_pos` from the center of viewport
        // using the original zoom level.
        let invariant_pos_old_pixel_offset = self.zoom.cells_to_pixels(&invariant_pos_offset);

        // Zoom into the center of the viewport.
        self.zoom = Zoom2D::from_power(self.zoom.power() + power).clamp();

        // Compute pixel offset of `invariant_pos` from the center of viewport
        // using the new zoom level.
        let invariant_pos_new_pixel_offset = self.zoom.cells_to_pixels(&invariant_pos_offset);
        // Compute the difference between those pixel offsets.
        let delta_pixel_offset = invariant_pos_new_pixel_offset - invariant_pos_old_pixel_offset;
        // Apply that offset so that the point goes back to the same pixel
        // location as before.
        self.center += self.zoom.pixels_to_cells(delta_pixel_offset);
    }

    /// Return the abstract "distance" between two viewports.
    pub fn distance(a: &Self, b: &Self) -> FixedPoint {
        let avg_zoom = Self::average_lerped_zoom(a.zoom, b.zoom);
        let avg_zoom_factor = FixedPoint::from(r64(avg_zoom.power())).exp_base2();
        let total_cells_delta = (&b.center - &a.center).mag();
        let total_pixels_delta = total_cells_delta * avg_zoom_factor;
        // Divide by a constant factor to bring translation to zoom into the
        // same arbitrary units of perceived motion.
        let pixel_distance: FixedPoint = total_pixels_delta / PIXELS_PER_ZOOM_LEVEL;
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
            ret.zoom_by_factor(delta_zoom_factor.powf(t), None);
        }

        // Read the comments in `average_lerped_zoom()` before proceeding.
        let avg_zoom = Self::average_lerped_zoom(a.zoom, b.zoom);
        let total_pixels_delta = avg_zoom.cells_to_pixels(&b.center - &a.center);

        // Now that we know the number of pixels to travel in the whole timestep
        // of 0 <= t <= 1, we have to figure out how many cells to travel during
        // 0 <= t <= T, where T is the "destination" time (argument to this
        // function). We can compute the average zoom level of this smaller
        // interpolation, just ranging from 0 to T, using the same equation as
        // in `average_lerped_zoom` but using z(T) instead of z₂.
        let zt = Self::average_lerped_zoom(a.zoom, ret.zoom);
        // Multiply the total number of pixels to travel by T to get the number
        // of pixels to travel on 0 <= t <= T.
        let pixels_delta = &total_pixels_delta * t;
        // Finally, divide by the new zoom factor to get the number of cells to travel
        // on 0 <= t <= T.
        let cells_delta = zt.pixels_to_cells(pixels_delta);

        if total_pixels_delta.mag() < r64(0.01).into() {
            // If there's less than 1% of a pixel left, snap into position.
            ret.center = b.center.clone();
        } else {
            ret.pan_cells(cells_delta);
        }

        ret
    }

    /// Returns the "average" zoom level between the two viewports, averaging
    /// zoom factor with respect to time during a linear interpolation.
    ///
    /// Read source comments to see how this is relevant.
    fn average_lerped_zoom(z1: Zoom2D, z2: Zoom2D) -> Zoom2D {
        let z1 = r64(z1.power());
        let z2 = r64(z2.power());
        // Read the comments in the first half of lerp() before proceeding.
        //
        // We want to find the total number of pixels to travel. The zoom power
        // is a linear function z(t) = z₁ + (z₂ - z₁) t for 0 <= t <= 1, where
        // z₁ and z₂ are the inital and final zoom powers respectively. The
        // number of pixels to travel P is a constant value for that range as
        // well. The number of cells per pixel is 1/(2^z(t)), so the total
        // number of cells to travel is the integral of pixels/(2^z(t)) dt from
        // t=0 to t=1. That integral comes out to the following:
        //
        //           pixels * ( 2^(-z₁) - 2^(-z₂) )
        // cells = - ------------------------------
        //                 ln(2) * (z₁ - z₂)
        //
        // (Note the negative sign in front!) We know how many cells to travel;
        // that's just b.pos - a.pos. We could solve the above equation for the
        // number of pixels, but instead let's solve it for the ratio of pixels
        // to cells:
        //
        // pixels     ln(2) * (z₁ - z₂)
        // ------ = - -----------------
        // cells      2^(-z₁) - 2^(-z₂)
        //
        // This gives us a sort of "average zoom factor." But zoom factors get
        // very small when zoomed out, so the reciprocal will be more precise:
        //
        //        1            2^(-z₁) - 2^(-z₂)
        // --------------- = - -----------------
        // avg zoom factor     ln(2) * (z₁ - z₂)
        //
        // This equation is undefined at z₁ = z₂, but the limit is just 2^(-z),
        // so we'll use that if z₁ = z₂.

        // Use fixed-point numbers for better precision.
        let z1_fixp = FixedPoint::from(z1);
        let z2_fixp = FixedPoint::from(z2);

        let recip_avg_zoom_factor = if z1_fixp == z2_fixp {
            (-z1_fixp).exp_base2()
        } else {
            let numerator = (-z1_fixp).exp_base2() - (-z2_fixp).exp_base2();
            let denominator = r64(2.0).ln() * (z1 - z2);
            -numerator / FixedPoint::from(denominator)
        };

        // Take the base-2 logarithm and then negate to get the zoom power.
        let avg_zoom_power = -recip_avg_zoom_factor.log2();
        Zoom2D::from_power(avg_zoom_power)
    }
}
