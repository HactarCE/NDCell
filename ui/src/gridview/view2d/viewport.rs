use noisy_float::prelude::r64;

use super::*;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Viewport2D {
    /// Cell position that is at the center of the viewport.
    pub pos: BigVec2D,
    /// Cell position offset (0..1).
    pub offset: FVec2D,
    /// The zoom level.
    pub zoom: Zoom2D,
}

impl Viewport2D {
    /// Pan the viewport by the given number of pixels along each axis.
    pub fn pan_pixels(&mut self, delta: FVec2D) {
        self.pan_cells(delta * r64(self.zoom.cells_per_pixel()));
    }
    /// Pan the viewport by the given number of cells along each axis.
    pub fn pan_cells(&mut self, delta: FVec2D) {
        // Add the delta.
        self.offset += delta;
        // Remove the integral part from self.offset, leaving only the fraction
        // part between 0 and 1.
        let int_delta = self.offset.floor();
        self.offset -= int_delta;
        // Add the integral part that we removed onto self.pos.
        self.pos += int_delta.as_ivec();
    }
    /// Snap to the nearest integer cell position.
    pub fn snap_pos(&mut self) {
        if self.offset[X] >= 0.5 {
            self.pos += IVec::unit(X);
        }
        if self.offset[Y] >= 0.5 {
            self.pos += IVec::unit(Y);
        }
        self.offset = NdVec::origin();
    }
    /// Snap to the nearest power-of-2 zoom level.
    pub fn snap_zoom(&mut self) {
        self.zoom = self.zoom.round();
    }
    /// Zoom in or out by the given factor.
    pub fn zoom_by(&mut self, factor: f64) {
        assert!(
            factor > 0.0,
            "Zoom factor must be a positive number, not {}",
            factor
        );
        self.zoom = (self.zoom * factor).clamp();
    }
    /// Returns a viewport that is some fraction of the distance between two viewports.
    #[must_use]
    pub fn interpolate(a: &Viewport2D, b: &Viewport2D, portion: f64) -> Viewport2D {
        let mut ret = a.clone();

        // Interpolate position.
        let delta_int: FVec2D = (b.pos.clone() - &a.pos).as_fvec();
        let delta_offset: FVec2D = b.offset - a.offset;
        let delta = delta_int + delta_offset;
        if delta.mag() * a.zoom.pixels_per_cell() < 0.01 {
            // If there's less than 1% of a pixel left, snap into position.
            ret.pos = b.pos.clone();
            ret.offset = b.offset;
        } else {
            ret.pan_cells(delta * r64(portion));
        }

        // Interpolate zoom level.
        let dz = b.zoom / a.zoom;
        if dz.log2().abs() < 0.01 {
            // If there's less than 1% of a power of two left, snap to the zoom
            // level.
            ret.zoom = b.zoom;
        } else {
            ret.zoom_by(dz.powf(portion));
        }

        ret
    }
}
