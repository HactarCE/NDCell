use super::*;

// TODO rewrite zoom

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Viewport2D {
    /// Cell position that is at the center of the viewport.
    pub pos: Vec2D,
    /// Offset along the X axis (0..1).
    pub x_offset: f32,
    /// Offset along the Y axis (0..1).
    pub y_offset: f32,
    /// The zoom level.
    pub zoom: Zoom2D,
}

impl Viewport2D {
    /// Scroll the viewport by the given number of pixels along each axis.
    pub fn scroll_pixels(&mut self, dx: f32, dy: f32) {
        self.scroll_cells(
            dx * self.zoom.cells_per_pixel(),
            dy * self.zoom.cells_per_pixel(),
        );
    }
    /// Scroll the viewport by the given number of cells along each axis.
    pub fn scroll_cells(&mut self, dx: f32, dy: f32) {
        // Add dx and dy.
        self.x_offset += dx;
        self.y_offset += dy;
        // Remove the integral part from self.x_offset and self.y_offset,
        // leaving only the fraction part between 0 and 1.
        let int_dx = self.x_offset.floor();
        let int_dy = self.y_offset.floor();
        self.x_offset -= int_dx;
        self.y_offset -= int_dy;
        // Add the integral part that we removed onto self.pos.
        let int_dx = int_dx as isize;
        let int_dy = int_dy as isize;
        self.pos += Vec2D::from([int_dx, int_dy]);
    }
    /// Snap to the nearest integer cell position.
    pub fn snap_pos(&mut self) {
        if self.x_offset >= 0.5 {
            self.pos += Vec2D::from([1, 0]);
        }
        if self.y_offset >= 0.5 {
            self.pos += Vec2D::from([0, 1]);
        }
        self.x_offset = 0.0;
        self.y_offset = 0.0;
    }
    /// Zoom in or out by the given factor.
    pub fn zoom_by(&mut self, factor: f32) {
        assert!(
            factor > 0.0,
            "Zoom factor must be a positive number, not {}",
            factor
        );
        self.zoom = (self.zoom * factor).clamp();
    }
    /// Returns a viewport that is some fraction of the distance between two viewports.
    pub fn interpolate(a: &Viewport2D, b: &Viewport2D, portion: f32) -> Viewport2D {
        let mut ret = a.clone();

        // Interpolate position.
        let dx_int = b.pos.x() - a.pos.x();
        let dy_int = b.pos.y() - a.pos.y();
        let dx_offset = b.x_offset - a.x_offset;
        let dy_offset = b.y_offset - a.y_offset;
        let mut dx = dx_int as f32 + dx_offset;
        let mut dy = dy_int as f32 + dy_offset;
        if (dx.powf(2.0) + dy.powf(2.0)) * a.zoom.pixels_per_cell() < 0.01 {
            // If there's less than 1% of a pixel left, snap into position.
            ret.pos = b.pos;
            ret.x_offset = b.x_offset;
            ret.y_offset = b.y_offset;
        } else {
            ret.scroll_cells(dx * portion, dy * portion);
        }

        // Interpolate zoom level.
        let mut dz = b.zoom / a.zoom;
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
