use super::*;

impl AutomatonView2D {
    pub fn scroll(&mut self, dx: f32, dy: f32) {
        self.viewport.scroll(dx, dy);
    }
    pub fn zoom(&mut self, dz: f32) {
        if dz > 0.0 {
            self.viewport.zoom_in();
        }
        if dz < 0.0 {
            self.viewport.zoom_out();
        }
    }
}
