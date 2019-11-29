use super::*;

impl AutomatonView2D {
    pub fn scroll(&mut self, dx: f32, dy: f32) {
        self.x += dx * self.zoom.cells_per_pixel();
        self.y += dy * self.zoom.cells_per_pixel();
    }
    pub fn zoom(&mut self, dz: f32) {
        if dz > 0.0 {
            self.zoom = self.zoom.closer();
        }
        if dz < 0.0 {
            self.zoom = self.zoom.farther();
        }
    }
    pub fn step(&mut self) {
        self.automaton.step();
    }
}
