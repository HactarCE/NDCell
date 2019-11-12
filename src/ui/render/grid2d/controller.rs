use super::*;

impl Grid2D {
    pub fn scroll(&mut self, dx: f32, dy: f32) {
        self.x += dx / self.scale;
        self.y += dy / self.scale;
    }
    pub fn zoom(&mut self, dz: f32) {
        self.scale *= 2.0f32.powf(dz);
    }
    pub fn step(&mut self) {
        self.slice.step();
    }
}
