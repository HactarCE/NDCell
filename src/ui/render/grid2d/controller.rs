use super::*;

impl AutomatonView2D {
    pub fn scroll(&mut self, dx: f32, dy: f32) {
        self.x += dx / self.scale;
        self.y += dy / self.scale;
    }
    pub fn zoom(&mut self, dz: f32) {
        self.scale *= 2.0f32.powf(dz);
    }
    pub fn step(&mut self) {
        self.automaton.step();
    }
}
