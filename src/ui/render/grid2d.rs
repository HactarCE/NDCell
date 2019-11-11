use glium::Surface;

use super::AutomatonSlice2D;
use crate::automaton::*;
use crate::ui::State;

pub struct Grid2D {
    pub slice: Box<dyn AutomatonSlice2D>,
}
impl Grid2D {
    pub fn draw_editor(&mut self, target: &mut glium::Frame) {
        target.clear_color_srgb(0.2, 0.2, 0.2, 1.0);
    }
}
