mod grid2d;
mod grid3d;
mod slice2d;

use super::State;
pub use grid2d::*;
pub use grid3d::*;
use slice2d::*;

pub enum GridView {
    Grid2D(Grid2D),
    Grid3D(Grid3D),
}

/// Draw the CA grid visualizer/editor.
pub fn draw_editor(state: &mut State, target: &mut glium::Frame) {
    match state.grid_view {
        GridView::Grid2D(ref mut render_state) => render_state.draw_editor(target),
        GridView::Grid3D(ref mut render_state) => render_state.draw_editor(target),
    }
}
