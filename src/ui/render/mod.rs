mod grid2d;
mod grid3d;
mod shaders;
mod slice2d;

use super::State;
pub use grid2d::*;
pub use grid3d::*;
pub use slice2d::*;

pub enum GridView {
    Grid2D(AutomatonView2D),
    Grid3D(Grid3D),
}

/// Draw the CA grid visualization/editor.
pub fn draw(state: &mut State, display: &glium::Display, target: &mut glium::Frame) {
    match state.grid_view {
        GridView::Grid2D(ref mut render_state) => render_state.draw(display, target),
        GridView::Grid3D(ref mut render_state) => render_state.draw(display, target),
    }
}
