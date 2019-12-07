mod grid2d;
mod grid3d;

use super::State;
pub use grid2d::*;
pub use grid3d::*;

pub enum GridView {
    Grid2D(AutomatonView2D),
    Grid3D(Grid3D),
}

/// Draw the CA grid visualization/editor.
pub fn draw(state: &mut State, target: &mut glium::Frame) {
    match state.grid_view {
        GridView::Grid2D(ref mut render_state) => render_state.draw(target),
        GridView::Grid3D(ref mut render_state) => render_state.draw(target),
    }
}
