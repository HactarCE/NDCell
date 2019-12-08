mod grid2d;
mod grid3d;

use super::State;
use crate::automaton::projection::*;
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

impl GridView {
    pub fn get_sim_step_size(&self) -> usize {
        match self {
            GridView::Grid2D(render_state) => render_state.automaton.get_step_size(),
            _ => unimplemented!(),
        }
    }
    pub fn set_sim_step_size(&mut self, sim_step: usize) {
        match self {
            GridView::Grid2D(render_state) => render_state.automaton.set_step_size(sim_step),
            _ => unimplemented!(),
        }
    }
    pub fn step(&mut self, record_in_history: bool) {
        match self {
            GridView::Grid2D(render_state) => render_state.automaton.step(record_in_history),
            _ => unimplemented!(),
        }
    }
    pub fn step_single(&mut self, record_in_history: bool) {
        match self {
            GridView::Grid2D(render_state) => render_state.automaton.step_single(record_in_history),
            _ => unimplemented!(),
        }
    }
    pub fn has_undo(&mut self) -> bool {
        match self {
            GridView::Grid2D(render_state) => render_state.automaton.has_undo(),
            _ => unimplemented!(),
        }
    }
    pub fn has_redo(&mut self) -> bool {
        match self {
            GridView::Grid2D(render_state) => render_state.automaton.has_redo(),
            _ => unimplemented!(),
        }
    }
    pub fn undo(&mut self) -> bool {
        match self {
            GridView::Grid2D(render_state) => render_state.automaton.undo(),
            _ => unimplemented!(),
        }
    }
    pub fn redo(&mut self) -> bool {
        match self {
            GridView::Grid2D(render_state) => render_state.automaton.redo(),
            _ => unimplemented!(),
        }
    }
    pub fn reset(&mut self) -> bool {
        match self {
            GridView::Grid2D(render_state) => render_state.automaton.reset(),
            _ => unimplemented!(),
        }
    }
    pub fn push_to_history(&mut self) {
        match self {
            GridView::Grid2D(render_state) => render_state.automaton.push_to_undo_history(),
            _ => unimplemented!(),
        }
    }
}
