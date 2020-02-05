use std::cell::RefCell;
use std::rc::Rc;

mod render;
mod shaders;
mod viewport;
mod zoom;

use super::GridViewTrait;
use crate::automaton::*;
use crate::ui::history::{History, HistoryManager};
pub use viewport::Viewport2D;
pub use zoom::Zoom2D;

#[derive(Default)]
pub struct GridView2D {
    /// Automaton being simulated and displayed.
    pub automaton: ProjectedAutomaton2D,
    /// Target viewport.
    pub viewport: Viewport2D,
    /// Viewport that interpolates to the target and is used for drawing.
    pub interpolating_viewport: Viewport2D,
    /// List of undo states.
    undo_stack: Vec<HistoryEntry>,
    /// List of redo states.
    redo_stack: Vec<HistoryEntry>,
    /// Whether the simulation is currently running.
    is_running: bool,
    // TODO: probably move render cache to ref_thread_local
    render_cache: Rc<RefCell<render::RenderCache>>,
    /// The coordinates of the cell that the mouse is currently hovering over,
    /// if any.
    hover_pos: Option<BigVec2D>,
}

impl GridViewTrait for GridView2D {
    fn render(&mut self, target: &mut glium::Frame) {
        render::render(self, target);
    }

    fn do_frame(&mut self) {
        const DECAY_CONSTANT: f64 = 4.0;
        if self.interpolating_viewport != self.viewport {
            self.interpolating_viewport = Viewport2D::interpolate(
                &self.interpolating_viewport,
                &self.viewport,
                1.0 / DECAY_CONSTANT,
            );
        }
        self.do_sim_frame();
    }

    fn is_running(&self) -> bool {
        self.is_running
    }
    fn start_running(&mut self) {
        self.record();
        self.is_running = true;
    }
    fn stop_running(&mut self) {
        self.is_running = false;
    }
    fn get_automaton<'a>(&'a self) -> Automaton<'a> {
        Automaton::from(&self.automaton)
    }
    fn get_automaton_mut<'a>(&'a mut self) -> AutomatonMut<'a> {
        AutomatonMut::from(&mut self.automaton)
    }
}

impl IntoNdSimulate for GridView2D {
    fn ndsim(&self) -> &dyn NdSimulate {
        &self.automaton
    }
    fn ndsim_mut(&mut self) -> &mut dyn NdSimulate {
        &mut self.automaton
    }
}

impl From<ProjectedAutomaton2D> for GridView2D {
    fn from(automaton: ProjectedAutomaton2D) -> Self {
        Self {
            automaton,
            ..Default::default()
        }
    }
}
impl From<Automaton2D> for GridView2D {
    fn from(automaton: Automaton2D) -> Self {
        Self::from(ProjectedAutomaton2D::from(automaton))
    }
}

impl GridView2D {
    pub fn use_viewport_from(&mut self, other: &Self) {
        self.viewport = other.viewport.clone();
        self.interpolating_viewport = other.interpolating_viewport.clone();
    }
    pub fn get_cell(&self, pos: &BigVec2D) -> u8 {
        self.automaton.get_projected_tree().get_cell(pos)
    }
    pub fn set_cell(&mut self, pos: &BigVec2D, new_cell_state: u8) {
        self.automaton.set_cell(pos, new_cell_state)
    }
    pub fn get_hover_pos(&self) -> Option<&BigVec2D> {
        self.hover_pos.as_ref()
    }
}

pub struct HistoryEntry {
    automaton: ProjectedAutomaton2D,
}

impl HistoryManager for GridView2D {
    type HistoryEntry = HistoryEntry;
    fn history_entry(&self) -> HistoryEntry {
        HistoryEntry {
            automaton: self.automaton.clone(),
        }
    }
    fn restore(&mut self, entry: HistoryEntry) -> HistoryEntry {
        HistoryEntry {
            // Replace automaton, but keep viewport.
            automaton: std::mem::replace(&mut self.automaton, entry.automaton),
        }
    }
    fn undo_stack(&mut self) -> &mut Vec<HistoryEntry> {
        &mut self.undo_stack
    }
    fn redo_stack(&mut self) -> &mut Vec<HistoryEntry> {
        &mut self.redo_stack
    }
}
