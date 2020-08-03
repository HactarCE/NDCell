use enum_dispatch::enum_dispatch;
use std::borrow::Cow;

use ndcell_core::*;

pub mod control;
mod view2d;
mod view3d;
mod worker;

use crate::config::Config;
use crate::history::History;
pub use control::*;
pub use view2d::{GridView2D, View2DRenderParams, View2DRenderResult, Viewport2D, Zoom2D};
pub use view3d::GridView3D;
use worker::*;

/// Methods implemented by GridView by dispatching to the GridView2D or
/// GridView3D within.
///
/// TODO: Document these methods!
#[enum_dispatch]
pub trait GridViewTrait: Sized + NdSimulate + History {
    fn do_frame(&mut self, config: &Config);

    fn enqueue<C: Into<Command>>(&self, command: C);

    fn do_history_command(&mut self, command: HistoryCommand, _config: &Config) {
        match command {
            HistoryCommand::Undo => {
                self.undo();
            }
            HistoryCommand::Redo => {
                self.redo();
            }
            // TODO make this JumpTo instead of UndoTo
            HistoryCommand::UndoTo(gen) => {
                while self.get_generation_count() > &gen && self.has_undo() {
                    self.undo();
                }
            }
        }
    }
    fn do_sim_command(&mut self, command: SimCommand, config: &Config) {
        match command {
            SimCommand::Step(step_size) => {
                if self.is_running() {
                    self.stop_running()
                }
                self.queue_worker_request(WorkerRequest::Step(step_size));
            }
            SimCommand::StepStepSize => {
                if self.is_running() {
                    self.stop_running()
                }
                self.queue_worker_request(WorkerRequest::Step(config.sim.step_size.clone()));
            }
            SimCommand::StartRunning => {
                self.start_running(config);
            }
            SimCommand::StopRunning => {
                self.stop_running();
            }
            SimCommand::ToggleRunning => {
                if self.is_running() {
                    self.stop_running()
                } else {
                    self.start_running(config)
                }
            }
            SimCommand::Cancel => {
                self.stop_running();
            }
        }
    }

    fn queue_worker_request(&mut self, request: WorkerRequest);
    fn reset_worker(&mut self);

    fn is_running(&self) -> bool;
    fn start_running(&mut self, config: &Config);
    fn stop_running(&mut self);

    fn get_automaton<'a>(&'a self) -> Automaton<'a>;
    fn get_automaton_mut<'a>(&'a mut self) -> AutomatonMut<'a>;
}

pub trait RenderGridView: GridViewTrait {
    /// Parameters passed each frame to control the rendering process.
    type RenderParams: Default;
    /// Information generated during the render that may be useful to the
    /// caller.
    type RenderResult: Default + Clone;
    /// Draws the 2D grid and return a RenderResult which includes the
    /// coordinates of the cell that the mouse is hovering over. This is the
    /// entry point for the entire 2D grid rendering process.
    fn render(
        &mut self,
        config: &Config,
        target: &mut glium::Frame,
        params: Self::RenderParams,
    ) -> Cow<Self::RenderResult>;
    /// Returns the RenderResult of the Nth most recent render, or
    /// RenderResult::default() if there hasn't been one or it has been
    /// forgotten.
    fn get_render_result(&self, frame: usize) -> Cow<Self::RenderResult>;
}

/// An enum between 2D and 3D views that manages the automaton.
#[enum_dispatch(GridViewTrait)]
pub enum GridView {
    View2D(pub GridView2D),
    View3D(pub GridView3D),
}

/// Conversions from an NdAutomaton to a GridView.
impl From<Automaton2D> for GridView {
    fn from(automaton: Automaton2D) -> Self {
        Self::View2D(GridView2D::from(ProjectedAutomaton::from(automaton)))
    }
}

impl IntoNdSimulate for GridView {
    fn ndsim(&self) -> &dyn NdSimulate {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
    fn ndsim_mut(&mut self) -> &mut dyn NdSimulate {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
}

impl History for GridView {
    fn record(&mut self) {
        self.history().record()
    }
    fn has_undo(&mut self) -> bool {
        self.history().has_undo()
    }
    fn has_redo(&mut self) -> bool {
        self.history().has_redo()
    }
    fn undo(&mut self) -> bool {
        let ret = self.history().undo();
        self.reset_worker();
        ret
    }
    fn redo(&mut self) -> bool {
        let ret = self.history().redo();
        self.reset_worker();
        ret
    }
}

impl GridView {
    fn history(&mut self) -> &mut dyn History {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
}
