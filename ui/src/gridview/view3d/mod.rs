use log::warn;
use std::borrow::Cow;
use std::collections::VecDeque;
use std::sync::{mpsc, Mutex};
use std::time::{Duration, Instant};

use ndcell_core::prelude::*;

mod camera;
mod render;

use super::control::*;
use super::worker::*;
use super::{GridViewTrait, RenderGridView, RenderResultTrait};
use crate::config::Config;
use crate::history::HistoryManager;
pub use camera::Camera3D;
use render::{RenderCache, RenderInProgress};

/// The number of render results to remember.
const RENDER_RESULTS_COUNT: usize = 4;
/// The number of previous simulation steps to track for counting UPS.
const MAX_LAST_SIM_TIMES: usize = 4;

#[derive(Default)]
pub struct GridView3D {
    /// Automaton being simulated and displayed.
    pub automaton: ProjectedAutomaton3D,
    /// Target camera.
    pub camera: Camera3D,
    /// Camera that interpolates to the target and is used for drawing.
    pub interpolating_camera: Camera3D,

    /// List of undo states.
    undo_stack: Vec<HistoryEntry>,
    /// List of redo states.
    redo_stack: Vec<HistoryEntry>,
    /// Whether the simulation is currently running.
    pub is_running: bool,
    /// Whether the user is current drawing.
    pub is_drawing: bool,
    /// Whether a one-off simulation request is currently happening
    /// concurrently.
    pub is_waiting: bool,
    /// Communication channel with the simulation worker thread.
    worker: Option<Worker<ProjectedAutomaton3D>>,
    /// Communication channel with the garbage collection thread.
    gc_channel: Option<mpsc::Receiver<()>>,
    /// The last several render results, with the most recent at the front.
    render_results: VecDeque<View3DRenderResult>,
    /// The last simulation time.
    pub last_sim_times: VecDeque<Duration>,
    /// Cached render data unique to this GridView.
    render_cache: Option<RenderCache>,
    /// Queue of pending commands to be executed on the next frame.
    command_queue: Mutex<Vec<Command>>,
}

impl GridViewTrait for GridView3D {
    fn do_frame(&mut self, _config: &Config) {
        // unimplemented!()
    }

    fn enqueue<C: Into<Command>>(&self, _command: C) {
        unimplemented!()
    }

    fn queue_worker_request(&mut self, _request: WorkerRequest) {
        unimplemented!()
    }
    fn reset_worker(&mut self) {
        unimplemented!()
    }

    fn is_running(&self) -> bool {
        self.is_running
    }
    fn start_running(&mut self, _config: &Config) {
        unimplemented!()
    }
    fn stop_running(&mut self) {
        // unimplemented!()
    }

    fn as_automaton<'a>(&'a self) -> AutomatonRef<'a> {
        AutomatonRef::from(&self.automaton)
    }
    fn as_automaton_mut<'a>(&'a mut self) -> AutomatonMut<'a> {
        AutomatonMut::from(&mut self.automaton)
    }
}

impl AsSimulate for GridView3D {
    fn as_sim(&self) -> &dyn Simulate {
        &self.automaton
    }
    fn as_sim_mut(&mut self) -> &mut dyn Simulate {
        &mut self.automaton
    }
}

impl From<ProjectedAutomaton3D> for GridView3D {
    fn from(automaton: ProjectedAutomaton3D) -> Self {
        Self {
            automaton,
            ..Default::default()
        }
    }
}
impl From<Automaton3D> for GridView3D {
    fn from(automaton: Automaton3D) -> Self {
        Self::from(ProjectedAutomaton3D::from(automaton))
    }
}

impl RenderGridView for GridView3D {
    type RenderParams = View3DRenderParams;
    type RenderResult = View3DRenderResult;
    fn render(
        &mut self,
        _config: &Config,
        target: &mut glium::Frame,
        _params: View3DRenderParams,
    ) -> Cow<View3DRenderResult> {
        // let mut render_cache = std::mem::replace(&mut self.render_cache, None).unwrap_or_default();
        let node_cache = self.automaton.projected_cache().read();
        let mut rip = RenderInProgress::new(
            self,
            &node_cache,
            // &mut render_cache,
            target,
        );
        rip.draw_cells();

        // let hover_pos = params.cursor_pos.map(|pos| rip.pixel_pos_to_cell_pos(pos));
        // // Only allow drawing at 1:1 or bigger.
        // let draw_pos = if self.interpolating_viewport.zoom.power() >= 0.0 {
        //     hover_pos.clone()
        // } else {
        //     None
        // };

        // // Draw gridlines.
        // let gridlines_width = self.interpolating_viewport.zoom.factor() / 32.0;
        // rip.draw_gridlines(gridlines_width);
        // // Draw crosshairs.
        // if let Some(pos) = &draw_pos {
        //     rip.draw_blue_cursor_highlight(&pos.floor().0, gridlines_width * 2.0);
        // }

        // self.render_cache = Some(render_cache);
        if self.render_results.len() >= RENDER_RESULTS_COUNT {
            self.render_results.pop_back();
        }
        self.render_results.push_front(View3DRenderResult {
            // hover_pos,
            // draw_pos,
            ..Default::default()
        });
        self.nth_render_result(0)
    }
    fn nth_render_result(&self, frame: usize) -> Cow<View3DRenderResult> {
        if frame > RENDER_RESULTS_COUNT {
            warn!("Attempted to access render result {:?} of GridView3D, but render results are only kept for {:?} frames", frame, RENDER_RESULTS_COUNT);
        }
        match &self.render_results.get(frame) {
            Some(res) => Cow::Borrowed(res),
            None => Cow::Owned(View3DRenderResult::default()),
        }
    }
}

#[derive(Debug, Default)]
pub struct View3DRenderParams {
    /// The pixel position of the mouse cursor from the top left of the area
    /// where the gridview is being drawn.
    pub cursor_pos: Option<IVec2D>,
}
#[derive(Debug, Clone)]
pub struct View3DRenderResult {
    // /// The cell that the mouse cursor is hovering over.
    // pub hover_pos: Option<FixedVec3D>,
    // /// The cell to draw at if the user draws.
    // pub draw_pos: Option<FixedVec3D>,
    /// The moment that this render finished.
    pub instant: Instant,
}
impl Default for View3DRenderResult {
    fn default() -> Self {
        Self {
            // hover_pos: None,
            // draw_pos: None,
            instant: Instant::now(),
        }
    }
}
impl RenderResultTrait for View3DRenderResult {
    fn instant(&self) -> Instant {
        self.instant
    }
}

pub struct HistoryEntry {}

impl HistoryManager for GridView3D {
    type HistoryEntry = HistoryEntry;
    fn history_entry(&self) -> HistoryEntry {
        unimplemented!()
    }
    fn restore(&mut self, _entry: HistoryEntry) -> HistoryEntry {
        unimplemented!()
    }
    fn undo_stack(&mut self) -> &mut Vec<HistoryEntry> {
        unimplemented!()
    }
    fn redo_stack(&mut self) -> &mut Vec<HistoryEntry> {
        unimplemented!()
    }
}
