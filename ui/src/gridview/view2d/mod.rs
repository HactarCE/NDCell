use log::{trace, warn};
use std::collections::VecDeque;
use std::sync::Mutex;
use std::time::Duration;

use ndcell_core::*;

mod render;
mod viewport;
mod zoom;

use super::control::*;
use super::worker::*;
use super::{GridViewTrait, RenderGridView};
use crate::clipboard_compat::{clipboard_get, clipboard_set};
use crate::config::Config;
use crate::history::{History, HistoryManager};
use render::{RenderCache, RenderInProgress};
pub use viewport::Viewport2D;
pub use zoom::Zoom2D;

/// The number of render results to remember.
const RENDER_RESULTS_COUNT: usize = 4;
/// The number of previous simulation steps to track for counting UPS.
const MAX_LAST_SIM_TIMES: usize = 4;

lazy_static! {
    static ref DEFAULT_RENDER_RESULT: View2DRenderResult = View2DRenderResult::default();
}

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
    pub is_running: bool,
    /// Whether the user is current drawing.
    pub is_drawing: bool,
    /// Whether a one-off simulation request is currently happening
    /// concurrently.
    pub is_waiting: bool,
    /// Communication channel with the simulation worker thread.
    worker: Option<Worker<ProjectedAutomaton2D>>,
    /// The last several render results, with the most recent at the front.
    render_results: VecDeque<View2DRenderResult>,
    /// The last simulation time.
    pub last_sim_times: VecDeque<Duration>,
    /// Cached render data unique to this GridView.
    render_cache: Option<RenderCache>,
    /// Queue of pending commands to be executed on the next frame.
    command_queue: Mutex<Vec<Command>>,
}

impl GridViewTrait for GridView2D {
    fn do_frame(&mut self, config: &Config) {
        // Handle commands.
        let old_command_queue = std::mem::replace(&mut *self.command_queue.lock().unwrap(), vec![]);
        for command in old_command_queue.into_iter() {
            let was_running = self.is_running;
            match command {
                Command::Sim(c) => {
                    if was_running || self.is_drawing {
                        self.stop_running();
                    } else {
                        self.do_sim_command(c, config);
                    }
                }
                Command::History(c) => {
                    if !self.is_drawing {
                        self.stop_running();
                        self.do_history_command(c, config);
                    }
                }
                Command::Move2D(c, interpolation) => {
                    let mut new_viewport = match interpolation {
                        Interpolation::Direct => self.interpolating_viewport.clone(),
                        Interpolation::Decay => self.viewport.clone(),
                    };
                    match c {
                        MoveCommand2D::PanPixels(delta) => new_viewport.pan_pixels(delta),
                        MoveCommand2D::ZoomByPower(zoom_power) => {
                            new_viewport.zoom_by(2.0f64.powf(zoom_power))
                        }
                        MoveCommand2D::SetPos(pos) => new_viewport.pos = pos,
                        MoveCommand2D::SnapPos => new_viewport.snap_pos(),
                        MoveCommand2D::SnapZoom => new_viewport.snap_zoom(),
                    };
                    self.viewport = new_viewport;
                    match interpolation {
                        Interpolation::Direct => {
                            self.interpolating_viewport = self.viewport.clone()
                        }
                        Interpolation::Decay => (),
                    };
                }
                Command::StartDraw => {
                    self.stop_running();
                    self.record();
                    self.is_drawing = true;
                }
                Command::EndDraw => self.is_drawing = false,
                Command::Draw2D(c) => {
                    assert!(
                        self.is_drawing,
                        "Attempt to execute draw command before {:?}",
                        Command::StartDraw
                    );
                    assert!(
                        !self.is_running,
                        "Attempt to execute draw command while simulation is running"
                    );
                    match c {
                        DrawCommand2D::Cell(pos, cell_state) => {
                            self.automaton.set_cell(&pos, cell_state)
                        }
                        DrawCommand2D::Line(pos1, pos2, cell_state) => {
                            for pos in ndcell_core::math::bresenham(pos1, pos2) {
                                self.automaton.set_cell(&pos, cell_state);
                            }
                        }
                    }
                }
                Command::Clipboard(c) => {
                    if !self.is_drawing {
                        self.stop_running();
                        match c {
                            ClipboardCommand::CopyRle => {
                                let result = match self.get_automaton() {
                                    Automaton::Automaton2D(automaton) => {
                                        clipboard_set(rle::RleEncode::to_rle(automaton))
                                            .map_err(|_| "Unable to set clipboard contents")
                                    }
                                    _ => Err("Unable to convert non-2D patterns to RLE"),
                                };
                                if let Err(msg) = result {
                                    warn!("Failed to save RLE to clipboard: {}", msg);
                                }
                            }
                            ClipboardCommand::CopyCxrle => {
                                let result = match self.get_automaton() {
                                    Automaton::Automaton2D(automaton) => {
                                        clipboard_set(rle::RleEncode::to_cxrle(automaton))
                                            .map_err(|_| "Unable to set clipboard contents")
                                    }
                                    _ => Err("Unable to convert non-2D patterns to RLE"),
                                };
                                if let Err(msg) = result {
                                    warn!("Failed to save CXRLE to clipboard: {}", msg);
                                }
                            }
                            ClipboardCommand::Paste => {
                                self.record();
                                let result: Result<Automaton2D, _> = clipboard_get()
                                    .map_err(|_| "Unable to access clipboard contents".to_owned())
                                    .and_then(|s| rle::RleEncode::from_rle(&s));
                                match result {
                                    Ok(mut new_automaton) => {
                                        new_automaton.set_sim(Simulation::from(rule::LIFE));
                                        *self = Self::from(new_automaton)
                                    }
                                    Err(msg) => warn!("Failed to load RLE from clipboard: {}", msg),
                                }
                            }
                        }
                    }
                }
            }
        }

        // Update viewport.
        const DECAY_CONSTANT: f64 = 4.0;
        if self.interpolating_viewport != self.viewport {
            self.interpolating_viewport = Viewport2D::interpolate(
                &self.interpolating_viewport,
                &self.viewport,
                1.0 / DECAY_CONSTANT,
            );
        }

        // Update automaton.
        if self.is_running
            && config.sim.use_breakpoint
            && self.get_generation_count() >= &config.sim.breakpoint_gen
        {
            self.stop_running();
        } else {
            if let Some(worker) = self.worker.as_mut() {
                if let Some(WorkerResult {
                    result,
                    record,
                    time,
                }) = worker.take()
                {
                    if !self.is_running {
                        self.is_waiting = worker.get_request_count() > 0;
                    }
                    if record {
                        self.record();
                    }
                    self.automaton = result;
                    self.last_sim_times.push_back(time);
                    if self.last_sim_times.len() > MAX_LAST_SIM_TIMES {
                        self.last_sim_times.pop_front();
                    }
                }
            }
        }
    }

    fn enqueue<C: Into<Command>>(&self, command: C) {
        self.command_queue.lock().unwrap().push(command.into());
    }

    fn queue_worker_request(&mut self, request: WorkerRequest) {
        match &request {
            WorkerRequest::Step(_) => self.is_waiting = true,
            WorkerRequest::SimContinuous(_) => (),
        }
        self.get_worker().request(request);
    }
    fn reset_worker(&mut self) {
        self.worker = None;
        trace!("Reset simulation worker thread");
    }

    fn is_running(&self) -> bool {
        self.is_running
    }
    fn start_running(&mut self, config: &Config) {
        self.is_running = true;
        self.queue_worker_request(WorkerRequest::SimContinuous(config.sim.step_size.clone()));
    }
    fn stop_running(&mut self) {
        self.is_running = false;
        self.is_waiting = false;
        self.reset_worker();
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

impl RenderGridView for GridView2D {
    type RenderParams = View2DRenderParams;
    type RenderResult = View2DRenderResult;
    fn render(
        &mut self,
        _config: &Config,
        target: &mut glium::Frame,
        params: View2DRenderParams,
    ) -> &View2DRenderResult {
        let mut hover_pos = None;
        let mut render_cache = std::mem::replace(&mut self.render_cache, None).unwrap_or_default();
        let mut rip = RenderInProgress::new(self, &mut render_cache, target);
        rip.draw_cells();

        // Draw gridlines.
        let gridlines_width = self.interpolating_viewport.zoom.factor() / 32.0;
        rip.draw_gridlines(gridlines_width);
        // Draw crosshairs.
        if let Some(cursor_pos) = params.cursor_pos {
            hover_pos = rip.pixel_pos_to_cell_pos(cursor_pos);
            if let Some(pos) = &hover_pos {
                rip.draw_blue_cursor_highlight(pos, gridlines_width * 2.0);
            }
        }

        self.render_cache = Some(render_cache);
        if self.render_results.len() >= RENDER_RESULTS_COUNT {
            self.render_results.pop_back();
        }
        self.render_results
            .push_front(View2DRenderResult { hover_pos });
        self.get_render_result(0)
    }
    fn get_render_result(&self, frame: usize) -> &View2DRenderResult {
        if frame > RENDER_RESULTS_COUNT {
            warn!("Attempted to access render result {:?} of GridView2D, but render results are only kept for {:?} frames", frame, RENDER_RESULTS_COUNT);
        }
        &self
            .render_results
            .get(frame)
            .unwrap_or(&*DEFAULT_RENDER_RESULT)
    }
}

impl GridView2D {
    pub fn get_cell(&self, pos: &BigVec2D) -> u8 {
        self.automaton.get_projected_tree().get_cell(pos)
    }
    fn get_worker(&mut self) -> &mut Worker<ProjectedAutomaton2D> {
        if let None = self.worker {
            self.worker = Some(Worker::new(self.automaton.clone()));
        }
        self.worker.as_mut().unwrap()
    }
}

#[derive(Debug, Default)]
pub struct View2DRenderParams {
    /// The pixel position of the mouse cursor from the top left of the area
    /// where the gridview is being drawn.
    pub cursor_pos: Option<IVec2D>,
}
#[derive(Debug, Default)]
pub struct View2DRenderResult {
    /// The cell that the mouse cursor is hovering over.
    pub hover_pos: Option<BigVec2D>,
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
            // Replace automaton, but keep viewport, cache, and everything else.
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
