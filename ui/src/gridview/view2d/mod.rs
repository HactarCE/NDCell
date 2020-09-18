use log::{debug, trace, warn};
use parking_lot::{Mutex, RwLock};
use std::borrow::Cow;
use std::collections::VecDeque;
use std::sync::{mpsc, Arc};
use std::time::{Duration, Instant};

use ndcell_core::prelude::*;

mod render;
mod viewport;
mod zoom;

use super::control::*;
use super::worker::*;
use super::{GridViewTrait, RenderGridView, RenderResultTrait};
use crate::clipboard_compat::{clipboard_get, clipboard_set};
use crate::config::{Config, Interpolation2D};
use crate::history::{History, HistoryManager};
use render::{RenderCache, RenderInProgress};
pub use viewport::Viewport2D;
pub use zoom::Zoom2D;

/// The number of render results to remember.
const RENDER_RESULTS_COUNT: usize = 4;
/// The number of previous simulation steps to track for counting UPS.
const MAX_LAST_SIM_TIMES: usize = 4;

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
    /// Communication channel with the garbage collection thread.
    gc_channel: Option<mpsc::Receiver<()>>,
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
        let old_command_queue = std::mem::replace(&mut *self.command_queue.lock(), vec![]);
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
                        MoveCommand2D::Zoom {
                            power,
                            invariant_pos,
                        } => new_viewport.zoom_by_factor(2.0_f64.powf(power), invariant_pos),
                        MoveCommand2D::SetPos(pos) => new_viewport.center = pos,
                        MoveCommand2D::SetZoom {
                            zoom,
                            invariant_pos,
                        } => new_viewport.zoom_by_factor(zoom / new_viewport.zoom, invariant_pos),
                        MoveCommand2D::SnapPos => new_viewport.snap_pos(),
                        MoveCommand2D::SnapZoom(invariant_pos) => {
                            new_viewport.snap_zoom(invariant_pos)
                        }
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
                        Command::StartDraw,
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
                                let result = match self.as_automaton() {
                                    AutomatonRef::Automaton2D(automaton) => {
                                        clipboard_set(RleEncode::to_rle(automaton))
                                            .map_err(|_| "Unable to set clipboard contents")
                                    }
                                    _ => Err("Unable to convert non-2D patterns to RLE"),
                                };
                                if let Err(msg) = result {
                                    warn!("Failed to save RLE to clipboard: {}", msg);
                                }
                            }
                            ClipboardCommand::CopyCxrle => {
                                let result = match self.as_automaton() {
                                    AutomatonRef::Automaton2D(automaton) => {
                                        clipboard_set(RleEncode::to_cxrle(automaton))
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
                                    .and_then(|s| RleEncode::from_rle(&s));
                                match result {
                                    Ok(mut new_automaton) => {
                                        new_automaton.rule = crate::load_custom_rule();
                                        *self = Self::from(new_automaton)
                                    }
                                    Err(msg) => warn!("Failed to load RLE from clipboard: {}", msg),
                                }
                            }
                        }
                    }
                }
                Command::GarbageCollect => {
                    self.schedule_gc();
                }
            }
        }

        // Update viewport.
        let fps = self.fps(config);
        if self.interpolating_viewport != self.viewport {
            let t = match config.ctrl.interpolation_2d {
                Interpolation2D::None => 1.0,
                Interpolation2D::Linear { speed } => {
                    let distance =
                        Viewport2D::distance(&self.interpolating_viewport, &self.viewport);
                    speed / fps / distance.to_f64().unwrap_or(f64::MAX)
                }
                Interpolation2D::Exponential { decay_constant } => 1.0 / fps / decay_constant,
            };
            self.interpolating_viewport = Viewport2D::lerp(
                &self.interpolating_viewport,
                &self.viewport,
                // Clamp to 0 <= t <= 1.
                t.max(0.0).min(1.0),
            );
        }

        // Update automaton.
        if self.is_running
            && config.sim.use_breakpoint
            && self.generation_count() >= &config.sim.breakpoint_gen
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

        // Collect garbage if memory usage has gotten too high.
        if !self.gc_in_progress() {
            if self.automaton.memory_usage() > config.sim.max_memory {
                trace!(
                    "Memory usage reached {} bytes; max is {}",
                    self.automaton.memory_usage(),
                    config.sim.max_memory
                );
                self.schedule_gc();
            }
        }
    }

    fn enqueue<C: Into<Command>>(&self, command: C) {
        self.command_queue.lock().push(command.into());
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

    fn as_automaton<'a>(&'a self) -> AutomatonRef<'a> {
        AutomatonRef::from(&self.automaton)
    }
    fn as_automaton_mut<'a>(&'a mut self) -> AutomatonMut<'a> {
        AutomatonMut::from(&mut self.automaton)
    }
}

impl AsSimulate for GridView2D {
    fn as_sim(&self) -> &dyn Simulate {
        &self.automaton
    }
    fn as_sim_mut(&mut self) -> &mut dyn Simulate {
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
    ) -> Cow<View2DRenderResult> {
        let mut render_cache = std::mem::replace(&mut self.render_cache, None).unwrap_or_default();
        let node_cache = self.automaton.projected_cache().read();
        let mut rip = RenderInProgress::new(self, &node_cache, &mut render_cache, target);
        rip.draw_cells();

        let hover_pos = params.cursor_pos.map(|pos| rip.pixel_pos_to_cell_pos(pos));
        // Only allow drawing at 1:1 or bigger.
        let draw_pos = if self.interpolating_viewport.zoom.power() >= 0.0 {
            hover_pos.clone()
        } else {
            None
        };

        // Draw gridlines.
        let gridlines_width = self.interpolating_viewport.zoom.factor() / 32.0;
        rip.draw_gridlines(gridlines_width);
        // Draw crosshairs.
        if let Some(pos) = &draw_pos {
            rip.draw_blue_cursor_highlight(&pos.floor().0, gridlines_width * 2.0);
        }

        self.render_cache = Some(render_cache);
        if self.render_results.len() >= RENDER_RESULTS_COUNT {
            self.render_results.pop_back();
        }
        self.render_results.push_front(View2DRenderResult {
            hover_pos,
            draw_pos,
            ..Default::default()
        });
        self.nth_render_result(0)
    }
    fn nth_render_result(&self, frame: usize) -> Cow<View2DRenderResult> {
        if frame > RENDER_RESULTS_COUNT {
            warn!("Attempted to access render result {:?} of GridView2D, but render results are only kept for {:?} frames", frame, RENDER_RESULTS_COUNT);
        }
        match &self.render_results.get(frame) {
            Some(res) => Cow::Borrowed(res),
            None => Cow::Owned(View2DRenderResult::default()),
        }
    }
}

impl GridView2D {
    pub fn cache(&self) -> &Arc<RwLock<NodeCache<Dim2D>>> {
        &self.automaton.projected_cache()
    }
    pub fn get_cell(&self, cache: &NodeCache<Dim2D>, pos: &BigVec2D) -> u8 {
        self.automaton.projected_tree().get_cell(cache, pos)
    }

    pub fn gc_in_progress(&mut self) -> bool {
        if let Some(chan) = &self.gc_channel {
            match chan.try_recv() {
                Ok(_) => {
                    drop(chan);
                    self.gc_channel = None;
                    false
                }
                Err(mpsc::TryRecvError::Disconnected) => panic!("GC thread panicked"),
                Err(mpsc::TryRecvError::Empty) => {
                    // There is already a GC in progress.
                    true
                }
            }
        } else {
            false
        }
    }
    pub fn schedule_gc(&mut self) {
        let old_memory_usage = self.automaton.memory_usage();
        let (tx, rx) = mpsc::channel();
        self.gc_channel = Some(rx);
        trace!("Spawning GC thread ...");
        self.automaton.schedule_gc(Box::new(
            move |nodes_dropped, nodes_kept, new_memory_usage| {
                let total_nodes = nodes_dropped + nodes_kept;
                let percent_dropped = nodes_dropped * 100 / total_nodes;
                let memory_saved = old_memory_usage - new_memory_usage;
                let percent_memory_saved = memory_saved * 100 / old_memory_usage;
                debug!(
                    "Dropped {} out of {} nodes ({}%), saving {} bytes ({}%)",
                    nodes_dropped, total_nodes, percent_dropped, memory_saved, percent_memory_saved,
                );
                // Notify the main thread that we are done with GC. If the main
                // thread isn't listening, then that's ok.
                let _ = tx.send(());
            },
        ));
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
#[derive(Debug, Clone)]
pub struct View2DRenderResult {
    /// The cell that the mouse cursor is hovering over.
    pub hover_pos: Option<FixedVec2D>,
    /// The cell to draw at if the user draws.
    pub draw_pos: Option<FixedVec2D>,
    /// The moment that this render finished.
    pub instant: Instant,
}
impl Default for View2DRenderResult {
    fn default() -> Self {
        Self {
            hover_pos: None,
            draw_pos: None,
            instant: Instant::now(),
        }
    }
}
impl RenderResultTrait for View2DRenderResult {
    fn instant(&self) -> Instant {
        self.instant
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
