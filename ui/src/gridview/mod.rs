use anyhow::{anyhow, Result};
use enum_dispatch::enum_dispatch;
use log::{debug, trace, warn};
use parking_lot::Mutex;
use std::borrow::Cow;
use std::collections::VecDeque;
use std::sync::mpsc;
use std::time::{Duration, Instant};

use ndcell_core::prelude::*;

mod camera;
pub mod commands;
mod render;
mod view2d;
mod view3d;
mod worker;

use crate::config::Config;
use crate::history::History;
pub use camera::*;
pub use commands::*;
pub use view2d::GridView2D;
pub use view3d::GridView3D;
use worker::*;

/// The number of render results to remember.
const RENDER_RESULTS_COUNT: usize = 4;
/// The number of previous simulation steps to track for counting UPS.
const MAX_LAST_SIM_TIMES: usize = 4;

/// Abstraction over 2D and 3D gridviews.
#[enum_dispatch(GridViewTrait)]
pub enum GridView {
    View2D(pub GridView2D),
    View3D(pub GridView3D),
}
/// Conversions from an `NdAutomaton` to a `GridView`.
impl From<Automaton2D> for GridView {
    fn from(automaton: Automaton2D) -> Self {
        Self::View2D(GridView2D::from(ProjectedAutomaton::from(automaton)))
    }
}
impl From<Automaton3D> for GridView {
    fn from(automaton: Automaton3D) -> Self {
        Self::View3D(GridView3D::from(ProjectedAutomaton::from(automaton)))
    }
}

impl AsRef<GridViewCommon> for GridView {
    fn as_ref(&self) -> &GridViewCommon {
        match self {
            GridView::View2D(view2d) => view2d.as_ref(),
            GridView::View3D(view3d) => view3d.as_ref(),
        }
    }
}
impl AsMut<GridViewCommon> for GridView {
    fn as_mut(&mut self) -> &mut GridViewCommon {
        match self {
            GridView::View2D(view2d) => view2d.as_mut(),
            GridView::View3D(view3d) => view3d.as_mut(),
        }
    }
}

/// Shared fields common to all `GridView`s.
#[derive(Debug, Default)]
pub struct GridViewCommon {
    /// Queue of pending commands to be executed on the next frame.
    command_queue: Mutex<Vec<Command>>,

    /// Whether the simulation is currently running.
    is_running: bool,
    /// Whether the user is currently drawing.
    is_drawing: bool,
    /// Whether a one-off simulation request is currently happening
    /// concurrently.
    ///
    /// TODO: reconsider is_waiting
    is_waiting: bool,

    /// Communication channel with the garbage collection thread.
    gc_channel: Option<mpsc::Receiver<()>>,

    /// The last several render results, with the most recent at the front.
    pub render_results: VecDeque<RenderResult>,
    /// The last several simulation times, with the most recent at the front.
    pub last_sim_times: VecDeque<Duration>,
}

/// Shared functionality for 2D and 3D gridviews.
#[enum_dispatch]
pub trait GridViewTrait:
    Sized + AsSimulate + History + AsRef<GridViewCommon> + AsMut<GridViewCommon>
{
    /// Enqueues a command to be executed on the next frame.
    ///
    /// This should be preferred to executing commands immediately.
    fn enqueue<C: Into<Command>>(&self, command: C) {
        self.as_ref().command_queue.lock().push(command.into());
    }

    /// Does all the frame things: executes commands, advances the simulation,
    /// etc.
    fn do_frame(&mut self, config: &Config) -> Result<()> {
        // Execute commands.
        let old_command_queue =
            std::mem::replace(&mut *self.as_mut().command_queue.get_mut(), vec![]);
        for command in old_command_queue {
            self.do_command(command, config)?;
        }

        // Interpolate camera.
        let fps = self.fps(config);
        let interpolation = config.ctrl.interpolation;
        self.camera_interpolator().advance(fps, interpolation);

        // Trigger breakpoint.
        if self.is_running()
            && config.sim.use_breakpoint
            && self.generation_count() >= &config.sim.breakpoint_gen
        {
            self.stop_running();
        }

        // Update simulation.
        if self.is_running() {
            self.run_step();
        }

        // Collect garbage if memory usage has gotten too high.
        if !self.gc_in_progress() {
            if self.as_sim().memory_usage() > config.sim.max_memory {
                trace!(
                    "Memory usage reached {} bytes; max is {}",
                    self.as_sim().memory_usage(),
                    config.sim.max_memory
                );
                self.schedule_gc();
            }
        }

        Ok(())
    }
    /// Executes a `Command`.
    fn do_command(&mut self, command: Command, config: &Config) -> Result<()> {
        match command {
            Command::Sim(c) => self.do_sim_command(c, config),
            Command::History(c) => self.do_history_command(c, config),
            Command::Move(c, interpolation) => self.do_move_command(c, interpolation, config),
            Command::Draw(c) => self.do_draw_command(c, config),
            Command::Clipboard(c) => self.do_clipboard_command(c, config),
            Command::GarbageCollect => Ok(self.schedule_gc()),
        }
    }
    /// Executes a `SimCommand`.
    fn do_sim_command(&mut self, command: SimCommand, config: &Config) -> Result<()> {
        match command {
            SimCommand::Step(step_size) => {
                if self.is_running() {
                    self.stop_running()
                }
                self.enqueue_worker_request(WorkerRequest::Step(step_size));
            }
            SimCommand::StepStepSize => {
                if self.is_running() {
                    self.stop_running()
                }
                self.enqueue_worker_request(WorkerRequest::Step(config.sim.step_size.clone()));
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
        Ok(())
    }
    /// Executes a `HistoryCommand`.
    fn do_history_command(&mut self, command: HistoryCommand, _config: &Config) -> Result<()> {
        if self.is_drawing() {
            warn!("Ignoring {:?} command while drawing", command);
            return Ok(());
        }
        self.stop_running();
        match command {
            HistoryCommand::Undo => {
                self.undo();
            }
            HistoryCommand::Redo => {
                self.redo();
            }
            // TODO make this JumpTo instead of UndoTo
            HistoryCommand::UndoTo(gen) => {
                while self.generation_count() > &gen && self.has_undo() {
                    self.undo();
                }
            }
        }
        Ok(())
    }
    /// Executes a `Move2Command`.
    fn do_move_command(
        &mut self,
        command: MoveCommand,
        interpolation: Interpolation,
        config: &Config,
    ) -> Result<()>;
    /// Executes a `Draw` command.
    fn do_draw_command(&mut self, command: DrawCommand, config: &Config) -> Result<()>;
    /// Executes a `Clipboard` command.
    fn do_clipboard_command(&mut self, command: ClipboardCommand, config: &Config) -> Result<()>;

    /// Enqueues a request to the simulation worker thread(s).
    fn enqueue_worker_request(&mut self, request: WorkerRequest);
    /// Resets the simulation worker thread(s).
    fn reset_worker(&mut self);

    /// Returns an immutable reference to the N-dimensional automaton.
    fn as_automaton<'a>(&'a self) -> AutomatonRef<'a>;
    /// Returns a mutable reference to the N-dimensional automaton.
    fn as_automaton_mut<'a>(&'a mut self) -> AutomatonMut<'a>;

    /// Returns whether the simulation is running (i.e. stepping forward every
    /// frame automatically with no user input).
    fn is_running(&self) -> bool {
        self.as_ref().is_running
    }
    /// Starts automatically running the simulation.
    fn start_running(&mut self, config: &Config) {
        self.as_mut().is_running = true;
        self.enqueue_worker_request(WorkerRequest::SimContinuous(config.sim.step_size.clone()));
    }
    /// Stops automatically running the simulation.
    fn stop_running(&mut self) {
        self.as_mut().is_running = false;
        self.as_mut().is_waiting = false;
        self.reset_worker();
    }
    /// Runs the simulation for one step.
    fn run_step(&mut self);

    /// Returns whether the user is currently drawing.
    fn is_drawing(&self) -> bool {
        self.as_ref().is_drawing
    }
    /// Returns whether a one-off simulation request is currently happening
    /// concurrently.
    fn is_waiting(&self) -> bool {
        self.as_ref().is_waiting
    }
    /// Returns the last several simulation times, with the most recent at the
    /// front.
    fn last_sim_times(&self) -> &VecDeque<Duration> {
        &self.as_ref().last_sim_times
    }

    /// Schedules garbage collection.
    fn schedule_gc(&mut self) {
        let old_memory_usage = self.as_sim().memory_usage();
        let (tx, rx) = mpsc::channel();
        self.as_mut().gc_channel = Some(rx);
        trace!("Spawning GC thread ...");
        self.as_sim().schedule_gc(Box::new(
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
    /// Returns `true` if garbage collection is queued or in progress, or
    /// `false` otherwise.
    fn gc_in_progress(&mut self) -> bool {
        if let Some(chan) = &self.as_ref().gc_channel {
            match chan.try_recv() {
                Ok(_) => {
                    self.as_mut().gc_channel = None;
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

    /// Returns the camera interpolator.
    fn camera_interpolator(&mut self) -> &mut dyn Interpolate;
    /// Renders the gridview.
    fn render(
        &mut self,
        config: &Config,
        target: &mut glium::Frame,
        params: RenderParams,
    ) -> Result<Cow<RenderResult>>;
    /// Pushes the `RenderResult` from the most recent render and returns a
    /// reference to it.
    fn push_render_result(&mut self, render_result: RenderResult) -> Cow<RenderResult> {
        let render_results = &mut self.as_mut().render_results;
        if render_results.len() >= RENDER_RESULTS_COUNT {
            render_results.pop_back();
        }
        render_results.push_front(render_result);
        self.nth_render_result(0)
    }
    /// Returns the `RenderResult` of the nth most recent render, or
    /// `RenderResult::default()` if there hasn't been one or it has been
    /// forgotten.
    fn nth_render_result(&self, frame: usize) -> Cow<RenderResult> {
        if frame > RENDER_RESULTS_COUNT {
            warn!("Attempted to access render result {:?} of gridview, but render results are only kept for {:?} frames", frame, RENDER_RESULTS_COUNT);
        }
        self.as_ref()
            .render_results
            .get(frame)
            .map(Cow::Borrowed)
            .unwrap_or_default()
    }
    /// Returns the framerate measured between the last two frames, or the
    /// user-configured FPS if that fails.
    fn fps(&self, config: &Config) -> f64 {
        self.nth_render_result(0)
            .instant
            .checked_duration_since(self.nth_render_result(1).instant)
            .map(|duration| 1.0 / duration.as_secs_f64())
            .unwrap_or(config.gfx.fps)
    }
}

impl GridView {
    /// Helper method to make `impl History for GridView` easier.
    fn as_history(&mut self) -> &mut dyn History {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
}

impl AsSimulate for GridView {
    fn as_sim(&self) -> &dyn Simulate {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
    fn as_sim_mut(&mut self) -> &mut dyn Simulate {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
}

impl History for GridView {
    fn record(&mut self) {
        self.as_history().record()
    }
    fn has_undo(&mut self) -> bool {
        self.as_history().has_undo()
    }
    fn has_redo(&mut self) -> bool {
        self.as_history().has_redo()
    }
    fn undo(&mut self) -> bool {
        let ret = self.as_history().undo();
        self.reset_worker();
        ret
    }
    fn redo(&mut self) -> bool {
        let ret = self.as_history().redo();
        self.reset_worker();
        ret
    }
}

/// Parameters that may control the rendering process.
#[derive(Debug, Default)]
pub struct RenderParams {
    /// The pixel position of the mouse cursor from the top left of the area
    /// where the gridview is being drawn.
    pub cursor_pos: Option<IVec2D>,
}

/// Information computed during the render.
#[derive(Debug, Clone)]
pub struct RenderResult {
    /// The cell in the projected ND-tree that the mouse cursor is hovering
    /// over.
    pub hover_pos: Option<AnyDimFixedVec>,
    /// The cell in the projected ND-tree to draw at if the user draws.
    pub draw_pos: Option<AnyDimFixedVec>,
    /// The moment that this render finished.
    pub instant: Instant,
}
impl Default for RenderResult {
    fn default() -> Self {
        Self {
            hover_pos: None,
            draw_pos: None,
            instant: Instant::now(),
        }
    }
}
