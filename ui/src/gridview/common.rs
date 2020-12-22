use anyhow::{anyhow, Context, Result};
use enum_dispatch::enum_dispatch;
use glium::glutin::event::ModifiersState;
use log::{debug, trace};
use parking_lot::Mutex;
use std::collections::VecDeque;
use std::sync::mpsc;
use std::time::{Duration, Instant};

use ndcell_core::prelude::*;

use super::camera::Interpolate;
use super::history::History;
use super::worker::*;
use crate::commands::*;
use crate::config::{Config, MouseDisplay, MouseDragBinding};

/// The number of render results to remember.
const RENDER_RESULTS_COUNT: usize = 4;
/// The number of previous simulation steps to track for counting UPS.
const MAX_LAST_SIM_TIMES: usize = 4;

/// Gridview functionality that does not depend on the number of dimensions.
#[derive(Debug)]
pub struct GridViewCommon {
    /// Queue of pending commands to be executed on the next frame.
    command_queue: Mutex<VecDeque<Command>>,
    /// Thread to offload long-running computations onto.
    worker_thread: WorkerThread,
    /// What kind of work the worker thread is doing right now.
    work_type: Option<WorkType>,

    /// State of the mouse cursor.
    pub mouse: MouseState,

    /// Whether the user is currently drawing.
    pub is_drawing: bool,
    /// Whether the user is currently dragging the viewport by holding down a
    /// mouse button.
    pub is_dragging_view: bool,

    /// Communication channel with the garbage collection thread.
    gc_channel: Option<mpsc::Receiver<()>>,

    /// Time that the last several frames completed.
    pub last_frame_times: VecDeque<Instant>,
    /// Last several simulation times, with the most recent at the front.
    pub last_sim_times: VecDeque<Duration>,
    /// Most recent render result.
    pub last_render_result: RenderResult,

    /// Selected cell state.
    pub selected_cell_state: u8,
}
impl Default for GridViewCommon {
    fn default() -> Self {
        Self {
            command_queue: Default::default(),
            worker_thread: Default::default(),
            work_type: Default::default(),

            mouse: Default::default(),

            is_drawing: Default::default(),
            is_dragging_view: Default::default(),

            gc_channel: Default::default(),

            last_frame_times: Default::default(),
            last_sim_times: Default::default(),
            last_render_result: Default::default(),

            selected_cell_state: 1_u8,
        }
    }
}

/// Common interface for 2D and 3D gridviews, with default implementations for
/// methods that would be identical.
#[enum_dispatch]
pub trait GridViewTrait:
    Sized + AsSimulate + History + AsRef<GridViewCommon> + AsMut<GridViewCommon>
{
    /// Enqueues a command to be executed on the next frame.
    ///
    /// This should be preferred to executing commands immediately.
    fn enqueue(&self, command: impl Into<Command>) {
        self.as_ref().command_queue.lock().push_back(command.into());
    }

    /// Does all the frame things: executes commands, advances the simulation,
    /// etc.
    fn do_frame(&mut self, config: &Config) -> Result<()> {
        // Fetch result from worker thread.
        match self.as_mut().worker_thread.take_data() {
            Ok(WorkerData::None) => (),
            Ok(WorkerData::Progress(progress)) => match progress {
                WorkerProgressReport::NewValues(new_values) => {
                    // TODO: reduce the repetition here
                    if let Some(new_contents) = new_values.clipboard.clone() {
                        crate::clipboard_compat::clipboard_set(new_contents)?;
                    }
                    if self.as_ref().work_type == Some(WorkType::SimContinuous) {
                        self.as_mut().last_sim_times.push_back(new_values.elapsed);
                        if self.as_mut().last_sim_times.len() > MAX_LAST_SIM_TIMES {
                            self.as_mut().last_sim_times.pop_front();
                        }
                    }
                    self.set_new_values(new_values)?
                }
            },
            Ok(WorkerData::Result(new_values)) => {
                let new_values = new_values?;
                if let Some(new_contents) = new_values.clipboard.clone() {
                    crate::clipboard_compat::clipboard_set(new_contents)?;
                }
                self.set_new_values(new_values)?;
                self.as_mut().work_type = None;
            }
            Err(WorkerIdle) => (),
        }

        // Interpolate camera.
        let fps = self.fps(config);
        let interpolation = config.ctrl.interpolation;
        self.camera_interpolator().advance(fps, interpolation);

        // Execute commands.
        let old_command_queue =
            std::mem::replace(self.as_mut().command_queue.get_mut(), VecDeque::new());
        for command in old_command_queue {
            self.do_command(command, config)?;
        }

        // Trigger breakpoint.
        if self.is_running()
            && config.sim.use_breakpoint
            && self.generation_count() >= &config.sim.breakpoint_gen
        {
            self.stop_running(config);
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
    fn do_command(&mut self, command: impl Into<Command>, config: &Config) -> Result<()> {
        match command.into() {
            Command::Sim(c) => self.do_sim_command(c, config),
            Command::History(c) => self.do_history_command(c, config),
            Command::View(c) => self.do_view_command(c, config),
            Command::Draw(c) => self.do_draw_command(c, config),
            Command::Select(c) => self.do_select_command(c, config),
            Command::GarbageCollect => Ok(self.schedule_gc()),

            Command::ContinueDrag(cursor_pos) => self.continue_drag(cursor_pos),
            Command::StopDrag => self.stop_drag(),

            Command::Cancel => {
                if self.reset_worker_thread(config) {
                    Ok(())
                } else if self.is_drawing() {
                    self.do_command(DrawCommand::Cancel, config)
                } else {
                    self.do_command(SelectCommand::Cancel, config)
                }
            }
        }
    }
    /// Executes a `SimCommand`.
    fn do_sim_command(&mut self, command: SimCommand, config: &Config) -> Result<()> {
        match command {
            SimCommand::Step(step_size) => {
                self.try_step(config, step_size)?;
            }
            SimCommand::StepStepSize => {
                self.try_step(config, config.sim.step_size.clone())?;
            }

            SimCommand::StartRunning => {
                // If this fails (e.g. because the user is drawing), ignore the
                // error.
                let _ = self.start_running(config);
            }
            SimCommand::StopRunning => {
                self.stop_running(config);
            }
            SimCommand::ToggleRunning => {
                if self.is_running() {
                    self.stop_running(config);
                } else {
                    // If this fails, ignore the error.
                    let _ = self.start_running(config);
                }
            }

            SimCommand::UpdateStepSize => {
                if self.is_running() {
                    self.stop_running(config);
                    // This should not fail.
                    self.start_running(config)?;
                }
            }

            SimCommand::Cancel => {
                if let Some(work_type) = self.as_ref().work_type {
                    match work_type {
                        WorkType::SimStep | WorkType::SimContinuous => {
                            self.reset_worker_thread(config);
                        }
                    }
                }
            }
        }
        Ok(())
    }
    /// Executes a `HistoryCommand`.
    fn do_history_command(&mut self, command: HistoryCommand, config: &Config) -> Result<()> {
        if self.is_drawing() {
            trace!("Ignoring {:?} command while drawing", command);
            return Ok(());
        }
        match command {
            HistoryCommand::Undo => {
                self.reset_worker_thread(config);
                self.undo(config);
            }
            HistoryCommand::Redo => {
                if self.can_redo() {
                    self.reset_worker_thread(config);
                    self.redo(config);
                }
            }
            // TODO make this JumpTo instead of UndoTo
            HistoryCommand::UndoTo(gen) => {
                while self.generation_count() > &gen && self.can_undo() {
                    self.undo(config);
                }
            }
        }
        Ok(())
    }
    /// Executes a `Move` command.
    fn do_view_command(&mut self, command: ViewCommand, config: &Config) -> Result<()>;
    /// Executes a `Draw` command.
    fn do_draw_command(&mut self, command: DrawCommand, config: &Config) -> Result<()>;
    /// Executes a `Select` command.
    fn do_select_command(&mut self, command: SelectCommand, config: &Config) -> Result<()>;
    /// Executes a `ContinueDrag` command.
    fn continue_drag(&mut self, cursor_pos: FVec2D) -> Result<()>;
    /// Executes a `StopDrag` command.
    fn stop_drag(&mut self) -> Result<()>;

    /// Submits a request to the worker thread. Returns an error if the worker
    /// thread is not idle or is in an invalid state.
    fn do_on_worker_thread(&mut self, work_type: WorkType, work_fn: WorkFn) -> Result<()> {
        self.as_mut().work_type = Some(work_type);
        self.as_mut()
            .worker_thread
            .request(work_fn)
            .context("Attempted to send request to worker thread")
    }
    /// Cancels any long-running operation on the worker thread. Returns `true`
    /// if an operation was canceled, or `false` if it was not.
    fn reset_worker_thread(&mut self, config: &Config) -> bool {
        match self.as_mut().work_type.take() {
            Some(WorkType::SimStep) => {
                // Remove redundant history entry.
                self.undo(config);
            }
            Some(WorkType::SimContinuous) => {
                // Remove redundant history entry if zero generations were
                // simulated.
                let new_gens = self.automaton().generation_count().clone();
                self.undo(config);
                let old_gens = self.automaton().generation_count().clone();
                if old_gens == new_gens {
                    trace!("Removing redundant history entry from simulating zero generations");
                } else {
                    self.redo(config);
                }
            }
            None => (),
        }

        self.as_mut().worker_thread.reset()
    }
    /// Requests a one-off simulation from the worker thread. Returns `true` if
    /// successful, or false if another running operation prevented it (e.g.
    /// drawing or continuous simulation).
    fn try_step(&mut self, config: &Config, step_size: BigInt) -> Result<bool> {
        if self.is_drawing() {
            return Ok(false);
        }
        if self.is_running() {
            self.reset_worker_thread(config);
            return Ok(false);
        }
        self.reset_worker_thread(config);

        let mut automaton = self.automaton();
        self.do_on_worker_thread(
            WorkType::SimStep,
            Box::new(move |hook| {
                let start = Instant::now();
                automaton.step(&step_size);
                let end = Instant::now();
                Ok(NewGridViewValues {
                    elapsed: end - start,
                    automaton: Some(automaton),
                    ..Default::default()
                })
            }),
        )?;
        self.record();
        Ok(true)
    }
    /// Stops continuous simulation, if it is running; does nothing otherwise.
    fn stop_running(&mut self, config: &Config) {
        if self.is_running() {
            self.reset_worker_thread(config);
        }
    }
    /// Starts continuous simulation; does nothing if it is already running.
    /// Returns an error if unsuccessful (e.g. the user is drawing).
    fn start_running(&mut self, config: &Config) -> Result<()> {
        if self.is_running() {
            return Ok(());
        } else if self.is_drawing() {
            return Err(anyhow!("Cannot start simulation while drawing"));
        }

        self.reset_worker_thread(config);
        let mut automaton = self.automaton();
        let step_size = config.sim.step_size.clone();
        self.do_on_worker_thread(
            WorkType::SimContinuous,
            Box::new(move |hook| loop {
                if hook.wants_cancel() {
                    break Ok(NewGridViewValues::default());
                }

                let start = Instant::now();
                automaton.step(&step_size);
                let end = Instant::now();

                hook.progress_report_blocking(WorkerProgressReport::NewValues(NewGridViewValues {
                    elapsed: end - start,
                    automaton: Some(automaton.clone()),
                    ..Default::default()
                }));
            }),
        )?;
        self.record();
        Ok(())
    }
    /// Returns whether the simulation is running (i.e. stepping forward every
    /// frame automatically with no user input).
    fn is_running(&self) -> bool {
        self.as_ref().work_type == Some(WorkType::SimContinuous)
    }

    /// Sets new values for various fields.
    fn set_new_values(&mut self, new_values: NewGridViewValues) -> Result<()>;

    /// Exports the simulation to a string.
    fn export(&self, format: CaFormat) -> Result<String, CaFormatError>;

    /// Returns a copy of the underlying automaton.
    fn automaton(&self) -> Automaton;

    /// Returns whether the user is currently drawing.
    fn is_drawing(&self) -> bool {
        self.as_ref().is_drawing
    }
    /// Returns whether the user is currently dragging the viewport by holding
    /// down a mouse button.
    fn is_dragging_view(&self) -> bool {
        self.as_ref().is_dragging_view
    }
    /// Returns the type of operation currently happening in the background.
    fn work_type(&self) -> Option<WorkType> {
        self.as_ref().work_type
    }
    /// Returns the last several simulation times, with the most recent at the
    /// front.
    fn last_sim_times(&self) -> &VecDeque<Duration> {
        &self.as_ref().last_sim_times
    }

    /// Returns the selected cell state.
    fn selected_cell_state(&self) -> u8 {
        self.as_ref().selected_cell_state
    }

    /// Updates state relating to the mouse cursor.
    fn update_mouse_state(&mut self, new_state: MouseState) {
        self.as_mut().mouse = new_state;
    }
    /// Returns state relating to the mouse cursor.
    fn mouse(&self) -> MouseState {
        self.as_ref().mouse
    }

    /// Schedules garbage collection.
    fn schedule_gc(&mut self) {
        // TODO: allow garbage collection during continuous simulation

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
    /// Updates the pixel size of the viewport.
    fn update_target_dimensions(&mut self, target_dimensions: (u32, u32)) {
        self.camera_interpolator()
            .update_target_dimensions(target_dimensions);
    }
    /// Renders the gridview.
    fn render(&mut self, params: RenderParams<'_>) -> Result<&RenderResult>;
    /// Returns data generated by the most recent render.
    fn last_render_result(&self) -> &RenderResult {
        &self.as_ref().last_render_result
    }

    /// Returns the framerate measured between the last two frames, or the
    /// user-configured FPS if that fails.
    fn fps(&self, config: &Config) -> f64 {
        let last_frame_times = &self.as_ref().last_frame_times;
        last_frame_times
            .get(0)
            .zip(last_frame_times.get(1))
            .and_then(|(latest, &prior)| latest.checked_duration_since(prior))
            .and_then(|duration| R64::try_new(1.0 / duration.as_secs_f64()))
            .map(R64::raw)
            .unwrap_or(config.gfx.fps)
    }
}

/// Parameters that may control the rendering process.
pub struct RenderParams<'a> {
    /// Render target.
    pub target: &'a mut glium::Frame,
    /// User configuration.
    pub config: &'a Config,
    /// Key modifiers.
    pub modifiers: ModifiersState,
}

/// Extra data generated when rendering a frame.
#[derive(Debug, Default, Clone)]
pub struct RenderResult {
    /// Target under the mouse cursor, if any.
    pub mouse_target: Option<MouseTargetData>,
}

#[derive(Debug, Default, Clone)]
pub struct MouseTargetData {
    /// Mouse binding for clicking the left mouse button over the target and
    /// dragging.
    pub binding: Option<MouseDragBinding>,
    /// Display mode for the cursor when hovering over the target or clicking on
    /// it and dragging.
    pub display: MouseDisplay,
}

#[derive(Debug, Default, Copy, Clone)]
pub struct MouseState {
    /// Pixel position of the mouse cursor from the top left of the area where
    /// the gridview is being drawn.
    pub pos: Option<FVec2D>,

    /// Whether a mouse button is being held and dragged.
    pub dragging: bool,

    /// What to display for the mouse cursor.
    ///
    /// This determines the mouse cursor icon and how/whether to indicate the
    /// highlighted cell in the grid.
    pub display: MouseDisplay,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum WorkType {
    SimStep,
    SimContinuous,
}
