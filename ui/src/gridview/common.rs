use anyhow::{Context, Result};
use enum_dispatch::enum_dispatch;
use log::{debug, trace, warn};
use parking_lot::Mutex;
use std::collections::VecDeque;
use std::sync::mpsc;
use std::time::{Duration, Instant};

use ndcell_core::prelude::*;

use super::camera::Interpolate;
use super::history::History;
use super::worker::WorkerRequest;
use crate::commands::*;
use crate::config::Config;

/// The number of render results to remember.
const RENDER_RESULTS_COUNT: usize = 4;
/// The number of previous simulation steps to track for counting UPS.
const MAX_LAST_SIM_TIMES: usize = 4;

/// Shared fields common to all `GridView`s.
#[derive(Debug)]
pub struct GridViewCommon {
    /// Queue of pending commands to be executed on the next frame.
    pub command_queue: Mutex<Vec<Command>>,

    /// Size of the pixel rectangle to render to.
    pub last_target_size: (u32, u32),

    /// Whether the simulation is currently running.
    pub is_running: bool,
    /// Whether the user is currently drawing.
    pub is_drawing: bool,
    /// Whether the user is currently dragging the viewport by holding down a
    /// mouse button.
    pub is_dragging_view: bool,

    /// Whether a one-off simulation request is currently happening
    /// concurrently.
    ///
    /// TODO: reconsider is_waiting
    pub is_waiting: bool,

    /// Communication channel with the garbage collection thread.
    gc_channel: Option<mpsc::Receiver<()>>,

    /// The time that the last several frames completed.
    pub last_frame_times: VecDeque<Instant>,
    /// The last several simulation times, with the most recent at the front.
    pub last_sim_times: VecDeque<Duration>,
}
impl Default for GridViewCommon {
    fn default() -> Self {
        Self {
            command_queue: Default::default(),

            // Initialize to something nonzero.
            last_target_size: (100, 100),

            is_running: Default::default(),
            is_drawing: Default::default(),
            is_dragging_view: Default::default(),

            is_waiting: Default::default(),

            gc_channel: Default::default(),

            last_frame_times: Default::default(),
            last_sim_times: Default::default(),
        }
    }
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
            Command::Move(c) => self.do_move_command(c, config),
            Command::Draw(c) => self.do_draw_command(c, config),
            Command::Select(c) => self.do_select_command(c, config),
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
    fn do_history_command(&mut self, command: HistoryCommand, config: &Config) -> Result<()> {
        if self.is_drawing() {
            warn!("Ignoring {:?} command while drawing", command);
            return Ok(());
        }
        self.stop_running();
        match command {
            HistoryCommand::Undo => {
                self.undo(config);
            }
            HistoryCommand::Redo => {
                self.redo(config);
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
    fn do_move_command(&mut self, command: ViewCommand, config: &Config) -> Result<()> {
        // if !matches!(command, ViewCommand::SnapPos | ViewCommand::SnapScale {..}) {
        //     trace!("Executing {:?}", command);
        // }

        self.camera_interpolator()
            .do_move_command(command, config)
            .context("Executing move command")?;
        self.as_mut().is_dragging_view = self.camera_interpolator().is_dragging();
        Ok(())
    }
    /// Executes a `Draw` command.
    fn do_draw_command(&mut self, command: DrawCommand, config: &Config) -> Result<()>;
    /// Executes a `Select` command.
    fn do_select_command(&mut self, command: DragCommand<()>, config: &Config) -> Result<()>;
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
    /// Returns whether the user is currently dragging the viewport by holding
    /// down a mouse button.
    fn is_dragging_view(&self) -> bool {
        self.as_ref().is_dragging_view
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
    /// Updates the pixel size of the viewport.
    fn update_target_dimensions(&mut self, target_dimensions: (u32, u32)) {
        self.camera_interpolator()
            .update_target_dimensions(target_dimensions);
    }
    /// Returns the pixel position of the mouse cursor from the top left of the
    /// area where the gridview is being drawn.
    fn cursor_pos(&self) -> Option<FVec2D>;
    /// Renders the gridview.
    fn render(
        &mut self,
        config: &Config,
        target: &mut glium::Frame,
        params: RenderParams,
    ) -> Result<()>;

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
#[derive(Debug, Default)]
pub struct RenderParams {
    /// Pixel position of the mouse cursor from the top left of the area where
    /// the gridview is being drawn.
    pub cursor_pos: Option<FVec2D>,
}
