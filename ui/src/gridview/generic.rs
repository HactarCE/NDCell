use anyhow::{anyhow, Context, Result};
use glium::Surface;
use log::{debug, trace, warn};
use parking_lot::Mutex;
use std::collections::VecDeque;
use std::convert::TryInto;
use std::fmt;
use std::sync::mpsc;
use std::time::{Duration, Instant};

use ndcell_core::prelude::*;

use super::camera::{Camera, Interpolate, Interpolator};
use super::history::{History, HistoryBase, HistoryManager};
use super::render::{RenderParams, RenderResult};
use super::selection::Selection;
use super::worker::*;
use super::{DragHandler, DragOutcome, DragType, WorkType};
use crate::commands::*;
use crate::CONFIG;

/// Number of previous frame times to track. If this is too low, camera
/// interpolation may not work.
const MAX_LAST_FRAME_TIMES: usize = 2;
/// Number of previous simulation steps to track for counting simulation
/// steps per second.
const MAX_LAST_SIM_TIMES: usize = 4;

/// Dimension-generic interactive cellular automaton interface.
pub struct GenericGridView<G: GridViewDimension> {
    pub automaton: NdAutomaton<G::D>,
    pub selection: Option<Selection<G::D>>,
    pub camera_interpolator: Interpolator<G::D, G::Camera>,
    history: HistoryManager<HistoryEntry<G>>,
    dimensionality: G,

    /// Queue of pending commands to be executed on the next frame.
    command_queue: Mutex<VecDeque<Command>>,
    /// Thread to offload long-running computations onto.
    worker_thread: WorkerThread,
    /// What kind of work the worker thread is doing right now.
    work_type: Option<WorkType>,

    /// Communication channel with the garbage collection thread.
    gc_channel: Option<mpsc::Receiver<()>>,

    /// Mouse drag handler.
    drag_handler: Option<DragHandler<Self>>,
    /// Type of mouse drag being handled.
    drag_type: Option<DragType>,

    /// Time that the last several frames completed.
    last_frame_times: VecDeque<Instant>,
    /// Last several simulation times, with the most recent at the front.
    last_sim_times: VecDeque<Duration>,
    /// Most recent render result.
    last_render_result: RenderResult,

    /// Selected cell state.
    pub selected_cell_state: u8,
}
impl<G: GridViewDimension> From<NdAutomaton<G::D>> for GenericGridView<G> {
    fn from(automaton: NdAutomaton<G::D>) -> Self {
        Self {
            automaton,
            ..Default::default()
        }
    }
}
impl<G: GridViewDimension> Default for GenericGridView<G> {
    fn default() -> Self {
        Self {
            automaton: Default::default(),
            selection: Default::default(),
            camera_interpolator: Default::default(),
            history: Default::default(),
            dimensionality: Default::default(),

            command_queue: Default::default(),
            worker_thread: Default::default(),
            work_type: Default::default(),

            gc_channel: Default::default(),

            drag_handler: Default::default(),
            drag_type: Default::default(),

            last_frame_times: Default::default(),
            last_sim_times: Default::default(),
            last_render_result: Default::default(),

            // ... all that for one non-default attribute
            selected_cell_state: 1_u8,
        }
    }
}
impl<G: GridViewDimension> AsSimulate for GenericGridView<G> {
    fn as_sim(&self) -> &dyn Simulate {
        &self.automaton
    }
    fn as_sim_mut(&mut self) -> &mut dyn Simulate {
        &mut self.automaton
    }
}
impl<G: GridViewDimension> HistoryBase for GenericGridView<G> {
    type Entry = HistoryEntry<G>;

    fn history_entry(&self) -> Self::Entry {
        HistoryEntry {
            automaton: self.automaton.clone(),
            selection: self.selection.clone(),
            camera: self.camera_interpolator.target.clone(),
        }
    }

    fn restore_history_entry(&mut self, entry: Self::Entry) -> Self::Entry {
        HistoryEntry {
            automaton: std::mem::replace(&mut self.automaton, entry.automaton),
            selection: Selection::restore_history_entry(&mut self.selection, entry.selection),
            camera: if CONFIG.lock().hist.record_view {
                std::mem::replace(&mut self.camera_interpolator.target, entry.camera)
            } else {
                self.camera_interpolator.target.clone()
            },
        }
    }

    fn as_history(&self) -> &HistoryManager<Self::Entry> {
        &self.history
    }
    fn as_history_mut(&mut self) -> &mut HistoryManager<Self::Entry> {
        &mut self.history
    }
}
impl<G: GridViewDimension> GenericGridView<G> {
    /// Enqueues a command to be executed on the next frame.
    ///
    /// This should be preferred to executing commands immediately.
    pub fn enqueue(&self, command: impl Into<Command>) {
        self.command_queue.lock().push_back(command.into());
    }

    /// Does all the frame things: executes commands, advances the simulation,
    /// etc.
    pub fn do_frame(&mut self) -> Result<()> {
        // Update frame times.
        self.last_frame_times.push_front(Instant::now());
        if self.last_frame_times.len() > MAX_LAST_FRAME_TIMES {
            self.last_frame_times.pop_back();
        }

        // Fetch result from worker thread.
        if let Some(work_type) = self.work_type {
            match self.worker_thread.take_data() {
                Ok(WorkerData::None) => (),
                Ok(WorkerData::Progress(progress)) => match progress {
                    WorkerProgressReport::NewValues(new_values) => {
                        self.set_new_values(work_type, new_values)?;
                    }
                },
                Ok(WorkerData::Result(new_values)) => {
                    self.set_new_values(work_type, new_values?)?;
                    self.work_type = None;
                }
                Err(WorkerIdle) => return Err(anyhow!("Worker is idle but work type is not None")),
            }
        }

        // Interpolate camera.
        if let Some(elapsed) = self.frame_duration() {
            self.camera_interpolator
                .advance(elapsed, CONFIG.lock().ctrl.interpolation);
        }

        // Execute commands.
        let old_command_queue = std::mem::replace(self.command_queue.get_mut(), VecDeque::new());
        for command in old_command_queue {
            self.do_command(command)?;
        }

        // Trigger breakpoint.
        if self.is_running()
            && CONFIG.lock().sim.use_breakpoint
            && self.generation_count() >= &CONFIG.lock().sim.breakpoint_gen
        {
            self.stop_running();
        }

        // Collect garbage if memory usage has gotten too high.
        if !self.gc_in_progress() {
            if self.as_sim().memory_usage() > CONFIG.lock().sim.max_memory {
                trace!(
                    "Memory usage reached {} bytes; max is {}",
                    self.as_sim().memory_usage(),
                    CONFIG.lock().sim.max_memory
                );
                self.schedule_gc();
            }
        }

        Ok(())
    }
    /// Executes a `Command`.
    pub(super) fn do_command(&mut self, command: impl Into<Command>) -> Result<()> {
        match command.into() {
            Command::Sim(c) => self.do_sim_command(c),
            Command::History(c) => self.do_history_command(c),
            Command::View(c) => self.do_view_command(c),
            Command::Draw(c) => self.do_draw_command(c),
            Command::Select(c) => self.do_select_command(c),
            Command::GarbageCollect => Ok(self.schedule_gc()),

            Command::ContinueDrag(cursor_pos) => self.continue_drag(cursor_pos),
            Command::StopDrag => Ok(self.stop_drag()),

            Command::Cancel => {
                if self.reset_worker_thread() {
                    Ok(())
                } else if self.is_drawing() {
                    self.do_command(DrawCommand::Cancel)
                } else {
                    self.do_command(SelectCommand::Cancel)
                }
            }
        }
    }
    /// Executes a `SimCommand`.
    fn do_sim_command(&mut self, command: SimCommand) -> Result<()> {
        match command {
            SimCommand::Step(step_size) => {
                self.try_step(step_size)?;
            }
            SimCommand::StepStepSize => {
                self.try_step(CONFIG.lock().sim.step_size.clone())?;
            }

            SimCommand::StartRunning => {
                // If this fails (e.g. because the user is drawing), ignore the
                // error.
                let _ = self.start_running();
            }
            SimCommand::StopRunning => {
                self.stop_running();
            }
            SimCommand::ToggleRunning => {
                if self.is_running() {
                    self.stop_running();
                } else {
                    // If this fails, ignore the error.
                    let _ = self.start_running();
                }
            }

            SimCommand::UpdateStepSize => {
                if self.is_running() {
                    self.stop_running();
                    // This should not fail.
                    self.start_running()?;
                }
            }

            SimCommand::Cancel => {
                if let Some(work_type) = self.work_type {
                    match work_type {
                        WorkType::SimStep | WorkType::SimContinuous => {
                            self.reset_worker_thread();
                        }
                    }
                }
            }
        }
        Ok(())
    }
    /// Executes a `HistoryCommand`.
    fn do_history_command(&mut self, command: HistoryCommand) -> Result<()> {
        if self.is_drawing() {
            trace!("Ignoring {:?} command while drawing", command);
            return Ok(());
        }
        match command {
            HistoryCommand::Undo => {
                self.reset_worker_thread();
                self.undo();
            }
            HistoryCommand::Redo => {
                if self.can_redo() {
                    self.reset_worker_thread();
                    self.redo();
                }
            }
            // TODO make this JumpTo instead of UndoTo
            HistoryCommand::UndoTo(gen) => {
                self.reset_worker_thread();
                while self.generation_count() > &gen && self.can_undo() {
                    self.undo();
                }
            }
        }
        Ok(())
    }
    /// Executes a `View` command.
    fn do_view_command(&mut self, command: ViewCommand) -> Result<()> {
        // `View` commands depend on the number of dimensions.
        G::do_view_command(self, command)
    }
    /// Executes a `Draw` command.
    fn do_draw_command(&mut self, command: DrawCommand) -> Result<()> {
        // `Draw` commands depend on the number of dimensions.
        G::do_draw_command(self, command)
    }
    /// Executes a `Select` command.
    fn do_select_command(&mut self, command: SelectCommand) -> Result<()> {
        // `Select` commands depend on the number of dimensions.
        G::do_select_command(self, command)
    }

    /// Starts a drag event.
    ///
    /// Do not call this method from within a drag handler.
    pub(super) fn start_drag(&mut self, drag_type: DragType, drag_handler: DragHandler<Self>) {
        self.drag_type = Some(drag_type);
        self.drag_handler = Some(drag_handler);
    }
    /// Executes a `ContinueDrag` command, calling the drag handler.
    ///
    /// Do not call this method from within a drag handler.
    pub(super) fn continue_drag(&mut self, cursor_pos: FVec2D) -> Result<()> {
        if let Some(mut drag_handler) = self.drag_handler.take() {
            match drag_handler(self, cursor_pos)? {
                DragOutcome::Continue => self.drag_handler = Some(drag_handler),
                DragOutcome::Cancel => self.stop_drag(),
            }
        }
        Ok(())
    }
    /// Executes a `StopDrag` command.
    ///
    /// Do not call this method from within a drag handler; instead return
    /// `DragOutcome::Cancel`.
    pub(super) fn stop_drag(&mut self) {
        self.drag_type = None;
        self.drag_handler = None;
    }

    /// Submits a request to the worker thread. Returns an error if the worker
    /// thread is not idle or is in an invalid state.
    pub(super) fn do_on_worker_thread(
        &mut self,
        work_type: WorkType,
        work_fn: WorkFn,
    ) -> Result<()> {
        self.work_type = Some(work_type);
        self.worker_thread
            .request(work_fn)
            .context("Attempted to send request to worker thread")
    }
    /// Cancels any long-running operation on the worker thread. Returns `true`
    /// if an operation was canceled, or `false` if it was not.
    pub(super) fn reset_worker_thread(&mut self) -> bool {
        match self.work_type.take() {
            Some(WorkType::SimStep) => {
                // Remove redundant history entry.
                self.undo();
            }
            Some(WorkType::SimContinuous) => {
                // Remove redundant history entry if zero generations were
                // simulated.
                let new_gens = self.automaton.generation_count().clone();
                self.undo();
                let old_gens = self.automaton.generation_count().clone();
                if old_gens == new_gens {
                    trace!("Removing redundant history entry from simulating zero generations");
                } else {
                    self.redo();
                }
            }
            None => (),
        }

        self.worker_thread.reset()
    }
    /// Requests a one-off simulation from the worker thread. Returns `true` if
    /// successful, or false if another running operation prevented it (e.g.
    /// drawing or continuous simulation).
    fn try_step(&mut self, step_size: BigInt) -> Result<bool> {
        if self.is_drawing() {
            return Ok(false);
        }
        if self.is_running() {
            self.reset_worker_thread();
            return Ok(false);
        }
        self.reset_worker_thread();

        let mut automaton: Automaton = self.automaton.clone().into();
        self.do_on_worker_thread(
            WorkType::SimStep,
            Box::new(move |_hook| {
                let start = Instant::now();
                automaton.step(&step_size); // TODO: pass in `hook`
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
    fn stop_running(&mut self) {
        if self.is_running() {
            self.reset_worker_thread();
        }
    }
    /// Starts continuous simulation; does nothing if it is already running.
    /// Returns an error if unsuccessful (e.g. the user is drawing).
    fn start_running(&mut self) -> Result<()> {
        if self.is_running() {
            return Ok(());
        } else if self.is_drawing() {
            return Err(anyhow!("Cannot start simulation while drawing"));
        }

        self.reset_worker_thread();
        let mut automaton: Automaton = self.automaton.clone().into();
        let step_size = CONFIG.lock().sim.step_size.clone();
        self.do_on_worker_thread(
            WorkType::SimContinuous,
            Box::new(move |hook| loop {
                if hook.wants_cancel() {
                    break Ok(NewGridViewValues::default());
                }

                let start = Instant::now();
                automaton.step(&step_size); // TODO: pass in `hook`
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
    pub fn is_running(&self) -> bool {
        self.work_type == Some(WorkType::SimContinuous)
    }

    /// Sets new values for various fields.
    fn set_new_values(&mut self, work_type: WorkType, new_values: NewGridViewValues) -> Result<()> {
        let NewGridViewValues {
            elapsed,

            clipboard,
            automaton,
            selection_2d,
            selection_3d,
        } = new_values;

        match work_type {
            WorkType::SimStep | WorkType::SimContinuous => {
                self.last_sim_times.push_back(elapsed);
                if self.last_sim_times.len() > MAX_LAST_SIM_TIMES {
                    self.last_sim_times.pop_front();
                }
            }
        }

        if let Some(new_contents) = clipboard {
            crate::clipboard_compat::clipboard_set(new_contents)?;
        }

        if let Some(a) = automaton {
            self.automaton = a.try_into().map_err(|_| {
                anyhow!("set_new_values() received Automaton of wrong dimensionality")
            })?;
        }

        if let Some(new_selection) = selection_2d {
            self.set_selection_nd(new_selection)?;
        }
        if let Some(new_selection) = selection_3d {
            self.set_selection_nd(new_selection)?;
        }

        Ok(())
    }

    /// Exports the simulation to a string.
    pub fn export(&self, format: CaFormat) -> Result<String, CaFormatError> {
        let ndtree = self
            .selection
            .as_ref()
            .and_then(|sel| sel.cells.as_ref())
            .unwrap_or(&self.automaton.ndtree);
        let two_states = TwoState::from_rule(&*self.automaton.rule);
        let rect = self.selection_rect();
        ndcell_core::io::export_ndtree_to_string(ndtree, format, two_states, rect)
    }

    /// Sets the selection, or returns an error if the dimensionality of the
    /// selection does not match the dimensionality of the gridview.
    fn set_selection_nd<D: Dim>(&mut self, new_selection: Option<Selection<D>>) -> Result<()> {
        if G::D::NDIM == D::NDIM {
            self.set_selection(unsafe {
                *std::mem::transmute::<Box<Option<Selection<D>>>, Box<Option<Selection<G::D>>>>(
                    Box::new(new_selection),
                )
            });
            Ok(())
        } else {
            Err(anyhow!(
                "set_selection_nd() received Selection of wrong dimensionality"
            ))
        }
    }
    /// Deselects and sets a new selection.
    pub(super) fn set_selection(&mut self, new_selection: Option<Selection<G::D>>) {
        self.deselect();
        self.selection = new_selection;
    }
    /// Deselects and sets a new selection rectangle.
    pub(super) fn set_selection_rect(&mut self, new_selection_rect: Option<BigRect<G::D>>) {
        self.set_selection(new_selection_rect.map(Selection::from))
    }
    /// Returns the selection rectangle.
    pub(super) fn selection_rect(&self) -> Option<BigRect<G::D>> {
        if let Some(s) = &self.selection {
            Some(s.rect.clone())
        } else {
            None
        }
    }
    /// Deselects all and returns the old selection.
    pub(super) fn deselect(&mut self) -> Option<Selection<G::D>> {
        if let Some(sel) = self.selection.clone() {
            if let Some(cells) = sel.cells {
                self.record();
                // Overwrite.
                self.automaton.ndtree.paste_custom(
                    cells,
                    Region::Rect(sel.rect),
                    |dest, src| {
                        if dest.is_empty() {
                            Some(src)
                        } else if src.is_empty() {
                            Some(dest)
                        } else {
                            None
                        }
                    },
                    |dest, src| if src == 0_u8 { dest } else { src },
                );
            }
        }
        self.selection.take()
    }
    /// Moves the selected cells from the selection to the main grid. Outputs a
    /// warning in the log if there is no selection.
    pub(super) fn drop_selected_cells(&mut self) {
        if let Some(sel) = self.deselect() {
            self.set_selection_rect(Some(sel.rect));
        } else {
            warn!("drop_selected_cells() called with no selection");
        }
    }
    /// Moves the cells in the selected region from the main grid into the
    /// selection. Outputs a warning to the log if there is no selection. Does
    /// nothing if the selection already contains cells.
    pub(super) fn grab_selected_cells(&mut self) {
        self._grab_selected_cells(true)
    }
    /// Copies the cells in the selected region from the main grid into the
    /// selection. Outputs a warning to the log if there is no selection. Does
    /// nothing if the selection already contains cells.
    pub(super) fn grab_copy_of_selected_cells(&mut self) {
        self._grab_selected_cells(false)
    }
    fn _grab_selected_cells(&mut self, clear_source: bool) {
        if let Some(sel) = &mut self.selection {
            if sel.cells.is_none() {
                let region = Region::Rect(sel.rect.clone());
                sel.cells = Some(self.automaton.ndtree.get_region(region.clone()));
                if clear_source {
                    self.automaton.ndtree.clear_region(region);
                }
            }
        } else {
            warn!("grab_selected_cells() called with no selection");
        }
    }

    /// Returns whether the user is currently drawing.
    pub fn is_drawing(&self) -> bool {
        self.drag_type == Some(DragType::Drawing)
    }
    /// Returns whether the user is currently moving the viewport by dragging
    /// the mouse.
    pub fn is_dragging_view(&self) -> bool {
        self.drag_type == Some(DragType::MovingView)
    }
    /// Returns whether the user is currently dragging the mouse.
    pub fn is_dragging(&self) -> bool {
        self.drag_type.is_some()
    }

    /// Returns the type of operation currently happening in the background.
    pub fn work_type(&self) -> Option<WorkType> {
        self.work_type
    }
    /// Returns the last several simulation times, with the most recent at the
    /// front.
    pub fn last_sim_times(&self) -> &VecDeque<Duration> {
        &self.last_sim_times
    }

    /// Returns the selected cell state.
    fn selected_cell_state(&self) -> u8 {
        self.selected_cell_state
    }

    /// Schedules garbage collection.
    fn schedule_gc(&mut self) {
        // TODO: allow garbage collection during continuous simulation

        let old_memory_usage = self.as_sim().memory_usage();
        let (tx, rx) = mpsc::channel();
        self.gc_channel = Some(rx);
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
        if let Some(chan) = &self.gc_channel {
            match chan.try_recv() {
                Ok(_) => {
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

    /// Returns the current camera.
    pub fn camera(&self) -> &G::Camera {
        &self.camera_interpolator.current
    }
    /// Updates camera parameters and renders the gridview, recording and
    /// returning the result.
    pub fn render(&mut self, params: RenderParams<'_>) -> Result<&RenderResult> {
        // Update DPI.
        self.camera_interpolator
            .set_dpi(CONFIG.lock().gfx.dpi as f32);
        // Update the pixel size of the viewport.
        self.camera_interpolator
            .set_target_dimensions(params.target.get_dimensions());

        // Render the grid to the viewport, then save and return the result.
        self.last_render_result = G::render(self, params)?;
        Ok(self.last_render_result())
    }
    /// Returns data generated by the most recent render.
    pub fn last_render_result(&self) -> &RenderResult {
        &self.last_render_result
    }

    /// Returns the time duration measured between the last two frames, or
    /// `None` if there is not enough data.
    pub fn frame_duration(&self) -> Option<Duration> {
        let last_frame_times = &self.last_frame_times;
        last_frame_times
            .get(0)
            .zip(last_frame_times.get(1))
            .and_then(|(&latest, &prior)| latest.checked_duration_since(prior))
    }
}

pub trait GridViewDimension: fmt::Debug + Default {
    type D: Dim;
    type Camera: Camera<Self::D>;

    /// Executes a `View` command.
    fn do_view_command(this: &mut GenericGridView<Self>, command: ViewCommand) -> Result<()>;
    /// Executes a `Draw` command.
    fn do_draw_command(this: &mut GenericGridView<Self>, command: DrawCommand) -> Result<()>;
    /// Executes a `Select` command.
    fn do_select_command(this: &mut GenericGridView<Self>, command: SelectCommand) -> Result<()>;

    /// Renders the gridview.
    fn render(this: &mut GenericGridView<Self>, params: RenderParams<'_>) -> Result<RenderResult>;
}

#[derive(Debug, Clone)]
pub struct HistoryEntry<G: GridViewDimension> {
    automaton: NdAutomaton<G::D>,
    selection: Option<Selection<G::D>>,
    camera: G::Camera,
}
