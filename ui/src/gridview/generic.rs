use anyhow::{anyhow, bail, ensure, Context, Result};
use glium::Surface;
use log::{debug, trace, warn};
use parking_lot::Mutex;
use std::collections::VecDeque;
use std::convert::TryInto;
use std::fmt;
use std::sync::mpsc;
use std::time::{Duration, Instant};

use ndcell_core::prelude::*;

use super::algorithms::bresenham;
use super::history::{History, HistoryBase, HistoryManager};
use super::render::{RenderParams, RenderResult};
use super::screenpos::ScreenPos;
use super::selection::Selection;
use super::viewpoint::{Interpolate, Interpolator, Viewpoint};
use super::worker::*;
use super::{DragHandler, DragOutcome, DragType, WorkType};
use crate::commands::*;
use crate::{Face, CONFIG};

/// Number of previous frame times to track. If this is too low, viewpoint
/// interpolation may not work.
const MAX_LAST_FRAME_TIMES: usize = 2;
/// Number of previous simulation steps to track for counting simulation
/// steps per second.
const MAX_LAST_SIM_TIMES: usize = 4;

pub type SelectionDragHandler<G> = Box<
    dyn FnMut(&mut GenericGridView<G>, <G as GridViewDimension>::ScreenPos) -> Result<DragOutcome>,
>;

macro_rules! ignore_command {
    ($c:expr) => {{
        warn!("Ignoring {:?} in GridView{}D", $c, G::D::NDIM);
        return None;
    }};
}

/// Dimension-generic interactive cellular automaton interface.
pub struct GenericGridView<G: GridViewDimension> {
    pub automaton: NdAutomaton<G::D>,
    pub selection: Option<Selection<G::D>>,
    pub viewpoint_interpolator: Interpolator<G::D, G::Viewpoint>,
    history: HistoryManager<HistoryEntry<G>>,
    /// Dimension-specific data.
    pub(super) dim: G,

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
    /// Initial screen position of mouse drag.
    pub(super) drag_initial: Option<G::ScreenPos>,

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
            viewpoint_interpolator: Default::default(),
            history: Default::default(),
            dim: Default::default(),

            command_queue: Default::default(),
            worker_thread: Default::default(),
            work_type: Default::default(),

            gc_channel: Default::default(),

            drag_handler: Default::default(),
            drag_type: Default::default(),
            drag_initial: Default::default(),

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
            viewpoint: self.viewpoint_interpolator.target.clone(),
        }
    }

    fn restore_history_entry(&mut self, entry: Self::Entry) -> Self::Entry {
        HistoryEntry {
            automaton: std::mem::replace(&mut self.automaton, entry.automaton),
            selection: Selection::restore_history_entry(&mut self.selection, entry.selection),
            viewpoint: if CONFIG.lock().hist.record_view {
                std::mem::replace(&mut self.viewpoint_interpolator.target, entry.viewpoint)
            } else {
                self.viewpoint_interpolator.target.clone()
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
                Err(WorkerIdle) => bail!("Worker is idle but work type is not None"),
            }
        }

        // Interpolate viewpoint.
        if let Some(elapsed) = self.frame_duration() {
            self.viewpoint_interpolator
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
        match command {
            DrawCommand::SetState(new_selected_cell_state) => {
                self.selected_cell_state = new_selected_cell_state;
            }
            DrawCommand::Drag(c, cursor_start) => {
                let initial_screen_pos = self.screen_pos(cursor_start);
                let initial_cell = match initial_screen_pos.cell_to_draw(c.mode) {
                    Some(cell) => cell,
                    None => return Ok(()),
                };

                let new_cell_state = c.mode.cell_state(
                    self.automaton.ndtree.get_cell(&initial_cell),
                    self.selected_cell_state,
                );

                let new_drag_handler: DragHandler<Self> = match c.shape {
                    DrawShape::Freeform => {
                        let mut pos1 = initial_cell;
                        Box::new(move |this, pixel| {
                            if let Some(pos2) = this
                                .screen_pos(pixel)
                                .cell_to_draw_starting_at(c.mode, &initial_screen_pos)
                            {
                                for pos in bresenham::line(pos1.clone(), pos2.clone()) {
                                    this.automaton.ndtree.set_cell(&pos, new_cell_state);
                                }
                                pos1 = pos2;
                            }
                            Ok(DragOutcome::Continue)
                        })
                    }
                    DrawShape::Line => {
                        // TODO: implement drawing straight line
                        warn!("Line drawing is not yet implemented!");
                        return Ok(());
                    }
                };

                self.reset_worker_thread();
                self.record();
                self.start_drag(DragType::Drawing, new_drag_handler, Some(cursor_start));
            }
            DrawCommand::Confirm => {
                if self.is_drawing() {
                    self.stop_drag();
                }
            }
            DrawCommand::Cancel => {
                if self.is_drawing() {
                    self.stop_drag();
                    self.undo();
                }
            }
        }
        Ok(())
    }
    /// Executes a `Select` command.
    fn do_select_command(&mut self, command: SelectCommand) -> Result<()> {
        match command {
            SelectCommand::Drag(c, cursor_start) => {
                let initial_pos = self.screen_pos(cursor_start);
                // Drag handlers depend on the number of dimensions.
                let new_drag_handler = match self.make_select_drag_handler(c, initial_pos) {
                    Some(h) => h,
                    None => return Ok(()),
                };
                let new_drag_handler_with_threshold: DragHandler<Self> =
                    make_selection_drag_handler_with_threshold(
                        cursor_start,
                        c.uses_drag_threshold(),
                        new_drag_handler,
                    );
                if CONFIG.lock().hist.should_record_select_drag_command(c) {
                    self.reset_worker_thread();
                    self.record();
                }

                if let SelectDragCommand::NewRect = c {
                    // Deselect immediately; don't wait for drag threshold.
                    self.deselect();
                }

                self.start_drag(
                    DragType::Selecting,
                    new_drag_handler_with_threshold,
                    Some(cursor_start),
                );
            }

            SelectCommand::SelectAll => {
                self.deselect(); // take into account pasted cells
                self.set_selection(self.automaton.ndtree.bounding_rect().map(Selection::from))
            }
            SelectCommand::Deselect => {
                self.deselect();
            }

            SelectCommand::Copy(format) => {
                if self.selection.is_some() {
                    let result = self.export(format);
                    match result {
                        Ok(s) => crate::clipboard_compat::clipboard_set(s)?,
                        Err(msg) => warn!("Failed to generate {}: {}", format, msg),
                    }
                }
            }
            SelectCommand::Paste => {
                let old_sel_rect = self.selection_rect();

                self.record();
                let string_from_clipboard = crate::clipboard_compat::clipboard_get()?;
                let result =
                    Selection::from_str(&string_from_clipboard, self.automaton.ndtree.pool());
                match result {
                    Ok(sel) => {
                        self.set_selection(sel);
                        self.ensure_selection_visible();

                        // If selection size is the same, preserve position.
                        if let Some((old_rect, new_sel)) = old_sel_rect.zip(self.selection.as_mut())
                        {
                            if old_rect.size() == new_sel.rect.size() {
                                *new_sel = new_sel.move_by(old_rect.min() - new_sel.rect.min());
                            }
                        }
                    }
                    Err(errors) => warn!("Failed to load pattern: {:?}", errors),
                }
            }
            SelectCommand::Delete => {
                if self.selection.is_some() {
                    self.record();
                    self.selection.as_mut().unwrap().cells = None;
                    self.grab_selected_cells();
                    self.selection.as_mut().unwrap().cells = None;
                }
            }

            SelectCommand::Cancel => {
                if let Some(sel) = &self.selection {
                    if sel.cells.is_some() {
                        self.drop_selected_cells();
                    } else {
                        self.deselect();
                    }
                }
            }
        }
        Ok(())
    }

    /// Returns a selection drag handler for a selection command, given the
    /// starting screen position.
    fn make_select_drag_handler(
        &self,
        command: SelectDragCommand,
        initial_pos: G::ScreenPos,
    ) -> Option<SelectionDragHandler<G>> {
        match command {
            SelectDragCommand::NewRect => {
                let resize_start = initial_pos.render_cell_to_select()?;
                Some(Box::new(move |this, new_pos| {
                    if let Some(resize_destination) =
                        new_pos.render_cell_to_select_starting_at(&initial_pos)
                    {
                        this.set_selection_rect(Some(NdRect::span_rects(
                            &resize_start,
                            &resize_destination,
                        )));
                    }
                    Ok(DragOutcome::Continue)
                }))
            }

            SelectDragCommand::Resize2D(direction) => {
                if G::D::NDIM != 2 {
                    ignore_command!(command);
                }
                self.make_selection_resize_drag_handler(initial_pos, direction.vector())
            }

            SelectDragCommand::Resize3D(face) => {
                if G::D::NDIM != 3 {
                    ignore_command!(command);
                }
                self.make_selection_resize_drag_handler(initial_pos, face.normal_ivec())
            }

            SelectDragCommand::ResizeToCell => {
                let mut initial_selection = None;
                let resize_start = initial_pos.absolute_selection_resize_start_pos()?;
                Some(Box::new(move |this, new_pos| {
                    initial_selection = initial_selection.take().or_else(|| this.deselect());
                    if let Some(s) = &initial_selection {
                        if let Some(resize_destination) =
                            new_pos.render_cell_to_select_starting_at(&initial_pos)
                        {
                            this.set_selection_rect(Some(
                                super::selection::resize_selection_absolute(
                                    &s.rect,
                                    &resize_start,
                                    &resize_destination,
                                ),
                            ));
                        }
                        Ok(DragOutcome::Continue)
                    } else {
                        // There is no selection to resize.
                        Ok(DragOutcome::Cancel)
                    }
                }))
            }

            SelectDragCommand::MoveSelection(face) => {
                self.make_selection_move_drag_handler(face, initial_pos, |this| {
                    // To move the selection, we must first drop the selected
                    // cells.
                    this.drop_selected_cells();
                })
            }

            SelectDragCommand::MoveCells(face) => {
                self.make_selection_move_drag_handler(face, initial_pos, |this| {
                    // To move the selected cells, we must first grab those
                    // cells.
                    this.grab_selected_cells();
                })
            }

            SelectDragCommand::CopyCells(face) => {
                self.make_selection_move_drag_handler(face, initial_pos, |this| {
                    // To copy the selected cells, we must first grab a copy of
                    // those cells.
                    this.drop_selected_cells();
                    this.grab_copy_of_selected_cells();
                })
            }
        }
    }
    fn make_selection_resize_drag_handler<D: Dim>(
        &self,
        initial_pos: G::ScreenPos,
        resize_vector: IVec<D>,
    ) -> Option<SelectionDragHandler<G>> {
        // Attempt to cast `resize_vector` to the correct number of dimensions.
        let resize_vector: IVec<G::D> = AnyDimIVec::from(resize_vector).try_into().ok()?;

        let mut initial_selection = None;
        Some(Box::new(move |this, new_pos| {
            initial_selection = initial_selection.take().or_else(|| this.deselect());
            if let Some(s) = &initial_selection {
                if let Some(resize_delta) =
                    new_pos.rect_resize_delta(&s.rect.to_fixedrect(), &initial_pos, &resize_vector)
                {
                    this.set_selection_rect(Some(super::selection::resize_selection_relative(
                        &s.rect,
                        &resize_delta,
                        &resize_vector,
                    )));
                }
                Ok(DragOutcome::Continue)
            } else {
                // There is no selection to resize.
                Ok(DragOutcome::Cancel)
            }
        }))
    }
    fn make_selection_move_drag_handler(
        &self,
        face: Option<Face>,
        initial_pos: G::ScreenPos,
        selection_setup_fn: impl 'static + Fn(&mut Self),
    ) -> Option<SelectionDragHandler<G>> {
        let mut initial_selection = None;
        Some(Box::new(move |this, new_pos| {
            initial_selection = initial_selection.take().or_else(|| {
                selection_setup_fn(this);
                this.selection.take()
            });
            if let Some(s) = &initial_selection {
                if let Some(delta) =
                    new_pos.rect_move_delta(&s.rect.to_fixedrect(), &initial_pos, face)
                {
                    this.selection = Some(s.move_by(delta.round()));
                }
                Ok(DragOutcome::Continue)
            } else {
                // There is no selection to move.
                Ok(DragOutcome::Cancel)
            }
        }))
    }

    /// Starts a drag event.
    ///
    /// Do not call this method from within a drag handler.
    pub(super) fn start_drag(
        &mut self,
        drag_type: DragType,
        drag_handler: DragHandler<Self>,
        pixel: Option<FVec2D>,
    ) {
        self.drag_type = Some(drag_type);
        self.drag_handler = Some(drag_handler);
        self.drag_initial = pixel.map(|p| self.screen_pos(p));
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
        self.drag_initial = None;
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
            bail!("Cannot start simulation while drawing");
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
        ensure!(
            G::D::NDIM == D::NDIM,
            "set_selection_nd() received Selection of wrong dimensionality",
        );

        self.set_selection(unsafe {
            *std::mem::transmute::<Box<Option<Selection<D>>>, Box<Option<Selection<G::D>>>>(
                Box::new(new_selection),
            )
        });
        Ok(())
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
    /// Moves the selection to the center of the screen along each axis for
    /// which it is outside the viewport.
    fn ensure_selection_visible(&mut self) {
        if let Some(mut sel) = self.selection.take() {
            // The number of render cells of padding to ensure.
            const PADDING: usize = 2;

            let render_cell_layer = self.viewpoint().render_cell_layer();

            // Convert to render cells.
            let sel_rect = sel.rect.div_outward(&render_cell_layer.big_len());
            let visible_rect = self
                .viewpoint()
                .global_visible_rect()
                .div_outward(&render_cell_layer.big_len());

            let sel_min = sel_rect.min();
            let sel_max = sel_rect.max();
            let sel_center = sel_rect.center();
            let visible_min = visible_rect.min();
            let visible_max = visible_rect.max();
            let view_center = self.viewpoint().center().floor();

            for &ax in Dim2D::axes() {
                if sel_max[ax] < visible_min[ax].clone() + PADDING
                    || visible_max[ax] < sel_min[ax].clone() + PADDING
                {
                    // Move selection to center along this axis.
                    sel =
                        sel.move_by(NdVec::unit(ax) * (view_center[ax].clone() - &sel_center[ax]));
                }
            }

            self.set_selection(Some(sel));
        }
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
    /// Returns whether the user is currently moving the viewpoint by dragging
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

    /// Returns the current viewpoint.
    pub fn viewpoint(&self) -> &G::Viewpoint {
        &self.viewpoint_interpolator.current
    }
    /// Updates viewpoint parameters and renders the gridview, recording and
    /// returning the result.
    pub fn render(&mut self, params: RenderParams<'_>) -> Result<&RenderResult> {
        // Update DPI.
        self.viewpoint_interpolator
            .set_dpi(CONFIG.lock().gfx.dpi as f32);
        // Update the pixel size of the render target.
        self.viewpoint_interpolator
            .set_target_dimensions(params.target.get_dimensions());

        // Render the grid to the target, then save and return the result.
        self.last_render_result = G::render(self, params)?;
        Ok(self.last_render_result())
    }
    /// Returns data generated by the most recent render.
    pub fn last_render_result(&self) -> &RenderResult {
        &self.last_render_result
    }
    /// Returns a useful representation of a pixel position on the screen.
    pub fn screen_pos(&self, pixel: FVec2D) -> G::ScreenPos {
        G::screen_pos(self, pixel)
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

pub trait GridViewDimension: 'static + fmt::Debug + Default {
    type D: Dim;
    type Viewpoint: Viewpoint<Self::D>;
    type ScreenPos: ScreenPos<D = Self::D>;

    /// Executes a `View` command.
    fn do_view_command(this: &mut GenericGridView<Self>, command: ViewCommand) -> Result<()>;

    /// Renders the gridview.
    fn render(this: &mut GenericGridView<Self>, params: RenderParams<'_>) -> Result<RenderResult>;

    /// Returns a useful representation of a pixel position on the screen.
    fn screen_pos(this: &GenericGridView<Self>, pixel: FVec2D) -> Self::ScreenPos;
}

fn make_selection_drag_handler_with_threshold<G: GridViewDimension>(
    cursor_start: FVec2D,
    wait_for_drag_threshold: bool,
    mut inner_drag_handler: SelectionDragHandler<G>,
) -> DragHandler<GenericGridView<G>> {
    let drag_threshold = r64(CONFIG.lock().mouse.drag_threshold);

    // State variable to be moved into the closure and used by the
    // drag handler.
    let mut past_drag_threshold: bool = false;
    if !wait_for_drag_threshold {
        past_drag_threshold = true;
    }

    Box::new(move |this, new_cursor_pos| {
        if !((new_cursor_pos - cursor_start).abs() < FVec::repeat(drag_threshold)) {
            past_drag_threshold = true;
        }
        if past_drag_threshold {
            let screen_pos = this.screen_pos(new_cursor_pos);
            inner_drag_handler(this, screen_pos)
        } else {
            Ok(DragOutcome::Continue)
        }
    })
}

#[derive(Debug, Clone)]
pub struct HistoryEntry<G: GridViewDimension> {
    automaton: NdAutomaton<G::D>,
    selection: Option<Selection<G::D>>,
    viewpoint: G::Viewpoint,
}
