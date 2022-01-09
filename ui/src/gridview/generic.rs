use anyhow::{anyhow, bail, ensure, Context, Result};
use glium::Surface;
use log::{debug, error, info, trace, warn};
use parking_lot::Mutex;
use std::collections::VecDeque;
use std::convert::TryInto;
use std::fmt;
use std::sync::mpsc;
use std::time::{Duration, Instant};

use ndcell_core::prelude::*;

use super::algorithms::bresenham;
use super::drag::{Drag, DragCancelFn, DragOutcome, DragUpdateFn};
use super::history::{History, HistoryBase, HistoryManager};
use super::render::{RenderParams, RenderResult};
use super::screenpos::{OperationPos, OperationPosTrait, ScreenPosTrait};
use super::selection::Selection;
use super::viewpoint::{Interpolator, Viewpoint};
use super::worker::*;
use super::WorkType;
use crate::commands::*;
use crate::mouse::MouseState;
use crate::{Face, Scale, CONFIG};

/// Number of previous frame times to track. If this is too low, viewpoint
/// interpolation may not work.
const MAX_LAST_FRAME_TIMES: usize = 2;
/// Number of previous simulation steps to track for counting simulation
/// steps per second.
const MAX_LAST_SIM_TIMES: usize = 4;

macro_rules! ignore_command {
    ($c:expr) => {{
        warn!("Ignoring {:?} in GridView{}D", $c, D::NDIM);
        return;
    }};
    ($c:expr, if $cond:expr) => {{
        if $cond {
            ignore_command!($c);
        }
    }};
}

/// Dimension-generic interactive cellular automaton interface.
pub struct GenericGridView<D: GridViewDimension> {
    pub automaton: NdAutomaton<D>,
    pub selection: Option<Selection<D>>,
    pub viewpoint_interpolator: Interpolator<D, D::Viewpoint>,
    history: HistoryManager<HistoryEntry<D>>,
    /// Dimension-specific data.
    pub(super) dim_data: D::Data,

    /// Queue of pending commands to be executed on the next frame.
    command_queue: Mutex<VecDeque<CmdMsg>>,
    /// Thread to offload long-running computations onto.
    worker_thread: WorkerThread,
    /// What kind of work the worker thread is doing right now.
    work_type: Option<WorkType>,

    /// Communication channel with the garbage collection thread.
    gc_channel: Option<mpsc::Receiver<()>>,

    /// Mouse drag in progress.
    drag: Option<Drag<D>>,

    /// Time that the last several frames completed.
    last_frame_times: VecDeque<Instant>,
    /// Last several simulation times, with the most recent at the front.
    last_sim_times: VecDeque<Duration>,
    /// Most recent render result.
    last_render_result: RenderResult,

    /// Selected cell state.
    pub selected_cell_state: u8,
}
impl<D: GridViewDimension> From<NdAutomaton<D>> for GenericGridView<D> {
    fn from(automaton: NdAutomaton<D>) -> Self {
        Self {
            automaton,
            ..Default::default()
        }
    }
}
impl<D: GridViewDimension> Default for GenericGridView<D> {
    fn default() -> Self {
        Self {
            automaton: Default::default(),
            selection: Default::default(),
            viewpoint_interpolator: Default::default(),
            history: Default::default(),
            dim_data: Default::default(),

            command_queue: Default::default(),
            worker_thread: Default::default(),
            work_type: Default::default(),

            gc_channel: Default::default(),

            drag: Default::default(),

            last_frame_times: Default::default(),
            last_sim_times: Default::default(),
            last_render_result: Default::default(),

            // ... all that for one non-default attribute
            selected_cell_state: 1_u8,
        }
    }
}
impl<D: GridViewDimension> AsSimulate for GenericGridView<D> {
    fn as_sim(&self) -> &dyn Simulate {
        &self.automaton
    }
    fn as_sim_mut(&mut self) -> &mut dyn Simulate {
        &mut self.automaton
    }
}
impl<D: GridViewDimension> HistoryBase for GenericGridView<D> {
    type Entry = HistoryEntry<D>;

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
                std::mem::replace(self.target_viewpoint(), entry.viewpoint)
            } else {
                self.target_viewpoint().clone()
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
impl<D: GridViewDimension> GenericGridView<D> {
    /// Enqueues a command to be executed on the next frame.
    ///
    /// This should be preferred to executing commands immediately.
    pub fn enqueue(&self, command: impl Into<CmdMsg>) {
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
        for cmd_msg in old_command_queue {
            self.do_command(cmd_msg)?;
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
    /// Executes a command.
    pub(super) fn do_command(
        &mut self,
        CmdMsg {
            command,
            cursor_pos,
        }: CmdMsg,
    ) -> Result<()> {
        const MISSING_CURSOR_POS_MSG: &str = "Missing cursor position for command that requires it";

        let screen_pos = cursor_pos
            .clone()
            .context(MISSING_CURSOR_POS_MSG)
            .map(|p| self.screen_pos(p));
        let cursor_pos = cursor_pos.context(MISSING_CURSOR_POS_MSG);

        match command {
            Cmd::BeginDrag(cmd) => Ok(self.begin_drag(cmd, cursor_pos?)),
            Cmd::ContinueDrag => self.continue_drag(cursor_pos?),
            Cmd::EndDrag => Ok(self.end_drag()),
            Cmd::CancelDrag => Ok(self.cancel_drag()),

            Cmd::Cancel => Ok(self.cancel()),

            Cmd::Undo => {
                self.end_drag();
                self.reset_worker_thread();
                self.undo();
                Ok(())
            }
            Cmd::Redo => {
                self.end_drag();
                self.reset_worker_thread();
                self.redo();
                Ok(())
            }
            Cmd::Reset => {
                self.end_drag();
                self.reset_worker_thread();
                self.reset();
                Ok(())
            }

            Cmd::Move2D(movement) => Ok(self.target_viewpoint().apply_move_2d(movement)),
            Cmd::Move3D(movement) => Ok(self.target_viewpoint().apply_move_3d(movement)),
            Cmd::Scale(log2_factor) => Ok(self
                .viewpoint_interpolator
                .target
                .scale_by_log2_factor(r64(log2_factor), None)),
            Cmd::ScaleToCursor(log2_factor) => Ok(self
                .target_viewpoint()
                .scale_by_log2_factor(r64(log2_factor), screen_pos?.scale_invariant_pos())),

            Cmd::SnapPos => Ok(self.target_viewpoint().snap_center()),
            Cmd::SnapScale => Ok(self.target_viewpoint().snap_scale(None)),
            Cmd::SnapScaleToCursor => Ok(self
                .target_viewpoint()
                .snap_scale(screen_pos?.scale_invariant_pos())),

            Cmd::ResetView => Ok(self.go_to_origin()),
            Cmd::FitView => Ok(self.fit_view()),
            Cmd::FocusCursor => Ok(D::focus(self, &screen_pos?)),

            Cmd::SetDrawState(state) => Ok(self.selected_cell_state = state),
            Cmd::NextDrawState { wrap } => Ok(self.select_next_cell_state(1, wrap)),
            Cmd::PrevDrawState { wrap } => Ok(self.select_next_cell_state(-1, wrap)),
            Cmd::ConfirmDraw => Ok(self.confirm_draw()),
            Cmd::CancelDraw => Ok(self.cancel_draw()),

            Cmd::SelectAll => Ok(self.select_all()),
            Cmd::Deselect => {
                self.deselect();
                Ok(())
            }
            Cmd::CopySelection(format) => self.copy_selection(format),
            Cmd::PasteSelection => self.paste_selection(),
            Cmd::DeleteSelection => Ok(self.delete_selection()),
            Cmd::CancelSelection => Ok(self.cancel_selection()),

            // For all the simulation-related commands, ignore failures due to
            // existing background tasks, drawing, etc.
            Cmd::Step(step_size) => self.step(step_size.into()),
            Cmd::StepStepSize => self.step(CONFIG.lock().sim.step_size.clone()),
            Cmd::StartRunning => self.start_running().map(|_is_running| ()),
            Cmd::StopRunning => {
                self.stop_running();
                Ok(())
            }
            Cmd::ToggleRunning => self.toggle_running().map(|_is_running| ()),
            Cmd::UpdateStepSize => self.update_step_size(),
            Cmd::CancelSim => Ok(self.cancel_sim()),

            Cmd::ClearCache => Ok(self.schedule_gc()),
        }
    }

    /// Executes a `Cancel` command.
    fn cancel(&mut self) {
        if self.is_dragging() {
            self.cancel_drag();
        } else if self.reset_worker_thread() {
            // ok
        } else if self.is_drawing() {
            self.cancel_draw()
        } else {
            self.cancel_selection()
        }
    }
    /// Executes a `CancelDrag` command.
    fn cancel_drag(&mut self) {
        if let Some(drag) = self.drag.take() {
            if let Some(cancel_fn) = drag.cancel_fn {
                cancel_fn(self);
            }
        }
    }
    /// Executes a `CancelDraw` command.
    fn cancel_draw(&mut self) {
        if self.is_drawing() {
            self.cancel_drag();
        }
    }
    /// Executes a `CancelSelection` command.
    fn cancel_selection(&mut self) {
        if let Some(sel) = &self.selection {
            if sel.cells.is_some() {
                self.drop_selected_cells();
            } else {
                self.deselect();
            }
        }
    }
    /// Executes a `CancelSim` command.
    fn cancel_sim(&mut self) {
        match self.work_type {
            Some(WorkType::SimStep) | Some(WorkType::SimContinuous) => {
                self.reset_worker_thread();
            }
            None => (),
        }
    }

    /// Executes a `ConfirmDraw` command.
    fn confirm_draw(&mut self) {
        if self.is_drawing() {
            self.end_drag();
        }
    }

    /// Executes a `Reset` command.
    fn reset(&mut self) {
        // TODO: Instead of looping, track the oldest known state or do binary
        // search on history stack.
        while self.automaton.generation_count().is_positive() && self.can_undo() {
            self.undo();
        }
    }

    /// Executes a `GoToOrigin` command.
    fn go_to_origin(&mut self) {
        self.target_viewpoint().set_center(NdVec::origin());
    }
    /// Executes a `FitView` command.
    fn fit_view(&mut self) {
        if let Some(pattern_bounding_rect) = self.automaton.ndtree.bounding_rect() {
            // Set position.
            let center = pattern_bounding_rect.center().to_fixedvec();
            self.target_viewpoint().set_center(center);
            // Set scale.
            let pattern_size = pattern_bounding_rect.size();
            let target_size = self.viewpoint().target_dimensions();
            let scale = Scale::from_fit(pattern_size, target_size).floor();
            self.target_viewpoint().set_scale(scale);
        }
    }

    /// Selects the next or previous cell state (depending on the sign of
    /// `delta`), optionally using wrapping arithmetic.
    fn select_next_cell_state(&mut self, delta: isize, wrap: bool) {
        let mut new_cell_state = self.selected_cell_state as isize + delta;
        let max_state = self.automaton.rule.max_state() as isize;
        let state_count = max_state + 1;
        if wrap {
            new_cell_state = new_cell_state.rem_euclid(state_count);
        } else {
            new_cell_state = new_cell_state.clamp(0, max_state);
        }
        self.selected_cell_state = new_cell_state as u8;
        assert!(self.selected_cell_state <= max_state as u8);
    }

    /// Executes a `CopySelection` command.
    fn copy_selection(&mut self, format: CaFormat) -> Result<()> {
        if self.selection.is_some() {
            let s = self
                .export(format)
                .context("Error while serializing pattern")?;
            crate::clipboard_compat::clipboard_set(s)?;
        }

        Ok(())
    }
    /// Executes a `PasteSelection` command.
    fn paste_selection(&mut self) -> Result<()> {
        let old_sel_rect = self.selection_rect();

        self.reset_worker_thread();
        self.record();
        let string_from_clipboard = crate::clipboard_compat::clipboard_get()?;
        let result = Selection::from_str(&string_from_clipboard, self.automaton.ndtree.pool());
        match result {
            Ok(sel) => {
                self.set_selection(sel);
                self.ensure_selection_visible();

                // If selection size is the same, preserve position.
                if let Some((old_rect, new_sel)) = old_sel_rect.zip(self.selection.as_mut()) {
                    if old_rect.size() == new_sel.rect.size() {
                        *new_sel = new_sel.move_by(old_rect.min() - new_sel.rect.min());
                    }
                }
            }
            Err(errors) => info!("Failed to load pattern: {:?}", errors),
        }

        Ok(())
    }
    /// Executes a `DeleteSelection` command.
    fn delete_selection(&mut self) {
        if self.selection.is_some() {
            self.reset_worker_thread();
            self.record();
            self.selection.as_mut().unwrap().cells = None;
            self.grab_selected_cells();
            self.selection.as_mut().unwrap().cells = None;
        }
    }

    /// Executes a `Step` command.
    fn step(&mut self, step_size: BigInt) -> Result<()> {
        self.try_step(step_size)?;
        Ok(())
    }
    /// Executes an `UpdateStepSize` command.
    fn update_step_size(&mut self) -> Result<()> {
        if self.is_running() {
            self.stop_running();
            let restarted = self.start_running()?; // This should not fail.
            if !restarted {
                error!("Unable to restart simulation after updating step size");
            }
        }
        Ok(())
    }

    /// Begins dragging.
    ///
    /// Do not call this method from within a drag update function.
    pub(super) fn begin_drag(&mut self, command: DragCmd, pixel: FVec2D) {
        if self.drag.is_some() {
            warn!("Attempted to start new drag while still in the middle of one");
        } else {
            let initial_screen_pos = self.screen_pos(pixel);

            let update_fn = match &command {
                DragCmd::View(cmd) => self.make_drag_view_update_fn(*cmd, pixel),

                DragCmd::DrawFreeform(draw_mode) => {
                    self.make_drag_draw_update_fn(*draw_mode, initial_screen_pos)
                }

                DragCmd::SelectNewRect => {
                    self.deselect();
                    self.make_drag_select_new_rect_update_fn()
                }
                DragCmd::ResizeSelectionToCursor => {
                    self.make_drag_resize_selection_to_cursor_update_fn(initial_screen_pos)
                }
                DragCmd::ResizeSelection2D(direction) => {
                    ignore_command!(command, if D::NDIM != 2);
                    self.make_drag_resize_selection_update_fn(
                        initial_screen_pos,
                        direction.vector(),
                    )
                }
                DragCmd::ResizeSelection3D(face) => {
                    ignore_command!(command, if D::NDIM != 3);
                    self.make_drag_resize_selection_update_fn(
                        initial_screen_pos,
                        face.normal_ivec(),
                    )
                }
                DragCmd::MoveSelection(face) => {
                    self.make_drag_move_selection_update_fn(*face, initial_screen_pos, |this| {
                        // To move the selection, we must first drop the
                        // selected cells.
                        this.drop_selected_cells();
                    })
                }

                DragCmd::MoveSelectedCells(face) => {
                    self.make_drag_move_selection_update_fn(*face, initial_screen_pos, |this| {
                        // To move the selected cells, we must first grab those
                        // cells.
                        this.grab_selected_cells();
                    })
                }

                DragCmd::CopySelectedCells(face) => {
                    self.make_drag_move_selection_update_fn(*face, initial_screen_pos, |this| {
                        // To copy the selected cells, we must first grab a copy
                        // of those cells. If the cells are already grabbed,
                        // this does nothing.
                        this.grab_copy_of_selected_cells();
                    })
                }
            };

            if update_fn.is_some() {
                let cancel_fn: Option<DragCancelFn<Self>>;

                if CONFIG
                    .lock()
                    .hist
                    .should_record_history_for_drag_command(&command)
                {
                    self.reset_worker_thread();
                    self.record();

                    cancel_fn = Some(Box::new(|this| {
                        this.undo();
                    }));
                } else {
                    let old_ndtree = self.automaton.ndtree.clone();
                    let old_selection = self.selection.clone();
                    cancel_fn = Some(Box::new(move |this| {
                        this.automaton.ndtree = old_ndtree;
                        this.selection = old_selection;
                    }));
                }

                // Recreate `initial_screen_pos` because it may have been
                // consumed in the big `match`.
                let initial_screen_pos = self.screen_pos(pixel);
                let waiting_for_drag_threshold = command.always_uses_movement_threshold();
                self.drag = Some(Drag {
                    command,
                    initial_screen_pos,
                    waiting_for_drag_threshold,

                    update_fn,
                    cancel_fn,

                    ndtree_base_pos: self.automaton.ndtree.base_pos().clone(),
                });
            }
        }
    }
    /// Executes a `ContinueDrag` command, calling the drag handler.
    ///
    /// Do not call this method from within a drag handler.
    pub(super) fn continue_drag(&mut self, cursor_pos: FVec2D) -> Result<()> {
        if let Some(mut drag) = self.drag.take() {
            let screen_pos = self.screen_pos(cursor_pos);
            let outcome = drag.update(self, &screen_pos)?;
            self.drag = Some(drag);
            match outcome {
                DragOutcome::Continue => (),
                DragOutcome::Cancel => self.end_drag(),
            }
        }
        Ok(())
    }
    /// Executes an `EndDrag` command.
    ///
    /// Do not call this method from within a drag handler; instead return
    /// `DragOutcome::Cancel`.
    pub(super) fn end_drag(&mut self) {
        self.drag = None;
    }

    fn make_drag_view_update_fn(
        &self,
        command: DragViewCmd,
        cursor_start: FVec2D,
    ) -> Option<DragUpdateFn<D>> {
        self.viewpoint_interpolator
            .make_drag_update_fn(command, cursor_start)
            .map(|mut viewpoint_update_fn| -> DragUpdateFn<D> {
                Box::new(move |_drag, this, new_screen_pos| {
                    viewpoint_update_fn(&mut this.viewpoint_interpolator, new_screen_pos.pixel())
                })
            })
    }
    fn make_drag_draw_update_fn(
        &self,
        draw_mode: DrawMode,
        initial_pos: D::ScreenPos,
    ) -> Option<DragUpdateFn<D>> {
        let initial_cell = initial_pos
            .op_pos_for_drag_command(&DragCmd::DrawFreeform(draw_mode))?
            .into_cell();
        let new_cell_state = draw_mode.cell_state(
            self.automaton.ndtree.get_cell(&initial_cell),
            self.selected_cell_state,
        );

        let mut pos1 = initial_cell;
        Some(Box::new(move |drag, this, new_screen_pos| {
            if let Some(pos2) = drag.new_pos(new_screen_pos).map(|p| p.into_cell()) {
                for pos in bresenham::line(pos1.clone(), pos2.clone()) {
                    this.automaton.ndtree.set_cell(&pos, new_cell_state);
                }
                pos1 = pos2;
                Ok(DragOutcome::Continue)
            } else {
                Ok(DragOutcome::Cancel)
            }
        }))
    }
    fn make_drag_select_new_rect_update_fn(&self) -> Option<DragUpdateFn<D>> {
        Some(Box::new(move |drag, this, new_pos| {
            if let Some(resize_start) = drag.initial_render_cell_rect() {
                if let Some(resize_end) = drag.new_render_cell_rect(new_pos) {
                    this.set_selection_rect(Some(NdRect::span_rects(&resize_start, &resize_end)));
                }
                Ok(DragOutcome::Continue)
            } else {
                Ok(DragOutcome::Cancel)
            }
        }))
    }
    fn make_drag_resize_selection_to_cursor_update_fn(
        &self,
        initial_pos: D::ScreenPos,
    ) -> Option<DragUpdateFn<D>> {
        let mut initial_selection = None;
        let resize_start = initial_pos.absolute_selection_resize_start_pos()?;
        Some(Box::new(move |drag, this, new_pos| {
            initial_selection = initial_selection.take().or_else(|| this.deselect());
            if let Some(s) = &initial_selection {
                if let Some(resize_end) = drag.new_render_cell_rect(new_pos) {
                    this.set_selection_rect(Some(D::resize_selection_to_cursor(
                        &s.rect,
                        &resize_start,
                        &resize_end,
                        drag,
                    )));
                } else {
                    this.set_selection_rect(Some(s.rect.clone()));
                }
                Ok(DragOutcome::Continue)
            } else {
                // There is no selection to resize.
                Ok(DragOutcome::Cancel)
            }
        }))
    }
    fn make_drag_resize_selection_update_fn<D2: Dim>(
        &self,
        initial_pos: D::ScreenPos,
        resize_vector: IVec<D2>,
    ) -> Option<DragUpdateFn<D>> {
        // Attempt to cast `resize_vector` to the correct number of dimensions.
        let resize_vector: IVec<D> = AnyDimIVec::from(resize_vector).try_into().ok()?;

        let mut initial_selection = None;
        Some(Box::new(move |_drag, this, new_pos| {
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
    fn make_drag_move_selection_update_fn(
        &self,
        face: Option<Face>,
        initial_pos: D::ScreenPos,
        selection_setup_fn: impl 'static + Fn(&mut Self),
    ) -> Option<DragUpdateFn<D>> {
        let mut initial_selection = None;
        Some(Box::new(move |_drag, this, new_pos| {
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
    /// Returns `true` if the simulation is now running, or false if another
    /// running operation prevented starting it (e.g. drawing or some other
    /// background task).
    fn start_running(&mut self) -> Result<bool> {
        if self.is_running() {
            return Ok(true);
        } else if self.is_drawing() {
            return Ok(false);
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
        Ok(true)
    }
    /// Toggles continuous simulation. Returns `true` if the simulation is now
    /// running, or `false` if it is not. The operation may not succeed.
    fn toggle_running(&mut self) -> Result<bool> {
        if self.is_running() {
            self.stop_running();
            Ok(false) // `stop_running()` always succeeds.
        } else {
            self.start_running()
        }
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
    fn set_selection_nd<D2: Dim>(&mut self, new_selection: Option<Selection<D2>>) -> Result<()> {
        ensure!(
            D::NDIM == D2::NDIM,
            "set_selection_nd() received Selection of wrong dimensionality",
        );

        self.set_selection(unsafe {
            *std::mem::transmute::<Box<Option<Selection<D2>>>, Box<Option<Selection<D>>>>(Box::new(
                new_selection,
            ))
        });
        Ok(())
    }
    /// Deselects and sets a new selection.
    pub(super) fn set_selection(&mut self, new_selection: Option<Selection<D>>) {
        self.deselect();
        self.selection = new_selection;
    }
    /// Deselects and sets a new selection rectangle.
    pub(super) fn set_selection_rect(&mut self, new_selection_rect: Option<BigRect<D>>) {
        self.set_selection(new_selection_rect.map(Selection::from))
    }
    /// Selects all cells in the pattern.
    pub(super) fn select_all(&mut self) {
        self.deselect(); // Include pasted cells.
        self.set_selection(self.automaton.ndtree.bounding_rect().map(Selection::from));
    }
    /// Returns the selection rectangle.
    pub(super) fn selection_rect(&self) -> Option<BigRect<D>> {
        if let Some(s) = &self.selection {
            Some(s.rect.clone())
        } else {
            None
        }
    }
    /// Deselects all and returns the old selection.
    pub(super) fn deselect(&mut self) -> Option<Selection<D>> {
        if let Some(sel) = self.selection.clone() {
            if let Some(cells) = sel.cells {
                self.reset_worker_thread();
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

    /// Returns whether the user is currently dragging the mouse.
    pub fn is_dragging(&self) -> bool {
        self.drag.is_some()
    }
    /// Returns the current mouse drag.
    pub fn get_drag(&self) -> Option<&Drag<D>> {
        self.drag.as_ref()
    }
    /// Returns whether the user is currently drawing.
    pub fn is_drawing(&self) -> bool {
        if let Some(drag) = &self.drag {
            drag.command.is_draw_cmd()
        } else {
            false
        }
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
    pub fn viewpoint(&self) -> &D::Viewpoint {
        &self.viewpoint_interpolator.current
    }
    /// Returns a mutable reference to the target viewpoint.
    pub fn target_viewpoint(&mut self) -> &mut D::Viewpoint {
        &mut self.viewpoint_interpolator.target
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
        self.last_render_result = D::render(self, params)?;
        Ok(self.last_render_result())
    }
    /// Returns data generated by the most recent render.
    pub fn last_render_result(&self) -> &RenderResult {
        &self.last_render_result
    }
    /// Returns a useful representation of a pixel position on the screen.
    pub fn screen_pos(&self, pixel: FVec2D) -> D::ScreenPos {
        D::screen_pos(self, pixel)
    }
    /// Returns the cell to highlight for a given mouse display mode.
    pub(super) fn cell_to_highlight(&self, mouse: &MouseState) -> Option<OperationPos<D>> {
        let screen_pos = self.screen_pos(mouse.pos?);
        if let Some(drag) = self.get_drag() {
            drag.new_pos(&screen_pos)
        } else {
            screen_pos.op_pos_for_mouse_display_mode(mouse.display_mode)
        }
    }
    /// Returns the rectangle of the render cell to highlight for a given mouse
    /// display mode.
    pub(super) fn render_cell_rect_to_highlight(&self, mouse: &MouseState) -> Option<BigRect<D>> {
        Some(
            self.viewpoint()
                .render_cell_layer()
                .round_rect_with_base_pos(
                    BigRect::single_cell(self.cell_to_highlight(mouse)?.into_cell()),
                    self.automaton.ndtree.base_pos(),
                ),
        )
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

pub trait GridViewDimension: Dim {
    type Viewpoint: Viewpoint<Self>;
    type ScreenPos: ScreenPosTrait<D = Self>;

    /// Extra data stored for a `GridView` with this number of dimensions.
    type Data: fmt::Debug + Default;

    /// Executes a `FocusCursor` command. This has different behavior in 2D vs.
    /// 3D.
    fn focus(this: &mut GenericGridView<Self>, pos: &Self::ScreenPos);
    /// Resizes a selection rectangle to the cell at the cursor. This has
    /// different behavior in 2D vs. 3D.
    fn resize_selection_to_cursor(
        rect: &BigRect<Self>,
        start: &FixedVec<Self>,
        end: &BigRect<Self>,
        drag: &Drag<Self>,
    ) -> BigRect<Self>;

    /// Renders the gridview.
    fn render(this: &mut GenericGridView<Self>, params: RenderParams<'_>) -> Result<RenderResult>;

    /// Returns a useful representation of a pixel position on the screen.
    fn screen_pos(this: &GenericGridView<Self>, pixel: FVec2D) -> Self::ScreenPos;
}

#[derive(Debug, Clone)]
pub struct HistoryEntry<D: GridViewDimension> {
    automaton: NdAutomaton<D>,
    selection: Option<Selection<D>>,
    viewpoint: D::Viewpoint,
}
