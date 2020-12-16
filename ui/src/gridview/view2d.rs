use anyhow::{anyhow, Context, Result};
use log::{trace, warn};

use ndcell_core::prelude::*;

use super::camera::{Camera, Camera2D, Interpolate, Interpolator, ScreenPos2D};
use super::common::{GridViewCommon, GridViewTrait, RenderParams, RenderResult};
use super::history::{History, HistoryBase, HistoryManager};
use super::render::grid2d::{NdTreeDrawParameters, RenderInProgress};
use super::selection::Selection2D;
use super::worker::*;
use super::{DragHandler, DragOutcome};
use crate::clipboard_compat::{clipboard_get, clipboard_set};
use crate::commands::*;
use crate::config::{Config, MouseDisplay};
use crate::Scale;

/// The number of render results to remember.
const RENDER_RESULTS_COUNT: usize = 4;
/// The number of previous simulation steps to track for counting UPS.
const MAX_LAST_SIM_TIMES: usize = 4;

/// Width of gridlines, in units of cells.
const GRIDLINE_WIDTH: f64 = 1.0 / 32.0;

#[derive(Default)]
pub struct GridView2D {
    common: GridViewCommon,

    /// Automaton being simulated and displayed.
    pub automaton: Automaton2D,
    /// Selection.
    selection: Option<Selection2D>,
    /// Undo/redo history manager.
    history: HistoryManager<HistoryEntry>,

    /// Camera interpolator.
    camera_interpolator: Interpolator<Dim2D, Camera2D>,

    /// Communication channel with the simulation worker thread.
    worker: Option<Worker<Automaton2D>>,

    /// Mouse drag handler.
    drag_handler: Option<DragHandler<Self>>,
}
impl AsRef<GridViewCommon> for GridView2D {
    fn as_ref(&self) -> &GridViewCommon {
        &self.common
    }
}
impl AsMut<GridViewCommon> for GridView2D {
    fn as_mut(&mut self) -> &mut GridViewCommon {
        &mut self.common
    }
}

impl GridViewTrait for GridView2D {
    fn do_draw_command(&mut self, command: DrawCommand, _config: &Config) -> Result<()> {
        // Don't draw if the scale is too small.
        if self.too_small_to_draw() {
            self.common.is_drawing = false;
            self.drag_handler = None;
            return Ok(());
        }

        match command {
            DrawCommand::SetState(new_selected_cell_state) => {
                self.common.selected_cell_state = new_selected_cell_state;
            }
            DrawCommand::Drag(c, cursor_start) => {
                let maybe_initial_pos = self.camera().pixel_to_screen_pos(cursor_start);
                let initial_pos = match maybe_initial_pos {
                    Some(pos) => pos.int_cell().clone(),
                    None => return Ok(()),
                };

                let new_cell_state = c.mode.cell_state(
                    self.automaton.ndtree.get_cell(&initial_pos),
                    self.selected_cell_state(),
                );

                let mut new_drag_handler: DragHandler<Self> = match c.shape {
                    DrawShape::Freeform => {
                        let mut pos1 = initial_pos;
                        Box::new(move |this, new_cursor_pos| {
                            if this.too_small_to_draw() {
                                return Ok(DragOutcome::Cancel);
                            }
                            let pos2 = this
                                .camera()
                                .cell_transform()
                                .pixel_to_global_cell(new_cursor_pos)
                                .map(|pos| pos.floor().0);
                            if let Some(pos2) = &pos2 {
                                for pos in ndcell_core::math::bresenham(pos1.clone(), pos2.clone())
                                {
                                    this.automaton.ndtree.set_cell(&pos, new_cell_state);
                                }
                                pos1 = pos2.clone();
                                Ok(DragOutcome::Continue)
                            } else {
                                Ok(DragOutcome::Cancel)
                            }
                        })
                    }
                    DrawShape::Line => {
                        // TODO: implement drawing straight line
                        warn!("Line drawing is not yet implemented!");
                        return Ok(());
                    }
                };

                self.stop_running();
                self.record();
                self.common.is_drawing = true;
                new_drag_handler(self, cursor_start)?;
                self.drag_handler = Some(new_drag_handler);
            }
        }
        Ok(())
    }
    fn do_select_command(&mut self, command: SelectCommand, config: &Config) -> Result<()> {
        match command {
            SelectCommand::Drag(c, cursor_start) => {
                let maybe_initial_pos = self.camera().pixel_to_screen_pos(cursor_start);
                let initial_pos = match maybe_initial_pos {
                    Some(pos) => pos,
                    None => return Ok(()),
                };

                let drag_threshold = r64(config.mouse.drag_threshold);
                // State variable to be moved into the closure and used by the
                // drag handler.
                let mut initial_selection = None;

                let mut new_drag_handler: Box<
                    dyn FnMut(&mut Self, ScreenPos2D) -> Result<DragOutcome>,
                > = match c {
                    SelectDragCommand::NewRect => Box::new(move |this, new_pos| {
                        this.set_selection_rect(Some(NdRect::span(
                            initial_pos.int_cell().clone(),
                            new_pos.int_cell().clone(),
                        )));
                        Ok(DragOutcome::Continue)
                    }),
                    SelectDragCommand::Resize { axes, .. } => Box::new(move |this, new_pos| {
                        initial_selection = initial_selection.take().or_else(|| this.deselect());
                        if let Some(s) = &initial_selection {
                            this.set_selection_rect(Some(
                                super::selection::resize_selection_relative(
                                    &s.rect,
                                    initial_pos.cell(),
                                    new_pos.cell(),
                                    axes,
                                ),
                            ));
                            Ok(DragOutcome::Continue)
                        } else {
                            // There is no selection to resize.
                            Ok(DragOutcome::Cancel)
                        }
                    }),
                    SelectDragCommand::ResizeToCell => Box::new(move |this, new_pos| {
                        initial_selection = initial_selection.take().or_else(|| this.deselect());
                        if let Some(s) = &initial_selection {
                            this.set_selection_rect(Some(
                                super::selection::resize_selection_absolute(
                                    &s.rect,
                                    initial_pos.cell(),
                                    new_pos.cell(),
                                ),
                            ));
                            Ok(DragOutcome::Continue)
                        } else {
                            // There is no selection to resize.
                            Ok(DragOutcome::Cancel)
                        }
                    }),
                    SelectDragCommand::MoveSelection => Box::new(move |this, new_pos| {
                        initial_selection = initial_selection.take().or_else(|| this.deselect());
                        if let Some(s) = &initial_selection {
                            let delta = (new_pos.cell() - initial_pos.cell()).round();
                            this.set_selection_rect(Some(s.rect.clone() + delta));
                            Ok(DragOutcome::Continue)
                        } else {
                            // There is no selection to move.
                            Ok(DragOutcome::Cancel)
                        }
                    }),
                    SelectDragCommand::MoveCells => Box::new(move |this, new_pos| {
                        initial_selection = initial_selection.take().or_else(|| {
                            this.grab_selected_cells();
                            this.selection.take()
                        });
                        if let Some(s) = &initial_selection {
                            let delta = (new_pos.cell() - initial_pos.cell()).round();
                            this.selection = Some(s.move_by(delta));
                            Ok(DragOutcome::Continue)
                        } else {
                            // There is no selection to move.
                            Ok(DragOutcome::Cancel)
                        }
                    }),
                };

                // State variable to be moved into the closure and used by the
                // drag handler.
                let mut past_drag_threshold: bool = false;

                let new_drag_handler_with_threshold: DragHandler<Self> =
                    Box::new(move |this, new_cursor_pos| {
                        if !((new_cursor_pos - cursor_start).abs() < FVec::repeat(drag_threshold)) {
                            past_drag_threshold = true;
                        }
                        if past_drag_threshold {
                            if let Some(new_pos) = this.camera().pixel_to_screen_pos(new_cursor_pos)
                            {
                                new_drag_handler(this, new_pos)
                            } else {
                                Ok(DragOutcome::Continue)
                            }
                        } else {
                            Ok(DragOutcome::Continue)
                        }
                    });

                match c {
                    SelectDragCommand::NewRect
                    | SelectDragCommand::Resize { .. }
                    | SelectDragCommand::ResizeToCell
                    | SelectDragCommand::MoveSelection => {
                        if config.hist.record_select {
                            self.stop_running();
                            self.record();
                        }
                    }
                    SelectDragCommand::MoveCells => {
                        self.stop_running();
                        self.record();
                    }
                }

                if let SelectDragCommand::NewRect = c {
                    // Deselect immediately; don't wait for drag threshold.
                    self.deselect();
                }

                self.drag_handler = Some(new_drag_handler_with_threshold);

                // Execute the drag handler once immediately.
                self.continue_drag(cursor_start)?;
            }

            SelectCommand::SelectAll => {
                self.set_selection(self.automaton.ndtree.bounding_rect().map(Selection2D::from))
            }
        }
        Ok(())
    }
    fn do_clipboard_command(&mut self, command: ClipboardCommand, _config: &Config) -> Result<()> {
        if !self.is_drawing() {
            self.stop_running();
            match command {
                ClipboardCommand::Copy(format) => {
                    let result = self.export(format);
                    match result {
                        Ok(s) => {
                            clipboard_set(s).map_err(|_| anyhow!("Setting clipboard contents"))?
                        }
                        Err(msg) => warn!("Failed to generate {}: {}", format, msg),
                    }
                }
                ClipboardCommand::Paste => {
                    self.record();
                    let string_from_clipboard =
                        clipboard_get().map_err(|_| anyhow!("Fetching clipboard contents"))?;
                    let result =
                        Selection2D::from_str(&string_from_clipboard, self.automaton.ndtree.pool());
                    match result {
                        Ok(sel) => self.set_selection(sel),
                        Err(errors) => warn!("Failed to load pattern: {:?}", errors),
                    }
                }
            }
        }
        Ok(())
    }
    fn do_view_command(&mut self, command: ViewCommand, config: &Config) -> Result<()> {
        let maybe_new_drag_handler = self
            .camera_interpolator
            .do_view_command(command, config)
            .context("Executing view command")?
            .map(|mut interpolator_drag_handler| {
                Box::new(move |this: &mut Self, cursor_pos| {
                    interpolator_drag_handler(&mut this.camera_interpolator, cursor_pos)
                }) as DragHandler<Self>
            });
        if maybe_new_drag_handler.is_some() && self.drag_handler.is_none() {
            self.drag_handler = maybe_new_drag_handler;
            self.common.is_dragging_view = true;
        }
        Ok(())
    }
    fn continue_drag(&mut self, cursor_pos: FVec2D) -> Result<()> {
        if let Some(mut h) = self.drag_handler.take() {
            match h(self, cursor_pos)? {
                DragOutcome::Continue => self.drag_handler = Some(h),
                DragOutcome::Cancel => self.stop_drag()?,
            }
        }
        Ok(())
    }
    fn stop_drag(&mut self) -> Result<()> {
        self.drag_handler = None;
        self.common.is_drawing = false;
        self.common.is_dragging_view = false;
        Ok(())
    }

    fn enqueue_worker_request(&mut self, request: WorkerRequest) {
        match &request {
            WorkerRequest::Step(_) => self.common.is_waiting = true,
            WorkerRequest::SimContinuous(_) => (),
        }
        self.get_worker().request(request);
    }
    fn reset_worker(&mut self) {
        self.worker = None;
        trace!("Reset simulation worker thread");
    }

    fn export(&self, format: CaFormat) -> Result<String, CaFormatError> {
        let ndtree = self
            .selection
            .as_ref()
            .and_then(|sel| sel.cells.as_ref())
            .unwrap_or(&self.automaton.ndtree);
        let two_states = TwoState::from_rule(&*self.automaton.rule);
        let rect = self.selection_rect().cloned();
        ndcell_core::io::export_ndtree_to_string(ndtree, format, two_states, rect)
    }

    fn run_step(&mut self) {
        if let Some(worker) = self.worker.as_mut() {
            if let Some(WorkerResult {
                result,
                record,
                time,
            }) = worker.take()
            {
                if !self.common.is_running {
                    self.common.is_waiting = worker.get_request_count() > 0;
                }
                if record {
                    self.record();
                }
                self.automaton = result;
                self.common.last_sim_times.push_back(time);
                if self.common.last_sim_times.len() > MAX_LAST_SIM_TIMES {
                    self.common.last_sim_times.pop_front();
                }
            }
        }
    }

    fn camera_interpolator(&mut self) -> &mut dyn Interpolate {
        &mut self.camera_interpolator
    }
    fn render(&mut self, params: RenderParams<'_>) -> Result<&RenderResult> {
        let config = params.config;

        // Update DPI.
        self.camera_interpolator().set_dpi(config.gfx.dpi as f32);

        let mut rip = RenderInProgress::new(self, params)?;

        // Draw main cells.
        rip.draw_cells(
            &self.automaton.ndtree,
            NdTreeDrawParameters {
                alpha: 1.0,
                rect: None,
            },
        )?;
        // Draw selection cells.
        if let Some(selection) = &self.selection {
            if let Some(cells) = &selection.cells {
                rip.draw_cells(
                    cells,
                    NdTreeDrawParameters {
                        alpha: 1.0,
                        rect: Some(&selection.rect),
                    },
                )?;
            }
        }

        // Draw gridlines.
        let gridlines_width = self
            .camera()
            .scale()
            .factor()
            .to_f64()
            .map(|x| x * GRIDLINE_WIDTH)
            .unwrap_or(1.0);
        rip.draw_gridlines(gridlines_width)?;
        // Draw mouse display.
        if let Some(mouse_pos) = self.mouse_pos() {
            match self.mouse().display {
                MouseDisplay::Draw => {
                    if !self.too_small_to_draw() {
                        rip.draw_hover_highlight(
                            mouse_pos.int_cell(),
                            gridlines_width * 2.0,
                            crate::colors::HOVERED_DRAW,
                        )?;
                    }
                }
                MouseDisplay::Select => {
                    rip.draw_hover_highlight(
                        mouse_pos.int_cell(),
                        gridlines_width * 2.0,
                        crate::colors::HOVERED_SELECT,
                    )?;
                }
                _ => (),
            }
        }
        // Draw selection highlight.
        if let Some(selection) = &self.selection {
            rip.draw_selection_highlight(
                selection.rect.clone(),
                gridlines_width * 4.0,
                selection.cells.is_none(),
            )?;
        }
        // Draw selection preview after drawing selection.
        if self.mouse().display == MouseDisplay::ResizeSelectionAbsolute
            && self.drag_handler.is_none()
        {
            if let Some((mouse_pos, s)) = self.mouse_pos().zip(self.selection.as_ref()) {
                rip.draw_absolute_selection_resize_preview(
                    s.rect.clone(),
                    &mouse_pos,
                    gridlines_width * 2.0,
                )?;
            }
        }

        self.common.last_render_result = rip.finish()?;
        Ok(self.last_render_result())
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

impl From<Automaton2D> for GridView2D {
    fn from(automaton: Automaton2D) -> Self {
        Self {
            automaton,
            ..Default::default()
        }
    }
}

impl GridView2D {
    /// Returns the current camera.
    pub fn camera(&self) -> &Camera2D {
        &self.camera_interpolator.current
    }

    /// Returns the selection rectangle.
    pub fn selection_rect(&self) -> Option<&BigRect2D> {
        self.selection.as_ref().map(|sel| &sel.rect)
    }
    /// Deselects and sets a new selection rectangle.
    pub fn set_selection_rect(&mut self, new_selection_rect: Option<BigRect2D>) {
        self.set_selection(new_selection_rect.map(Selection2D::from))
    }
    /// Deselects and sets a new selection.
    pub fn set_selection(&mut self, new_selection: Option<Selection2D>) {
        self.deselect();
        self.selection = new_selection;
    }
    /// Deselects all returns the old selection.
    pub fn deselect(&mut self) -> Option<Selection2D> {
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
    /// Moves the cells in the selected region from the main grid into the
    /// selection. Outputs a warning to the log if there is no selection. Does
    /// nothing if the selection already contains cells.
    pub fn grab_selected_cells(&mut self) {
        if let Some(sel) = &mut self.selection {
            if sel.cells.is_none() {
                let region = Region::Rect(sel.rect.clone());
                sel.cells = Some(self.automaton.ndtree.get_region(region.clone()));
                self.automaton.ndtree.clear_region(region);
            }
        } else {
            warn!("grab_selected_cells() called with no selection");
        }
    }

    /// Returns `true` if the scale is too small to draw individual cells, or
    /// `false` otherwise.
    fn too_small_to_draw(&self) -> bool {
        self.camera().scale() < Scale::from_factor(r64(1.0))
    }

    pub fn mouse_pos(&self) -> Option<ScreenPos2D> {
        self.camera().pixel_to_screen_pos(self.mouse().pos?)
    }

    fn get_worker(&mut self) -> &mut Worker<Automaton2D> {
        if let None = self.worker {
            self.worker = Some(Worker::new(self.automaton.clone()));
        }
        self.worker.as_mut().unwrap()
    }
}

pub struct HistoryEntry {
    automaton: Automaton2D,
    selection: Option<Selection2D>,
    camera: Camera2D,
}

impl HistoryBase for GridView2D {
    type Entry = HistoryEntry;

    fn history_entry(&self) -> Self::Entry {
        HistoryEntry {
            automaton: self.automaton.clone(),
            selection: self.selection.clone(),
            camera: self.camera_interpolator.target.clone(),
        }
    }

    fn restore_history_entry(&mut self, config: &Config, entry: Self::Entry) -> Self::Entry {
        HistoryEntry {
            automaton: std::mem::replace(&mut self.automaton, entry.automaton),
            selection: Selection2D::restore_history_entry(
                config,
                &mut self.selection,
                entry.selection,
            ),
            camera: if config.hist.record_view {
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
