use anyhow::{anyhow, Context, Result};
use log::{trace, warn};
use parking_lot::RwLock;
use std::sync::Arc;

use ndcell_core::axis::Axis::{X, Y};
use ndcell_core::prelude::*;

use super::camera::{Camera, Camera2D, Interpolate, Interpolator};
use super::common::{GridViewCommon, GridViewTrait, RenderParams, RenderResult};
use super::history::{History, HistoryBase, HistoryManager};
use super::render::grid2d::{RenderCache, RenderInProgress};
use super::selection::Selection2D;
use super::worker::*;
use super::DragHandler;
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
    pub automaton: ProjectedAutomaton2D,
    /// Selection.
    selection: Option<Selection2D>,
    /// Undo/redo history manager.
    history: HistoryManager<HistoryEntry>,

    /// Camera interpolator.
    camera_interpolator: Interpolator<Dim2D, Camera2D>,

    /// Communication channel with the simulation worker thread.
    worker: Option<Worker<ProjectedAutomaton2D>>,
    /// Cached render data unique to this GridView.
    render_cache: Option<RenderCache>,

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
                let cell_transform = self.camera().cell_transform();

                let initial_pos = cell_transform
                    .pixel_to_global_cell(cursor_start)
                    .map(|pos| pos.floor().0);

                let new_cell_state = match c.mode {
                    DrawMode::Place => self.selected_cell_state(),
                    DrawMode::Replace => {
                        if let Some(pos) = &initial_pos {
                            if self.get_cell(&self.cache().read(), pos)
                                == self.selected_cell_state()
                            {
                                0
                            } else {
                                self.selected_cell_state()
                            }
                        } else {
                            self.selected_cell_state()
                        }
                    }
                    DrawMode::Erase => 0,
                };

                let mut new_drag_handler: DragHandler<Self> = match c.shape {
                    DrawShape::Freeform => {
                        let mut pos1 = initial_pos;
                        Box::new(move |this, new_cursor_pos| {
                            if this.too_small_to_draw() {
                                // Cancel the drag.
                                return Ok(false);
                            }
                            let pos2 = this
                                .camera()
                                .cell_transform()
                                .pixel_to_global_cell(new_cursor_pos)
                                .map(|pos| pos.floor().0);
                            if let (Some(pos1), Some(pos2)) = (&pos1, &pos2) {
                                for pos in ndcell_core::math::bresenham(pos1.clone(), pos2.clone())
                                {
                                    this.automaton.set_cell(&pos, new_cell_state);
                                }
                            }
                            pos1 = pos2;
                            // Continue the drag.
                            Ok(true)
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
                if let SelectDragCommand::Resize { .. } = c {
                    if self.selection.is_none() {
                        return Ok(());
                    }
                }

                let cell_transform = self.camera().cell_transform();

                let initial_pos_fract = match cell_transform.pixel_to_global_cell(cursor_start) {
                    Some(pos) => pos,
                    None => return Ok(()),
                };
                let initial_pos = initial_pos_fract.floor().0;

                // Compute initial selection rectangle. `pos1` is fixed; `pos2`
                // will change with the mouse cursor.
                let (pos1, pos2) = match c {
                    SelectDragCommand::NewRect => (initial_pos.clone(), initial_pos.clone()),
                    SelectDragCommand::Resize { .. } => {
                        if let Some(sel) = &self.selection {
                            (
                                // Farthest corner stays fixed.
                                sel.rect.farthest_corner(&initial_pos),
                                // Closest corner varies.
                                sel.rect.closest_corner(&initial_pos),
                            )
                        } else {
                            // Can't resize the selection if there is no
                            // selection!
                            return Ok(());
                        }
                    }
                };

                let mut moved = match c {
                    SelectDragCommand::NewRect => false,
                    SelectDragCommand::Resize { .. } => true,
                };
                let new_drag_handler: DragHandler<Self> = Box::new(move |this, new_cursor_pos| {
                    // TODO: DPI-aware mouse movement threshold before making
                    // new selection, then call drag handler before returning
                    // from do_select_command(), in case threshold=0
                    if !moved {
                        if new_cursor_pos != cursor_start {
                            moved = true;
                        } else {
                            // Give the user a chance to remove their selection
                            // instead of making a new one.
                            return Ok(true);
                        }
                    }

                    let new_pos = this
                        .camera()
                        .cell_transform()
                        .pixel_to_global_cell(new_cursor_pos)
                        .map(|pos| pos);
                    if let Some(new_pos) = new_pos {
                        let new_corner = match c {
                            SelectDragCommand::NewRect => new_pos.floor().0,
                            SelectDragCommand::Resize {
                                x: resize_x,
                                y: resize_y,
                                absolute,
                                ..
                            } => {
                                if absolute {
                                    new_pos.floor().0
                                } else {
                                    // Use delta from original cursor position to new
                                    // cursor position.
                                    let delta = new_pos - &initial_pos_fract;
                                    let mut new_corner = delta.round() + &pos2;

                                    // Only resize along certain axes.
                                    if !resize_x {
                                        // Keep original X value.
                                        new_corner[X] = pos2[X].clone();
                                    }
                                    if !resize_y {
                                        // Keep original Y value.
                                        new_corner[Y] = pos2[Y].clone();
                                    }

                                    new_corner
                                }
                            }
                        };
                        this.selection = Some(Selection2D {
                            rect: BigRect::span(pos1.clone(), new_corner),
                            cells: None,
                        });
                    }
                    // Continue the drag.
                    Ok(true)
                });

                if config.hist.record_select {
                    self.stop_running();
                    self.record();
                }
                if let SelectDragCommand::NewRect = c {
                    self.deselect();
                }
                self.drag_handler = Some(new_drag_handler);
            }
        }
        Ok(())
    }
    fn do_clipboard_command(&mut self, command: ClipboardCommand, _config: &Config) -> Result<()> {
        if !self.is_drawing() {
            self.stop_running();
            match command {
                ClipboardCommand::CopyRle => {
                    let result = self.as_automaton().to_rle_string();
                    match result {
                        Ok(s) => {
                            clipboard_set(s).map_err(|_| anyhow!("Setting clipboard contents"))?
                        }
                        Err(msg) => warn!("Failed to generate RLE: {}", msg),
                    }
                }
                ClipboardCommand::CopyCxrle => {
                    let result = self.as_automaton().to_cxrle_string();
                    match result {
                        Ok(s) => {
                            clipboard_set(s).map_err(|_| anyhow!("Setting clipboard contents"))?
                        }
                        Err(msg) => warn!("Failed to generate CXRLE: {}", msg),
                    }
                }
                ClipboardCommand::Paste => {
                    self.record();
                    let rle_string =
                        clipboard_get().map_err(|_| anyhow!("Fetching clipboard contents"))?;
                    let result = Automaton::from_rle_str(&rle_string, |_| {
                        Ok(crate::load_custom_rule_2d().into())
                    });
                    match result {
                        Ok(Automaton::Automaton2D(automaton)) => *self = Self::from(automaton),
                        Ok(_) => warn!("Failed to load RLE because rule is not 2D"),
                        Err(msg) => warn!("Failed to load RLE: {}", msg),
                    }
                }
            }
        }
        Ok(())
    }
    fn do_move_command(&mut self, command: ViewCommand, config: &Config) -> Result<()> {
        let maybe_new_drag_handler = self
            .camera_interpolator
            .do_move_command(command, config)
            .context("Executing move command")?
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
            let continue_drag = h(self, cursor_pos)?;
            if continue_drag {
                self.drag_handler = Some(h);
            } else {
                self.stop_drag()?;
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

    fn as_automaton<'a>(&'a self) -> AutomatonRef<'a> {
        AutomatonRef::from(&self.automaton)
    }
    fn as_automaton_mut<'a>(&'a mut self) -> AutomatonMut<'a> {
        AutomatonMut::from(&mut self.automaton)
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

        let mut render_cache = std::mem::replace(&mut self.render_cache, None).unwrap_or_default();
        let node_cache = self.automaton.projected_cache().read();
        let mut rip = RenderInProgress::new(self, params, &node_cache, &mut render_cache)?;
        rip.draw_cells()?;

        // Draw gridlines.
        let gridlines_width = self
            .camera()
            .scale()
            .factor()
            .to_f64()
            .map(|x| x * GRIDLINE_WIDTH)
            .unwrap_or(1.0);
        rip.draw_gridlines(gridlines_width)?;
        // Draw crosshairs.
        match self.mouse().display {
            MouseDisplay::Draw => {
                if !self.too_small_to_draw() {
                    rip.draw_hover_highlight(gridlines_width * 2.0, crate::colors::HOVERED_DRAW)?;
                }
            }
            MouseDisplay::Select => {
                rip.draw_hover_highlight(gridlines_width * 2.0, crate::colors::HOVERED_SELECT)?;
            }
            _ => (),
        }
        // Draw selection.
        if let Some(selection) = &self.selection {
            rip.draw_selection_highlight(selection.rect.clone(), gridlines_width * 4.0)?;
        }

        self.common.last_render_result = rip.finish();
        self.render_cache = Some(render_cache);
        Ok(self.last_render_result())
    }
}

impl GridView2D {
    /// Returns the current camera.
    pub fn camera(&self) -> &Camera2D {
        &self.camera_interpolator.current
    }
    /// Returns the cell position underneath the cursor. Floor this to get an
    /// integer cell position.
    pub fn hovered_cell_pos(&self) -> Option<FixedVec2D> {
        self.camera()
            .cell_transform()
            .pixel_to_global_cell(self.common.mouse.pos?)
    }
    /// Deselect all.
    pub fn deselect(&mut self) {
        if self
            .selection
            .as_ref()
            .and_then(|s| s.cells.as_ref())
            .is_some()
        {
            todo!("deselect with cells");
        }
        self.selection = None;
    }

    /// Returns `true` if the scale is too small to draw individual cells, or
    /// `false` otherwise.
    fn too_small_to_draw(&self) -> bool {
        self.camera().scale() < Scale::from_factor(r64(1.0))
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

impl GridView2D {
    pub fn cache(&self) -> &Arc<RwLock<NodeCache<Dim2D>>> {
        &self.automaton.projected_cache()
    }
    pub fn get_cell(&self, cache: &NodeCache<Dim2D>, pos: &BigVec2D) -> u8 {
        self.automaton.projected_tree().get_cell(cache, pos)
    }

    fn get_worker(&mut self) -> &mut Worker<ProjectedAutomaton2D> {
        if let None = self.worker {
            self.worker = Some(Worker::new(self.automaton.clone()));
        }
        self.worker.as_mut().unwrap()
    }
}

pub struct HistoryEntry {
    automaton: ProjectedAutomaton2D,
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
