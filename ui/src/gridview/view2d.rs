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
                if matches!(c, SelectDragCommand::Resize {.. } | SelectDragCommand::ResizeToCell) {
                    // We are supposed to resize a selection ...
                    if self.selection.is_none() {
                        // ... but there is no selection to resize.
                        return Ok(());
                    }
                }

                let maybe_initial_mouse_pos = MousePos::from_pixel(cursor_start, self.camera());
                let initial_mouse_pos = match maybe_initial_mouse_pos {
                    Some(pos) => pos,
                    None => return Ok(()),
                };

                let maybe_inner_drag_handler = selection_drag_handler(
                    c,
                    self.selection.as_ref().map(|sel| sel.rect.clone()),
                    initial_mouse_pos,
                );
                let mut inner_drag_handler = match maybe_inner_drag_handler {
                    Some(f) => f,
                    None => return Ok(()),
                };

                let new_drag_handler: DragHandler<Self> = Box::new(move |this, new_cursor_pos| {
                    if let Some(new_mouse_pos) = MousePos::from_pixel(new_cursor_pos, this.camera())
                    {
                        // Compute the new selection rectangle and update the
                        // selection.
                        let new_selection_rect = inner_drag_handler(&new_mouse_pos);
                        this.selection = new_selection_rect.map(Selection2D::from);
                    }
                    // Continue the drag action.
                    return Ok(true);
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
        // Draw mouse display.
        if let Some(mouse_pos) = self.mouse_pos() {
            match self.mouse().display {
                MouseDisplay::Draw => {
                    if !self.too_small_to_draw() {
                        rip.draw_hover_highlight(
                            mouse_pos.int_global(),
                            gridlines_width * 2.0,
                            crate::colors::HOVERED_DRAW,
                        )?;
                    }
                }
                MouseDisplay::Select => {
                    rip.draw_hover_highlight(
                        mouse_pos.int_global(),
                        gridlines_width * 2.0,
                        crate::colors::HOVERED_SELECT,
                    )?;
                }
                _ => (),
            }
        }
        // Draw selection.
        if let Some(selection) = &self.selection {
            rip.draw_selection_highlight(selection.rect.clone(), gridlines_width * 4.0)?;
        }
        // Draw selection preview after drawing selection.
        if self.mouse().display == MouseDisplay::ResizeSelectionAbsolute
            && self.drag_handler.is_none()
        {
            if let Some((mouse_pos, s)) = self.mouse_pos().zip(self.selection.as_ref()) {
                rip.draw_selection_resize_preview(
                    s.rect.clone(),
                    &mouse_pos,
                    gridlines_width * 2.0,
                )?;
            }
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

    fn mouse_pos(&self) -> Option<MousePos> {
        MousePos::from_pixel(self.mouse().pos?, self.camera())
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

#[derive(Debug, Clone)]
pub struct MousePos {
    pixel: FVec2D,
    global: FixedVec2D,
    int_global: BigVec2D,
    render_cell: FVec2D,
    int_render_cell: IVec2D,
}
impl MousePos {
    pub fn from_pixel(pixel: FVec2D, camera: &Camera2D) -> Option<Self> {
        let cell_transform = camera.cell_transform();
        let global = cell_transform.pixel_to_global_cell(pixel)?;
        let int_global = global.floor().0;
        let render_cell = cell_transform.pixel_to_local_render_cell(pixel)?;
        let int_render_cell = render_cell.floor().to_ivec();

        Some(Self {
            pixel,
            global,
            int_global,
            render_cell,
            int_render_cell,
        })
    }
    pub fn pixel(&self) -> FVec2D {
        self.pixel
    }
    pub fn global(&self) -> &FixedVec2D {
        &self.global
    }
    pub fn int_global(&self) -> &BigVec2D {
        &self.int_global
    }
    pub fn render_cell(&self) -> FVec2D {
        self.render_cell
    }
    pub fn int_render_cell(&self) -> IVec2D {
        self.int_render_cell
    }
}

pub fn selection_drag_handler(
    command: SelectDragCommand,
    initial_selection: Option<BigRect2D>,
    initial_mouse_pos: MousePos,
) -> Option<Box<dyn FnMut(&MousePos) -> Option<BigRect2D>>> {
    // TODO: at the time of writing, this method is marked as `pub` because it
    // is used by `render::grid2d`, but that should probably not be necessary.

    // Compute initial selection rectangle. `pos1` is fixed; `pos2` will change
    // with the mouse cursor.
    let pos1: BigVec2D;
    let pos2: BigVec2D;
    match command {
        SelectDragCommand::NewRect => {
            pos1 = initial_mouse_pos.int_global().clone();
            pos2 = initial_mouse_pos.int_global().clone();
        }
        SelectDragCommand::Resize { .. } | SelectDragCommand::ResizeToCell => {
            if let Some(sel) = &initial_selection {
                // Farthest corner stays fixed.
                pos1 = sel.farthest_corner(initial_mouse_pos.global());
                // Closest corner varies.
                pos2 = sel.closest_corner(initial_mouse_pos.global());
            } else {
                // Can't resize the selection if there is no selection!
                return None;
            }
        }
    };

    // Return the drag handler.
    Some(match command {
        SelectDragCommand::NewRect => {
            let mut moved = false;
            Box::new(move |new_mouse_pos| -> Option<BigRect2D> {
                // TODO: DPI-aware mouse movement threshold before making new
                // selection, then call drag handler before returning from
                // `do_select_command()`, in case threshold=0.
                if !moved {
                    if new_mouse_pos.pixel() != initial_mouse_pos.pixel() {
                        moved = true;
                    } else {
                        // Give the user a chance to remove their selection
                        // instead of making a new one.
                        return None;
                    }
                }

                let new_pos2 = new_mouse_pos.int_global();
                Some(NdRect::span(pos1.clone(), new_pos2.clone()))
            })
        }
        SelectDragCommand::Resize {
            x: resize_x,
            y: resize_y,
            ..
        } => {
            let old_pos2 = pos2;
            Box::new(move |new_mouse_pos| -> Option<BigRect2D> {
                // Use delta from original cursor position to new cursor
                // position.
                let cell_delta = new_mouse_pos.global() - initial_mouse_pos.global();

                // Only resize along some axes.
                let mut new_pos2 = old_pos2.clone();
                if resize_x {
                    new_pos2[X] += cell_delta[X].round().0;
                }
                if resize_y {
                    new_pos2[Y] += cell_delta[Y].round().0;
                }

                Some(NdRect::span(pos1.clone(), new_pos2.clone()))
            })
        }
        SelectDragCommand::ResizeToCell => {
            let old_pos2 = pos2;
            let (resize_x, resize_y) = absolute_selection_resize_axes(
                &initial_selection?.to_fixedrect(),
                &initial_mouse_pos.global(),
            );
            Box::new(move |new_mouse_pos| -> Option<BigRect2D> {
                // Only resize along some axes.
                let mut new_pos2 = old_pos2.clone();
                if resize_x {
                    new_pos2[X] = new_mouse_pos.int_global()[X].clone();
                }
                if resize_y {
                    new_pos2[Y] = new_mouse_pos.int_global()[Y].clone();
                }

                Some(NdRect::span(pos1.clone(), new_pos2.clone()))
            })
        }
    })
}

/// Returns a boolean `(resize_x, resize_y)` for absolute selection resizing
/// given the selection rectangle and the cursor position.
fn absolute_selection_resize_axes(
    selection: &FixedRect2D,
    cursor_pos: &FixedVec2D,
) -> (bool, bool) {
    let selection_min = selection.min();
    let selection_max = selection.max();

    // Measure signed distance from the nearest edge of the selection; distance
    // is zero at the selection edge, increases moving away from the selection
    // box, and decreases moving toward the center of the selection box.
    let x_edge_distance = std::cmp::max(
        cursor_pos[X].clone() - &selection_max[X],
        selection_min[X].clone() - &cursor_pos[X],
    );
    let y_edge_distance = std::cmp::max(
        cursor_pos[Y].clone() - &selection_max[Y],
        selection_min[Y].clone() - &cursor_pos[Y],
    );

    // If the cursor is beyond the corner of the selection (both edge distances
    // are positive), resize along both axes. Otherwise resize along whichever
    // one is most positive. If both are equal, favor X arbitrarily.
    (
        x_edge_distance.is_positive() || x_edge_distance >= y_edge_distance,
        y_edge_distance.is_positive() || y_edge_distance > x_edge_distance,
    )
}
