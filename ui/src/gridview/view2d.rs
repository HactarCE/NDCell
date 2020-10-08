use anyhow::Result;
use log::{trace, warn};
use parking_lot::RwLock;
use std::sync::Arc;

use ndcell_core::prelude::*;

use super::camera::{Camera, Camera2D, Interpolate, Interpolator};
use super::common::{GridViewCommon, GridViewTrait, RenderParams};
use super::history::{History, HistoryBase, HistoryManager};
use super::render::grid2d::{RenderCache, RenderInProgress};
use super::selection::Selection2D;
use super::worker::*;
use crate::clipboard_compat::{clipboard_get, clipboard_set};
use crate::commands::*;
use crate::config::Config;
use crate::Scale;

/// The number of render results to remember.
const RENDER_RESULTS_COUNT: usize = 4;
/// The number of previous simulation steps to track for counting UPS.
const MAX_LAST_SIM_TIMES: usize = 4;

/// Width of gridlines, in units of cells.
const GRIDLINE_WIDTH: f64 = 1.0 / 32.0;

type DrawDragHandler = Box<dyn FnMut(&mut GridView2D, FVec2D) -> Result<()>>;

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
    /// Pixel position of the mouse cursor from the top left of the area where
    /// the gridview is being drawn.
    cursor_pos: Option<FVec2D>,

    /// Communication channel with the simulation worker thread.
    worker: Option<Worker<ProjectedAutomaton2D>>,
    /// Cached render data unique to this GridView.
    render_cache: Option<RenderCache>,

    /// Non-movement drag handler.
    drag_handler: Option<DrawDragHandler>,
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
        if self.camera().scale() < Scale::from_factor(r64(1.0)) {
            self.common.is_drawing = false;
            self.drag_handler = None;
            return Ok(());
        }

        match command.0 {
            DragCommand::Start {
                action: (DrawDragAction { mode, shape }, selected_cell_state),
                cursor_start,
            } => {
                let cell_transform = self.camera().cell_transform();

                let initial_pos = cell_transform
                    .pixel_to_global_cell(cursor_start)
                    .map(|pos| pos.floor().0);

                let new_cell_state = match mode {
                    DrawMode::Place => selected_cell_state,
                    DrawMode::Replace => {
                        if let Some(pos) = &initial_pos {
                            if self.get_cell(&self.cache().read(), pos) == selected_cell_state {
                                0
                            } else {
                                selected_cell_state
                            }
                        } else {
                            selected_cell_state
                        }
                    }
                    DrawMode::Erase => 0,
                };

                let mut new_drag_handler: DrawDragHandler = match shape {
                    DrawShape::Freeform => {
                        let mut pos1 = initial_pos;
                        Box::new(move |this, new_cursor_pos| {
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
                            Ok(())
                        })
                    }
                    DrawShape::Line => todo!(),
                };

                self.stop_running();
                self.record();
                self.common.is_drawing = true;
                new_drag_handler(self, cursor_start)?;
                self.drag_handler = Some(new_drag_handler);
            }

            DragCommand::Continue { cursor_pos } => {
                if let Some(mut h) = self.drag_handler.take() {
                    h(self, cursor_pos)?;
                    self.drag_handler = Some(h);
                }
            }

            DragCommand::Stop => {
                self.common.is_drawing = false;
                self.drag_handler = None;
            }
        }
        Ok(())
    }
    fn do_select_command(&mut self, command: DragCommand<()>, config: &Config) -> Result<()> {
        match command {
            DragCommand::Start {
                action: (),
                cursor_start,
            } => {
                let cell_transform = self.camera().cell_transform();

                let initial_pos = cell_transform
                    .pixel_to_global_cell(cursor_start)
                    .map(|pos| pos.floor().0);

                let pos1 = initial_pos;
                let new_drag_handler: DrawDragHandler = Box::new(move |this, new_cursor_pos| {
                    let pos2 = this
                        .camera()
                        .cell_transform()
                        .pixel_to_global_cell(new_cursor_pos)
                        .map(|pos| pos.floor().0);
                    if let (Some(pos1), Some(pos2)) = (&pos1, &pos2) {
                        this.selection = Some(Selection2D {
                            rect: BigRect::span(pos1.clone(), pos2.clone()),
                            cells: None,
                        });
                    }
                    Ok(())
                });

                if !self.is_running() && config.hist.record_select {
                    self.record();
                }
                self.deselect();
                self.drag_handler = Some(new_drag_handler);
            }

            DragCommand::Continue { cursor_pos } => {
                if let Some(mut h) = self.drag_handler.take() {
                    h(self, cursor_pos)?;
                    self.drag_handler = Some(h);
                }
            }

            DragCommand::Stop => {
                self.common.is_drawing = false;
                self.drag_handler = None;
            }
        }
        Ok(())
    }
    fn do_clipboard_command(&mut self, command: ClipboardCommand, _config: &Config) -> Result<()> {
        if !self.is_drawing() {
            self.stop_running();
            match command {
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
    fn cursor_pos(&self) -> Option<FVec2D> {
        self.cursor_pos
    }
    fn render(
        &mut self,
        _config: &Config,
        target: &mut glium::Frame,
        params: RenderParams,
    ) -> Result<()> {
        let mut render_cache = std::mem::replace(&mut self.render_cache, None).unwrap_or_default();
        let node_cache = self.automaton.projected_cache().read();
        let mut rip = RenderInProgress::new(self, &node_cache, &mut render_cache, target)?;
        rip.draw_cells()?;

        self.cursor_pos = params.cursor_pos;
        let hover_pos = params
            .cursor_pos
            .and_then(|pos| rip.cell_transform().pixel_to_global_cell(pos));
        // Only allow drawing at 1:1 or bigger.
        let draw_pos = if self.camera().scale().log2_factor() >= 0.0 {
            hover_pos.clone()
        } else {
            None
        };

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
        if let Some(pos) = &draw_pos {
            rip.draw_blue_cursor_highlight(&pos.floor().0, gridlines_width * 2.0)?;
        }
        // Draw selection.
        if let Some(selection) = &self.selection {
            rip.draw_green_selection_highlight(selection.rect.clone(), gridlines_width * 4.0)?;
        }

        self.render_cache = Some(render_cache);
        Ok(())
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
            .pixel_to_global_cell(self.cursor_pos?)
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
