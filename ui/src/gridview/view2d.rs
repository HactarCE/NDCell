use anyhow::{anyhow, Context, Result};
use log::{trace, warn};
use parking_lot::RwLock;
use std::borrow::Cow;
use std::sync::Arc;

use ndcell_core::prelude::*;

use super::commands::*;
use super::render::grid2d::{RenderCache, RenderInProgress};
use super::worker::*;
use super::{
    Camera, Camera2D, GridViewCommon, GridViewTrait, Interpolate, Interpolator, RenderParams,
    RenderResult,
};
use crate::clipboard_compat::{clipboard_get, clipboard_set};
use crate::config::Config;
use crate::history::{History, HistoryManager};

/// The number of render results to remember.
const RENDER_RESULTS_COUNT: usize = 4;
/// The number of previous simulation steps to track for counting UPS.
const MAX_LAST_SIM_TIMES: usize = 4;

#[derive(Default)]
pub struct GridView2D {
    common: GridViewCommon,

    /// Automaton being simulated and displayed.
    pub automaton: ProjectedAutomaton2D,
    /// Camera interpolator.
    camera_interpolator: Interpolator<Dim2D, Camera2D>,

    /// List of undo states.
    undo_stack: Vec<HistoryEntry>,
    /// List of redo states.
    redo_stack: Vec<HistoryEntry>,

    /// Communication channel with the simulation worker thread.
    worker: Option<Worker<ProjectedAutomaton2D>>,
    /// Cached render data unique to this GridView.
    render_cache: Option<RenderCache>,
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
    fn do_move_command(
        &mut self,
        command: MoveCommand,
        interpolation: Interpolation,
        config: &Config,
    ) -> Result<()> {
        let mut new_viewport = match interpolation {
            Interpolation::Direct => self.camera().clone(),
            Interpolation::Decay => self.camera_interpolator.target.clone(),
        };

        match command {
            MoveCommand::SnapPos => {
                if config.ctrl.snap_pos_2d {
                    new_viewport.snap_pos()
                }
            }

            MoveCommand::Scale { log2_factor } => {
                new_viewport.scale_by_log2_factor(log2_factor, None)
            }
            MoveCommand::SetScale { scale } => new_viewport.scale_to(scale, None),
            MoveCommand::SnapScale => new_viewport.snap_scale(None),

            MoveCommand::Move2D(c) => {
                match c {
                    MoveCommand2D::PanPixels(delta) => new_viewport.pan_pixels(delta),
                    MoveCommand2D::SetPos(pos) => new_viewport.set_pos(pos),

                    MoveCommand2D::Scale {
                        log2_factor,
                        invariant_pos,
                    } => new_viewport.scale_by_factor(log2_factor.exp2(), invariant_pos),
                    MoveCommand2D::SetScale {
                        scale,
                        invariant_pos,
                    } => new_viewport.scale_to(scale, invariant_pos),
                    MoveCommand2D::SnapScale { invariant_pos } => {
                        if config.ctrl.snap_scale_2d {
                            new_viewport.snap_scale(invariant_pos)
                        }
                    }
                };
            }

            MoveCommand::Move3D(_) => warn!("Ignoring {:?} in GridView2D", command),
        }

        self.camera_interpolator.target = new_viewport;
        match interpolation {
            Interpolation::Direct => {
                self.camera_interpolator.current = self.camera_interpolator.target.clone();
            }
            Interpolation::Decay => (),
        };
        Ok(())
    }
    fn do_draw_command(&mut self, command: DrawCommand, _config: &Config) -> Result<()> {
        match command {
            DrawCommand::Start => {
                self.stop_running();
                self.record();
                self.common.is_drawing = true;
            }
            DrawCommand::End => self.common.is_drawing = false,
            DrawCommand::Draw2D(c) => {
                if !self.is_drawing() {
                    return Err(anyhow!(
                        "Attempt to execute draw command before {:?}",
                        DrawCommand::Start,
                    ));
                }
                if self.is_running() {
                    return Err(anyhow!(
                        "Attempt to execute draw command while simulation is running",
                    ));
                }
                match c {
                    DrawCommand2D::Cell(pos, cell_state) => {
                        self.automaton.set_cell(&pos, cell_state)
                    }
                    DrawCommand2D::Line(pos1, pos2, cell_state) => {
                        for pos in ndcell_core::math::bresenham(pos1, pos2) {
                            self.automaton.set_cell(&pos, cell_state);
                        }
                    }
                }
            }
            DrawCommand::Draw3D(_) => warn!("Ignoring {:?} in GridView2D", command),
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
    fn render(
        &mut self,
        _config: &Config,
        target: &mut glium::Frame,
        params: RenderParams,
    ) -> Result<Cow<RenderResult>> {
        let mut render_cache = std::mem::replace(&mut self.render_cache, None).unwrap_or_default();
        let node_cache = self.automaton.projected_cache().read();
        let mut rip = RenderInProgress::new(self, &node_cache, &mut render_cache, target)?;
        rip.draw_cells()?;

        let hover_pos = params.cursor_pos.map(|pos| rip.pixel_pos_to_cell_pos(pos));
        // Only allow drawing at 1:1 or bigger.
        let draw_pos = if self.camera().scale().log2_factor() >= 0.0 {
            hover_pos.clone()
        } else {
            None
        };

        // Draw gridlines.
        // TODO: rewrite to avoid conversion to f64.
        let gridlines_width = (self.camera().scale().factor() / 32.0)
            .to_f64()
            .context("Computing gridline width")?;
        rip.draw_gridlines(gridlines_width)?;
        // Draw crosshairs.
        if let Some(pos) = &draw_pos {
            rip.draw_blue_cursor_highlight(&pos.floor().0, gridlines_width * 2.0)?;
        }

        drop(node_cache);
        self.render_cache = Some(render_cache);
        Ok(self.push_render_result(RenderResult {
            hover_pos: hover_pos.map(|v| v.into()),
            draw_pos: draw_pos.map(|v| v.into()),
            ..Default::default()
        }))
    }
}

impl GridView2D {
    /// Returns the current camera.
    pub fn camera(&self) -> &Camera2D {
        &self.camera_interpolator.current
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
}

impl HistoryManager for GridView2D {
    type HistoryEntry = HistoryEntry;
    fn history_entry(&self) -> HistoryEntry {
        HistoryEntry {
            automaton: self.automaton.clone(),
        }
    }
    fn restore(&mut self, entry: HistoryEntry) -> HistoryEntry {
        HistoryEntry {
            // Replace automaton, but keep camera, cache, and everything else.
            automaton: std::mem::replace(&mut self.automaton, entry.automaton),
        }
    }
    fn undo_stack(&mut self) -> &mut Vec<HistoryEntry> {
        &mut self.undo_stack
    }
    fn redo_stack(&mut self) -> &mut Vec<HistoryEntry> {
        &mut self.redo_stack
    }
}
