use anyhow::{anyhow, Context, Result};

use ndcell_core::prelude::*;

use super::camera::{Camera, Camera3D, Interpolate, Interpolator};
use super::common::{GridViewCommon, GridViewTrait, RenderParams, RenderResult};
use super::history::{HistoryBase, HistoryManager};
use super::render::grid3d::{RenderCache, RenderInProgress};
use super::selection::Selection3D;
use super::worker::*;
use super::DragHandler;
use crate::commands::*;
use crate::config::Config;

/// The number of render results to remember.
const RENDER_RESULTS_COUNT: usize = 4;
/// The number of previous simulation steps to track for counting UPS.
const MAX_LAST_SIM_TIMES: usize = 4;

#[derive(Default)]
pub struct GridView3D {
    common: GridViewCommon,

    /// Automaton being simulated and displayed.
    pub automaton: Automaton3D,
    /// Selection.
    selection: Option<Selection3D>,
    /// Undo/redo history manager.
    history: HistoryManager<HistoryEntry>,

    /// Camera interpolator.
    camera_interpolator: Interpolator<Dim3D, Camera3D>,

    /// Communication channel with the simulation worker thread.
    worker: Option<Worker<Automaton3D>>,
    /// Cached render data unique to this GridView.
    render_cache: Option<RenderCache>,

    /// Mouse drag handler.
    drag_handler: Option<DragHandler<Self>>,
}
impl AsRef<GridViewCommon> for GridView3D {
    fn as_ref(&self) -> &GridViewCommon {
        &self.common
    }
}
impl AsMut<GridViewCommon> for GridView3D {
    fn as_mut(&mut self) -> &mut GridViewCommon {
        &mut self.common
    }
}

impl GridViewTrait for GridView3D {
    fn do_draw_command(&mut self, _command: DrawCommand, _config: &Config) -> Result<()> {
        Err(anyhow!("unimplemented"))
    }
    fn do_select_command(&mut self, _command: SelectCommand, _config: &Config) -> Result<()> {
        Err(anyhow!("unimplemented"))
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
            h(self, cursor_pos)?;
            self.drag_handler = Some(h);
        }
        Ok(())
    }
    fn stop_drag(&mut self) -> Result<()> {
        self.drag_handler = None;
        self.common.is_drawing = false;
        self.common.is_dragging_view = false;
        Ok(())
    }

    fn enqueue_worker_request(&mut self, _request: WorkerRequest) {
        todo!()
    }
    fn reset_worker(&mut self) {
        todo!()
    }

    fn export(&self, format: CaFormat) -> Result<String, CaFormatError> {
        ndcell_core::io::export_ndautomaton_to_string(&self.automaton, format, None)
    }

    fn run_step(&mut self) {
        todo!()
    }

    fn camera_interpolator(&mut self) -> &mut dyn Interpolate {
        &mut self.camera_interpolator
    }
    fn render(&mut self, params: RenderParams<'_>) -> Result<&RenderResult> {
        let config = params.config;

        // Update DPI.
        self.camera_interpolator().set_dpi(config.gfx.dpi as f32);

        // let mut render_cache = std::mem::replace(&mut self.render_cache, None).unwrap_or_default();
        let mut rip = RenderInProgress::new(
            self, params,
            // &mut render_cache,
        )?;
        rip.draw_cells();

        // let hover_pos = params.cursor_pos.map(|pos| rip.pixel_pos_to_cell_pos(pos));
        // // Only allow drawing at 1:1 or bigger.
        // let draw_pos = if self.interpolating_viewport.zoom.power() >= 0.0 {
        //     hover_pos.clone()
        // } else {
        //     None
        // };

        // // Draw gridlines.
        // let gridlines_width = self.interpolating_viewport.zoom.factor() / 32.0;
        // rip.draw_gridlines(gridlines_width);
        // // Draw crosshairs.
        // if let Some(pos) = &draw_pos {
        //     rip.draw_blue_cursor_highlight(&pos.floor().0, gridlines_width * 2.0);
        // }

        // self.common.last_render_result = rip.finish();
        // self.render_cache = Some(render_cache);
        Ok(self.last_render_result())
    }
}

impl GridView3D {
    /// Returns the current camera.
    pub fn camera(&self) -> &Camera3D {
        &self.camera_interpolator.current
    }
    /// Returns the cell position underneath the cursor. Floor this to get an
    /// integer cell position.
    pub fn hovered_cell_pos(&self) -> Option<FixedVec3D> {
        // TODO: Raycast or something
        let z = Camera3D::DISTANCE_TO_PIVOT;
        self.camera()
            .cell_transform()
            .pixel_to_global_cell(self.common.mouse.pos?, z)
    }
}

impl AsSimulate for GridView3D {
    fn as_sim(&self) -> &dyn Simulate {
        &self.automaton
    }
    fn as_sim_mut(&mut self) -> &mut dyn Simulate {
        &mut self.automaton
    }
}

impl From<Automaton3D> for GridView3D {
    fn from(automaton: Automaton3D) -> Self {
        Self {
            automaton,
            ..Default::default()
        }
    }
}

pub struct HistoryEntry {
    automaton: Automaton3D,
    selection: Option<Selection3D>,
    camera: Camera3D,
}

impl HistoryBase for GridView3D {
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
            selection: Selection3D::restore_history_entry(
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
