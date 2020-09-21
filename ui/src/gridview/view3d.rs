use anyhow::{anyhow, Result};
use log::warn;
use std::borrow::Cow;
use std::collections::VecDeque;
use std::sync::{mpsc, Mutex};
use std::time::{Duration, Instant};

use ndcell_core::prelude::*;

use super::commands::*;
use super::render::grid3d::{RenderCache, RenderInProgress};
use super::worker::*;
use super::{Camera, Camera3D, GridViewCommon, GridViewTrait, Interpolator, RenderResult};
use crate::config::Config;
use crate::history::HistoryManager;

/// The number of render results to remember.
const RENDER_RESULTS_COUNT: usize = 4;
/// The number of previous simulation steps to track for counting UPS.
const MAX_LAST_SIM_TIMES: usize = 4;

#[derive(Default)]
pub struct GridView3D {
    common: GridViewCommon,

    /// Automaton being simulated and displayed.
    pub automaton: ProjectedAutomaton3D,
    /// Camera interpolator.
    camera_interpolator: Interpolator<Dim3D, Camera3D>,

    /// List of undo states.
    undo_stack: Vec<HistoryEntry>,
    /// List of redo states.
    redo_stack: Vec<HistoryEntry>,

    /// Communication channel with the simulation worker thread.
    worker: Option<Worker<ProjectedAutomaton3D>>,
    /// Cached render data unique to this GridView.
    render_cache: Option<RenderCache>,
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
    fn do_move_command(&mut self, command: MoveCommand, config: &Config) -> Result<()> {
        match command {
            MoveCommand::Move2D(_, _) => warn!("Ignoring {:?} in GridView3D", command),
            MoveCommand::Move3D(c, interpolation) => {
                let mut new_viewport = match interpolation {
                    Interpolation::Direct => self.camera().clone(),
                    Interpolation::Decay => self.camera_interpolator.target.clone(),
                };

                match c {
                    MoveCommand3D::PanPixels { start, end } => {}
                    MoveCommand3D::SetPos(_) => {}
                    MoveCommand3D::SnapPos => {}

                    MoveCommand3D::RotPixels(_) => {}
                    MoveCommand3D::SetPitch(_) => {}
                    MoveCommand3D::SetYaw(_) => {}

                    MoveCommand3D::Scale {
                        log2_factor,
                        invariant_pos,
                    } => new_viewport.scale_by_factor(log2_factor.exp2(), invariant_pos),
                    MoveCommand3D::SetScale {
                        scale,
                        invariant_pos,
                    } => new_viewport.scale_to(scale, invariant_pos),
                    MoveCommand3D::SnapScale { invariant_pos } => {
                        if config.ctrl.snap_scale {
                            new_viewport.snap_scale(invariant_pos)
                        }
                    }
                }
                self.camera_interpolator.target = new_viewport;
                match interpolation {
                    Interpolation::Direct => {
                        self.camera_interpolator.current = self.camera_interpolator.target.clone();
                    }
                    Interpolation::Decay => (),
                };
            }
        }
        Ok(())
    }
    fn do_draw_command(&mut self, command: DrawCommand, config: &Config) -> Result<()> {
        // Err(anyhow!("unimplemented"))
        Ok(())
    }
    fn do_clipboard_command(&mut self, command: ClipboardCommand, config: &Config) -> Result<()> {
        Err(anyhow!("unimplemented"))
    }

    fn enqueue_worker_request(&mut self, request: WorkerRequest) {
        todo!()
    }
    fn reset_worker(&mut self) {
        todo!()
    }

    fn as_automaton<'a>(&'a self) -> AutomatonRef<'a> {
        AutomatonRef::from(&self.automaton)
    }
    fn as_automaton_mut<'a>(&'a mut self) -> AutomatonMut<'a> {
        AutomatonMut::from(&mut self.automaton)
    }

    fn run_step(&mut self) {
        todo!()
    }

    fn camera_interpolator(&mut self) -> &mut dyn super::Interpolate {
        &mut self.camera_interpolator
    }
    fn render(
        &mut self,
        _config: &Config,
        target: &mut glium::Frame,
        _params: super::RenderParams,
    ) -> Result<Cow<super::RenderResult>> {
        // let mut render_cache = std::mem::replace(&mut self.render_cache, None).unwrap_or_default();
        let node_cache = self.automaton.projected_cache().read();
        let mut rip = RenderInProgress::new(
            self,
            &node_cache,
            // &mut render_cache,
            target,
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

        drop(node_cache);
        // self.render_cache = Some(render_cache);
        Ok(self.push_render_result(RenderResult {
            // hover_pos,
            // draw_pos,
            ..Default::default()
        }))
    }
}

impl GridView3D {
    /// Returns the current camera.
    pub fn camera(&self) -> &Camera3D {
        &self.camera_interpolator.current
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

impl From<ProjectedAutomaton3D> for GridView3D {
    fn from(automaton: ProjectedAutomaton3D) -> Self {
        Self {
            automaton,
            ..Default::default()
        }
    }
}
impl From<Automaton3D> for GridView3D {
    fn from(automaton: Automaton3D) -> Self {
        Self::from(ProjectedAutomaton3D::from(automaton))
    }
}

pub struct HistoryEntry {}

impl HistoryManager for GridView3D {
    type HistoryEntry = HistoryEntry;
    fn history_entry(&self) -> HistoryEntry {
        unimplemented!()
    }
    fn restore(&mut self, _entry: HistoryEntry) -> HistoryEntry {
        unimplemented!()
    }
    fn undo_stack(&mut self) -> &mut Vec<HistoryEntry> {
        unimplemented!()
    }
    fn redo_stack(&mut self) -> &mut Vec<HistoryEntry> {
        unimplemented!()
    }
}
