use anyhow::Result;
use enum_dispatch::enum_dispatch;
use std::collections::VecDeque;
use std::sync::Arc;
use std::time::Duration;

use ndcell_core::prelude::*;

mod algorithms;
mod drag;
mod generic;
mod history;
pub mod render;
mod screenpos;
mod selection;
mod view2d;
mod view3d;
mod viewpoint;
mod worker;

use crate::commands::{CmdMsg, DragCmd};
pub use generic::GenericGridView;
pub use history::History;
pub use render::{MouseTargetData, RenderParams, RenderResult};
pub use screenpos::*;
pub use selection::*;
pub use view2d::GridView2D;
pub use view3d::GridView3D;
pub use viewpoint::*;

/// Abstraction over 2D and 3D gridviews.
#[enum_dispatch(History)]
pub enum GridView {
    View2D(pub GridView2D),
    View3D(pub GridView3D),
}

// Conversions from an `NdAutomaton` to a `GridView`.
impl From<Automaton2D> for GridView {
    fn from(automaton: Automaton2D) -> Self {
        Self::View2D(GridView2D::from(automaton))
    }
}
impl From<Automaton3D> for GridView {
    fn from(automaton: Automaton3D) -> Self {
        Self::View3D(GridView3D::from(automaton))
    }
}

impl AsSimulate for GridView {
    fn as_sim(&self) -> &dyn Simulate {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
    fn as_sim_mut(&mut self) -> &mut dyn Simulate {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
}

impl GridView {
    pub fn is_2d(&self) -> bool {
        match self {
            GridView::View2D(_) => true,
            GridView::View3D(_) => false,
        }
    }
    pub fn is_3d(&self) -> bool {
        match self {
            GridView::View2D(_) => false,
            GridView::View3D(_) => true,
        }
    }

    pub fn rule(&self) -> Rule {
        match self {
            GridView::View2D(view2d) => Rule::Rule2D(Arc::clone(&view2d.automaton.rule)),
            GridView::View3D(view3d) => Rule::Rule3D(Arc::clone(&view3d.automaton.rule)),
        }
    }

    pub fn selected_cell_state(&self) -> u8 {
        match self {
            GridView::View2D(view2d) => view2d.selected_cell_state,
            GridView::View3D(view3d) => view3d.selected_cell_state,
        }
    }

    pub fn frame_duration(&self) -> Option<Duration> {
        match self {
            GridView::View2D(view2d) => view2d.frame_duration(),
            GridView::View3D(view3d) => view3d.frame_duration(),
        }
    }
    pub fn last_render_result(&self) -> &RenderResult {
        match self {
            GridView::View2D(view2d) => view2d.last_render_result(),
            GridView::View3D(view3d) => view3d.last_render_result(),
        }
    }
    pub fn last_sim_times(&self) -> &VecDeque<Duration> {
        match self {
            GridView::View2D(view2d) => view2d.last_sim_times(),
            GridView::View3D(view3d) => view3d.last_sim_times(),
        }
    }

    pub fn work_type(&self) -> Option<WorkType> {
        match self {
            GridView::View2D(view2d) => view2d.work_type(),
            GridView::View3D(view3d) => view3d.work_type(),
        }
    }
    pub fn is_dragging(&self) -> bool {
        match self {
            GridView::View2D(view2d) => view2d.is_dragging(),
            GridView::View3D(view3d) => view3d.is_dragging(),
        }
    }
    pub fn drag_cmd(&self) -> Option<&DragCmd> {
        match self {
            GridView::View2D(view2d) => view2d.get_drag().map(|d| &d.command),
            GridView::View3D(view3d) => view3d.get_drag().map(|d| &d.command),
        }
    }
    pub fn is_drawing(&self) -> bool {
        match self {
            GridView::View2D(view2d) => view2d.is_drawing(),
            GridView::View3D(view3d) => view3d.is_drawing(),
        }
    }
    pub fn is_running(&self) -> bool {
        match self {
            GridView::View2D(view2d) => view2d.is_running(),
            GridView::View3D(view3d) => view3d.is_running(),
        }
    }

    /// Enqueues a command to be executed on the next frame.
    pub fn enqueue(&self, command: impl Into<CmdMsg>) {
        match self {
            GridView::View2D(view2d) => view2d.enqueue(command),
            GridView::View3D(view3d) => view3d.enqueue(command),
        }
    }
    /// Does all the frame things: executes commands, advances the simulation,
    /// etc.
    pub fn do_frame(&mut self) -> Result<()> {
        match self {
            GridView::View2D(view2d) => view2d.do_frame(),
            GridView::View3D(view3d) => view3d.do_frame(),
        }
    }
    /// Updates viewpoint parameters and renders the gridview, recording and
    /// returning the result.
    pub fn render(&mut self, params: RenderParams<'_>) -> Result<&RenderResult> {
        match self {
            GridView::View2D(view2d) => view2d.render(params),
            GridView::View3D(view3d) => view3d.render(params),
        }
    }
    /// Exports the simulation to a string.
    pub fn export(&self, format: CaFormat) -> Result<String, CaFormatError> {
        match self {
            GridView::View2D(view2d) => view2d.export(format),
            GridView::View3D(view3d) => view3d.export(format),
        }
    }
    /// Returns whether there is a selection.
    pub fn has_selection(&self) -> bool {
        match self {
            GridView::View2D(view2d) => view2d.selection.is_some(),
            GridView::View3D(view3d) => view3d.selection.is_some(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum WorkType {
    SimStep,
    SimContinuous,
}
