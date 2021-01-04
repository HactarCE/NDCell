use anyhow::Result;
use enum_dispatch::enum_dispatch;
use std::collections::VecDeque;
use std::time::Duration;

use ndcell_core::prelude::*;

mod camera;
mod generic;
mod history;
pub mod render;
mod selection;
mod view2d;
mod view3d;
mod worker;

use crate::commands::*;
pub use camera::*;
pub use generic::GenericGridView;
pub use history::History;
pub use render::{MouseTargetData, RenderParams, RenderResult};
pub use selection::*;
pub use view2d::GridView2D;
pub use view3d::GridView3D;

/// Handler for mouse drag events, when the user starts dragging and called for
/// each cursor movement until released. Returns whether to continue or cancel
/// the drag.
///
/// It takes as input the current state, which is mutated, and the current mouse
/// cursor position. If any initial state or mouse cursor history is relevant,
/// the closure must maintain this information.
pub type DragHandler<G> = Box<dyn FnMut(&mut G, FVec2D) -> Result<DragOutcome>>;

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
    pub fn is_drawing(&self) -> bool {
        match self {
            GridView::View2D(view2d) => view2d.is_drawing(),
            GridView::View3D(view3d) => view3d.is_drawing(),
        }
    }
    pub fn is_dragging_view(&self) -> bool {
        match self {
            GridView::View2D(view2d) => view2d.is_dragging_view(),
            GridView::View3D(view3d) => view3d.is_dragging_view(),
        }
    }
    pub fn is_running(&self) -> bool {
        match self {
            GridView::View2D(view2d) => view2d.is_running(),
            GridView::View3D(view3d) => view3d.is_running(),
        }
    }

    /// Enqueues a command to be executed on the next frame.
    pub fn enqueue(&self, command: impl Into<Command>) {
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
    /// Updates camera parameters and renders the gridview, recording and
    /// returning the result.
    pub fn render(&mut self, params: RenderParams<'_>) -> Result<&RenderResult> {
        match self {
            GridView::View2D(view2d) => view2d.render(params),
            GridView::View3D(view3d) => view3d.render(params),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum WorkType {
    SimStep,
    SimContinuous,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DragType {
    MovingView,
    Drawing,
    Selecting,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DragOutcome {
    Continue,
    Cancel,
}
