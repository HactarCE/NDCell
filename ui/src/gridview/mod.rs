use anyhow::Result;
use enum_dispatch::enum_dispatch;
use std::collections::VecDeque;
use std::time::Duration;

use ndcell_core::prelude::*;

mod camera;
mod common;
mod history;
mod render;
mod selection;
mod view2d;
mod view3d;
mod worker;

use crate::commands::*;
use crate::config::Config;
pub use camera::*;
use common::GridViewCommon;
pub use common::{GridViewTrait, MouseState, MouseTargetData, RenderParams, RenderResult};
pub use history::History;
pub use render::post_frame_clean_render_cache;
pub use selection::*;
pub use view2d::GridView2D;
pub use view3d::GridView3D;
use worker::WorkerRequest;

/// Handler for mouse drag events, when the user starts dragging and called for
/// each cursor movement until released. Returns whether to continue or cancel
/// the drag.
///
/// It takes as input the current state, which is mutated, and the current mouse
/// cursor position. If any initial state or mouse cursor history is relevant,
/// the closure must maintain this information.
pub type DragHandler<G> = Box<dyn FnMut(&mut G, FVec2D) -> Result<DragOutcome>>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DragOutcome {
    Continue,
    Cancel,
}

/// Abstraction over 2D and 3D gridviews.
#[enum_dispatch(GridViewTrait, History)]
pub enum GridView {
    View2D(pub GridView2D),
    View3D(pub GridView3D),
}
/// Conversions from an `NdAutomaton` to a `GridView`.
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

impl GridView {
    /// Helper method to make `impl History for GridView` easier.
    fn as_history(&mut self) -> &mut dyn History {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }

    /// Returns whether the `GridView` is 2D.
    pub fn is_2d(&self) -> bool {
        match self {
            GridView::View2D(_) => true,
            GridView::View3D(_) => false,
        }
    }

    /// Returns whether the `GridView` is 3D.
    pub fn is_3d(&self) -> bool {
        match self {
            GridView::View2D(_) => false,
            GridView::View3D(_) => true,
        }
    }
}

impl AsRef<GridViewCommon> for GridView {
    fn as_ref(&self) -> &GridViewCommon {
        match self {
            GridView::View2D(view2d) => view2d.as_ref(),
            GridView::View3D(view3d) => view3d.as_ref(),
        }
    }
}
impl AsMut<GridViewCommon> for GridView {
    fn as_mut(&mut self) -> &mut GridViewCommon {
        match self {
            GridView::View2D(view2d) => view2d.as_mut(),
            GridView::View3D(view3d) => view3d.as_mut(),
        }
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
