use anyhow::{anyhow, Result};

use ndcell_core::prelude::*;

use super::generic::{GenericGridView, GridViewDimension};
use super::screenpos::{OperationPos, OperationPosTrait, ScreenPosTrait};
use crate::commands::DragCmd;
use crate::mouse::MouseDisplayMode;
use crate::CONFIG;

/// Closure for mouse drag events, created when the user starts dragging and
/// called for each cursor movement until released. Returns whether to continue
/// or cancel the drag.
///
/// It takes as input the `Drag` struct, the current state (which is mutated),
/// and the latest mouse cursor position. If any initial state or mouse cursor
/// history is relevant, the closure must maintain this information.
pub type DragUpdateFn<D, T = GenericGridView<D>> =
    Box<dyn FnMut(&Drag<D>, &mut T, &<D as GridViewDimension>::ScreenPos) -> Result<DragOutcome>>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DragOutcome {
    Continue,
    Cancel,
}

pub struct Drag<D: GridViewDimension> {
    pub command: DragCmd,
    pub(super) initial_screen_pos: D::ScreenPos,
    pub waiting_for_drag_threshold: bool,

    pub(super) update_fn: Option<DragUpdateFn<D>>,

    pub(super) ndtree_base_pos: BigVec<D>,
}
impl<D: GridViewDimension> Drag<D> {
    /// Returns the way to display the mouse cursor during the drag.
    pub fn mouse_display_mode(&self) -> MouseDisplayMode {
        self.command.mouse_display_mode()
    }

    /// Returns the cell initially clicked on.
    pub fn initial(&self) -> Option<OperationPos<D>> {
        self.initial_screen_pos
            .op_pos_for_drag_command(&self.command)
    }
    /// Returns the cell dragged over at `new_screen_pos`.
    pub fn new(&self, new_screen_pos: &D::ScreenPos) -> Option<OperationPos<D>> {
        new_screen_pos.op_pos_for_continue_drag_command(&self.command, &self.initial_screen_pos)
    }

    /// Returns the rectangle of the render cell initially clicked on.
    pub fn initial_render_cell_rect(&self) -> Option<BigRect<D>> {
        self.initial().map(|op_pos| {
            self.initial_screen_pos.layer().round_rect_with_base_pos(
                BigRect::single_cell(op_pos.cell().clone()),
                &self.ndtree_base_pos,
            )
        })
    }
    /// Returns the rectangle of the render cell dragged over at
    /// `new_screen_pos`.
    pub fn new_render_cell_rect(&self, new_screen_pos: &D::ScreenPos) -> Option<BigRect<D>> {
        self.new(new_screen_pos).map(|op_pos| {
            self.initial_screen_pos.layer().round_rect_with_base_pos(
                BigRect::single_cell(op_pos.cell().clone()),
                &self.ndtree_base_pos,
            )
        })
    }

    /// Returns cell highlight data for the cell dragged over at
    /// `new_screen_pos`.
    pub fn cell_highlight(&self, new_screen_pos: &D::ScreenPos) -> Option<OperationPos<D>> {
        new_screen_pos.op_pos_for_continue_drag_command(&self.command, &self.initial_screen_pos)
    }

    /// Updates a gridview for a continuation of the drag.
    pub fn update(
        &mut self,
        gridview: &mut GenericGridView<D>,
        new_screen_pos: &D::ScreenPos,
    ) -> Result<DragOutcome> {
        let config = CONFIG.lock();
        if self.waiting_for_drag_threshold {
            if *(new_screen_pos.pixel() - self.initial_screen_pos.pixel())
                .abs()
                .max_component()
                < config.mouse.drag_threshold
            {
                return Ok(DragOutcome::Continue);
            } else {
                self.waiting_for_drag_threshold = false;
            }
        }
        drop(config); // `CONFIG` might be used by `update_fn`

        let mut update_fn = self
            .update_fn
            .take()
            .ok_or(anyhow!("Drag update function mysteriously vanished"))?;
        let result = update_fn(self, gridview, new_screen_pos);
        self.update_fn = Some(update_fn);
        result
    }
}
