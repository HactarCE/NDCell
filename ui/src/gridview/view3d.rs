use anyhow::{anyhow, Context, Result};

use ndcell_core::prelude::*;

use super::camera::{Camera, Camera3D};
use super::generic::{GenericGridView, GridViewDimension};
use super::render::{CellDrawParams, GridViewRender3D, RenderParams, RenderResult};
use super::{DragHandler, DragType};
use crate::commands::*;

pub type GridView3D = GenericGridView<GridViewDim3D>;

#[derive(Debug, Default)]
pub struct GridViewDim3D;
impl GridViewDimension for GridViewDim3D {
    type D = Dim3D;
    type Camera = Camera3D;

    fn do_view_command(this: &mut GridView3D, command: ViewCommand) -> Result<()> {
        // Delegate to the camera.
        let maybe_new_drag_handler = this
            .camera_interpolator
            .do_view_command(command)
            .context("Executing view command")?;

        // Update drag handler, if the camera gave one.
        if !this.is_dragging() {
            if let Some(mut interpolator_drag_handler) = maybe_new_drag_handler {
                this.start_drag(
                    DragType::MovingView,
                    Box::new(move |gridview: &mut GridView3D, cursor_pos| {
                        interpolator_drag_handler(&mut gridview.camera_interpolator, cursor_pos)
                    }) as DragHandler<GridView3D>,
                );
            }
        }

        Ok(())
    }
    fn do_draw_command(_this: &mut GridView3D, _command: DrawCommand) -> Result<()> {
        Err(anyhow!("unimplemented"))
    }
    fn do_select_command(_this: &mut GridView3D, _command: SelectCommand) -> Result<()> {
        Err(anyhow!("unimplemented"))
    }

    fn render(this: &mut GridView3D, params: RenderParams<'_>) -> Result<RenderResult> {
        let mut frame = GridViewRender3D::new(params, this.camera())?;
        frame.draw_cells(CellDrawParams {
            ndtree: &this.automaton.ndtree,
            alpha: 1.0,
            rect: None,
        })?;

        // let hover_pos = params.cursor_pos.map(|pos| frame.pixel_pos_to_cell_pos(pos));
        // // Only allow drawing at 1:1 or bigger.
        // let draw_pos = if this.interpolating_viewport.zoom.power() >= 0.0 {
        //     hover_pos.clone()
        // } else {
        //     None
        // };

        // // Draw gridlines.
        // let gridlines_width = this.interpolating_viewport.zoom.factor() / 32.0;
        // frame.draw_gridlines(gridlines_width);
        // // Draw crosshairs.
        // if let Some(pos) = &draw_pos {
        //     frame.draw_blue_cursor_highlight(&pos.floor().0, gridlines_width * 2.0);
        // }

        frame.finish()
    }
}
impl GridView3D {
    /// Returns the cell position underneath the cursor. Floor this to get an
    /// integer cell position.
    pub fn hovered_cell_pos(&self, mouse_pos: Option<FVec2D>) -> Option<FixedVec3D> {
        // TODO: Raycast or something
        let z = Camera3D::DISTANCE_TO_PIVOT;
        mouse_pos.and_then(|pos| self.camera().cell_transform().pixel_to_global_cell(pos, z))
    }
}
