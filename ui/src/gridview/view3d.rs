use anyhow::{anyhow, Context, Result};

use ndcell_core::prelude::*;

use super::camera::{Camera, Camera3D};
use super::generic::{GenericGridView, GridViewDimension, RenderParams, RenderResult};
use super::render::grid3d::RenderInProgress;
use super::DragHandler;
use crate::commands::*;
use crate::config::Config;

pub type GridView3D = GenericGridView<GridViewDim3D>;

#[derive(Debug, Default)]
pub struct GridViewDim3D;
impl GridViewDimension for GridViewDim3D {
    type D = Dim3D;
    type Camera = Camera3D;

    fn do_view_command(this: &mut GridView3D, command: ViewCommand, config: &Config) -> Result<()> {
        let maybe_new_drag_handler = this
            .camera_interpolator
            .do_view_command(command, config)
            .context("Executing view command")?
            .map(|mut interpolator_drag_handler| {
                Box::new(move |gridview: &mut GridView3D, cursor_pos| {
                    interpolator_drag_handler(&mut gridview.camera_interpolator, cursor_pos)
                }) as DragHandler<GridView3D>
            });
        if maybe_new_drag_handler.is_some() && this.drag_handler.is_none() {
            this.drag_handler = maybe_new_drag_handler;
            this.is_dragging_view = true;
        }
        Ok(())
    }
    fn do_draw_command(
        _this: &mut GridView3D,
        _command: DrawCommand,
        _config: &Config,
    ) -> Result<()> {
        Err(anyhow!("unimplemented"))
    }
    fn do_select_command(
        _this: &mut GridView3D,
        _command: SelectCommand,
        _config: &Config,
    ) -> Result<()> {
        Err(anyhow!("unimplemented"))
    }

    fn render(this: &mut GridView3D, params: RenderParams<'_>) -> Result<RenderResult> {
        let mut rip = RenderInProgress::new(this, params)?;
        rip.draw_cells();

        // let hover_pos = params.cursor_pos.map(|pos| rip.pixel_pos_to_cell_pos(pos));
        // // Only allow drawing at 1:1 or bigger.
        // let draw_pos = if this.interpolating_viewport.zoom.power() >= 0.0 {
        //     hover_pos.clone()
        // } else {
        //     None
        // };

        // // Draw gridlines.
        // let gridlines_width = this.interpolating_viewport.zoom.factor() / 32.0;
        // rip.draw_gridlines(gridlines_width);
        // // Draw crosshairs.
        // if let Some(pos) = &draw_pos {
        //     rip.draw_blue_cursor_highlight(&pos.floor().0, gridlines_width * 2.0);
        // }

        rip.finish()
    }
}
impl GridView3D {
    /// Returns the cell position underneath the cursor. Floor this to get an
    /// integer cell position.
    pub fn hovered_cell_pos(&self) -> Option<FixedVec3D> {
        // TODO: Raycast or something
        let z = Camera3D::DISTANCE_TO_PIVOT;
        self.camera()
            .cell_transform()
            .pixel_to_global_cell(self.mouse.pos?, z)
    }
}
