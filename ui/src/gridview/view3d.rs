use anyhow::{anyhow, Context, Result};

use ndcell_core::prelude::*;

use super::generic::{GenericGridView, GridViewDimension};
use super::render::{CellDrawParams, GridViewRender3D, RenderParams, RenderResult};
use super::viewpoint::{CellTransform3D, Viewpoint, Viewpoint3D};
use super::{DragHandler, DragType};
use crate::commands::*;
use crate::math::raycast;

pub type GridView3D = GenericGridView<GridViewDim3D>;

#[derive(Debug, Default)]
pub struct GridViewDim3D;
impl GridViewDimension for GridViewDim3D {
    type D = Dim3D;
    type Viewpoint = Viewpoint3D;

    fn do_view_command(this: &mut GridView3D, command: ViewCommand) -> Result<()> {
        // Delegate to the viewpoint.
        let maybe_new_drag_handler = this
            .viewpoint_interpolator
            .do_view_command(command)
            .context("Executing view command")?;

        // Update drag handler, if the viewpoint gave one.
        if !this.is_dragging() {
            if let Some(mut interpolator_drag_handler) = maybe_new_drag_handler {
                this.start_drag(
                    DragType::MovingView,
                    Box::new(move |gridview: &mut GridView3D, cursor_pos| {
                        interpolator_drag_handler(&mut gridview.viewpoint_interpolator, cursor_pos)
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
        let mut frame = GridViewRender3D::new(params, this.viewpoint());
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

        // Draw gridlines.
        frame.draw_gridlines()?;
        // // Draw crosshairs.
        // if let Some(pos) = &draw_pos {
        //     frame.draw_blue_cursor_highlight(&pos.floor());
        // }

        frame.finish()
    }
}
impl GridView3D {
    pub fn screen_pos(&self, pixel: FVec2D) -> ScreenPos3D {
        let vp = self.viewpoint();
        let xform = vp.cell_transform();

        let mut raycast_hit;
        {
            // Get the `NdTreeSlice` containing all of the visible cells.
            let global_visible_rect = vp.global_visible_rect();
            let visible_octree = self.automaton.ndtree.slice_containing(&global_visible_rect);
            let local_octree_base_pos =
                xform.global_to_local_int(&visible_octree.base_pos).unwrap();

            // Compute the ray, relative to the root node of the octree slice.
            let (mut start, delta) = xform.pixel_to_local_ray(pixel);
            printlnd!("octree off {}", local_octree_base_pos);
            start -= local_octree_base_pos.to_fvec();

            let layer = xform.render_cell_layer;
            let node = visible_octree.root.as_ref();
            raycast_hit = crate::math::raycast::octree_raycast(start, delta, layer, node);

            // Convert coordinates back from octree node space into local space.
            if let Some(hit) = &mut raycast_hit {
                hit.start += local_octree_base_pos.to_fvec();
                hit.pos_int += local_octree_base_pos;
                hit.pos_float += local_octree_base_pos.to_fvec();
            }
        };

        ScreenPos3D {
            pixel,
            xform,
            raycast_hit,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScreenPos3D {
    pixel: FVec2D,
    xform: CellTransform3D,
    raycast_hit: Option<raycast::Hit>,
}
impl ScreenPos3D {
    /// Returns the position of the mouse in pixel space.
    pub fn pixel(&self) -> FVec2D {
        self.pixel
    }
    /// Returns the global cell position at the mouse cursor in the plane parallel
    /// to the screen at the distance of the viewpoint pivot.
    pub fn global_pos_at_pivot_depth(&self) -> FixedVec3D {
        self.xform
            .pixel_to_global_pos(self.pixel, Viewpoint3D::DISTANCE_TO_PIVOT)
    }

    /// Returns the global position of the cell visible at the mouse cursor.
    pub fn raycast(&self) -> Option<RaycastHit> {
        self.raycast_hit.map(|hit| RaycastHit {
            pos: self.xform.local_to_global_float(hit.pos_float),
            cell: self.xform.local_to_global_int(hit.pos_int),
            face: (hit.face_axis, hit.face_sign),
        })
    }
    /// Returns the global position of the cell visible at the mouse cursor.
    pub fn raycast_face(&self) -> Option<(Axis, Sign)> {
        self.raycast_hit.map(|hit| (hit.face_axis, hit.face_sign))
    }
}

pub struct RaycastHit {
    /// Point of intersection between ray and cell.
    pub pos: FixedVec3D,
    /// Position of intersected cell.
    pub cell: BigVec3D,
    /// Face of cell intersected.
    pub face: (Axis, Sign),
}
