use anyhow::{Context, Result};

use ndcell_core::prelude::*;

use super::algorithms::raycast;
use super::generic::{GenericGridView, GridViewDimension, SelectionDragHandler};
use super::render::{CellDrawParams, GridViewRender3D, RenderParams, RenderResult};
use super::screenpos::{RaycastHit, RaycastHitThing, ScreenPos3D};
use super::viewpoint::{Viewpoint, Viewpoint3D};
use super::{DragHandler, DragType};
use crate::commands::*;
use crate::mouse::MouseDisplay;

pub type GridView3D = GenericGridView<GridViewDim3D>;
type SelectionDragHandler3D = SelectionDragHandler<GridViewDim3D>;

#[derive(Debug)]
pub struct GridViewDim3D {
    grid_axis: Option<Axis>,
    grid_coord: BigInt,
}
impl Default for GridViewDim3D {
    fn default() -> Self {
        Self {
            grid_axis: None,
            grid_coord: BigInt::zero(),
        }
    }
}
impl GridViewDimension for GridViewDim3D {
    type D = Dim3D;
    type Viewpoint = Viewpoint3D;
    type ScreenPos = ScreenPos3D;

    fn do_view_command(this: &mut GridView3D, command: ViewCommand) -> Result<()> {
        // Handle `FocusPixel` specially because it depends on the cell contents
        // of the automaton.
        if let ViewCommand::FocusPixel(pixel) = command {
            if let Some(hit) = this.screen_pos(pixel).raycast_octree_hit {
                // Set grid axes.
                let axis = hit.face.normal_axis();
                this.show_grid(axis, hit.pos[axis].round());

                // Set position.
                let NdVec([x, y, z]) = hit.pos;
                this.do_command(ViewCommand::GoTo3D {
                    x: Some(x),
                    y: Some(y),
                    z: Some(z),
                    yaw: None,
                    pitch: None,
                    relative: false,
                    scaled: false,
                })?;
            } else {
                // Hide grid.
                this.hide_grid();
            }

            return Ok(());
        }

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
                    None,
                );
            }
        }

        Ok(())
    }

    fn render(this: &mut GridView3D, params: RenderParams<'_>) -> Result<RenderResult> {
        if this.automaton.ndtree.root_ref().is_empty() {
            // Provide a surface to place initial cells on.
            this.show_grid(Axis::Y, BigInt::zero());
        }

        let mouse = params.mouse;
        let screen_pos = mouse.pos.map(|pixel| this.screen_pos(pixel));

        let mut frame = GridViewRender3D::new(params, this.viewpoint());
        frame.draw_cells(CellDrawParams {
            ndtree: &this.automaton.ndtree,
            alpha: 1.0,
            rect: None,
        })?;

        // Draw gridlines.
        if let Some((axis, coord)) = this.grid() {
            frame.add_gridlines_overlay(axis, coord.clone());
        }

        // Draw mouse display.
        if let Some(screen_pos) = &screen_pos {
            match mouse.display {
                MouseDisplay::Draw(mode) => {
                    let cell_and_face_to_draw = match &this.drag_initial {
                        Some(initial) => {
                            screen_pos.cell_and_face_to_draw_starting_at(mode, initial)
                        }
                        None => screen_pos.cell_and_face_to_draw(mode),
                    };
                    if let Some((cell, face)) = cell_and_face_to_draw {
                        frame.add_hover_draw_overlay(&cell, face, mode);
                    }
                }
                MouseDisplay::Select => {
                    if let Some(hit) = &screen_pos.raycast {
                        frame.add_hover_select_overlay(&hit.cell, hit.face);
                    }
                }
                _ => (),
            }
        }
        // Draw selection highlight.
        if let Some(selection) = &this.selection {
            if selection.cells.is_some() {
                frame.add_selection_cells_highlight_overlay(&selection.rect);
            } else {
                frame.add_selection_region_highlight_overlay(&selection.rect);
            }
        }
        // Draw relative selection resize preview after drawing selection.
        if let MouseDisplay::ResizeSelectionFace(face) = mouse.display {
            if let Some(current_selection) = &this.selection {
                frame.add_selection_face_resize_overlay(&current_selection.rect, face);
            }
        }

        frame.finish()
    }

    fn screen_pos(this: &GridView3D, pixel: FVec2D) -> ScreenPos3D {
        let layer = this.viewpoint().render_cell_layer();

        let vp = this.viewpoint();
        let xform = vp.cell_transform();

        let (start, delta) = xform.pixel_to_local_ray(pixel);

        let raycast_octree_hit;
        {
            // Get the `NdTreeSlice` containing all of the visible cells.
            let global_visible_rect = vp.global_visible_rect();
            let visible_octree = this.automaton.ndtree.slice_containing(&global_visible_rect);
            let local_octree_base_pos =
                xform.global_to_local_int(&visible_octree.base_pos).unwrap();

            // Compute the ray, relative to the root node of the octree slice.
            let octree_start = start - local_octree_base_pos.to_fvec();

            let layer = xform.render_cell_layer;
            let node = visible_octree.root.as_ref();

            raycast_octree_hit = raycast::intersect_octree(octree_start, delta, layer, node)
                // Use `add_base_pos()` to convert coordinates back from octree
                // node space into local space.
                .map(|h| h.add_base_pos(local_octree_base_pos))
                .map(|h| RaycastHit::new(&xform, h, RaycastHitThing::Cell));
        }

        let raycast_gridlines_hit = this.grid().and_then(|(grid_axis, grid_coord)| {
            let grid_coord = xform.global_to_local_visible_coord(grid_axis, &grid_coord)?;
            raycast::intersect_plane(start, delta, grid_axis, r64(grid_coord as f64))
                .map(|h| RaycastHit::new(&xform, h, RaycastHitThing::Gridlines))
        });
        let raycast_selection_hit = None; // TODO probably remove this entirely

        // Choose the ONE RAYCAST TO RULE THEM ALL (the one that is shortest).
        let raycast = [
            raycast_octree_hit.as_ref(),
            raycast_gridlines_hit.as_ref(),
            raycast_selection_hit.as_ref(),
        ]
        .iter()
        .copied()
        .flatten() // Remove `Option<T>` layer.
        .min() // Select minimum by distance.
        .cloned();

        ScreenPos3D {
            pixel,
            layer,
            xform,

            raycast,
            raycast_octree_hit,
            raycast_gridlines_hit,
            raycast_selection_hit,
        }
    }
}
impl GridView3D {
    pub fn show_grid(&mut self, axis: Axis, coord: BigInt) {
        self.dim.grid_axis = Some(axis);
        self.dim.grid_coord = coord;
    }
    pub fn hide_grid(&mut self) {
        self.dim.grid_axis = None;
    }
    pub fn grid(&self) -> Option<(Axis, &BigInt)> {
        match self.dim.grid_axis {
            Some(axis) => Some((axis, &self.dim.grid_coord)),
            None => None,
        }
    }
}
