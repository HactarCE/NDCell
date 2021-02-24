use anyhow::Result;

use ndcell_core::prelude::*;

use super::algorithms::raycast;
use super::drag::Drag;
use super::generic::{GenericGridView, GridViewDimension};
use super::render::{CellDrawParams, GridViewRender3D, RenderParams, RenderResult};
use super::screenpos::{RaycastHit, RaycastHitThing, ScreenPos3D, ScreenPosTrait};
use super::viewpoint::{Viewpoint, Viewpoint3D};
use crate::mouse::MouseDisplayMode;
use crate::CONFIG;

pub type GridView3D = GenericGridView<Dim3D>;

#[derive(Debug)]
pub struct GridViewData3D {
    grid_axis: Option<Axis>,
    grid_coord: BigInt,
}
impl Default for GridViewData3D {
    fn default() -> Self {
        Self {
            grid_axis: None,
            grid_coord: BigInt::zero(),
        }
    }
}
impl GridViewDimension for Dim3D {
    type Viewpoint = Viewpoint3D;
    type ScreenPos = ScreenPos3D;

    type Data = GridViewData3D;

    fn focus(this: &mut GridView3D, pos: &ScreenPos3D) {
        if let Some(hit) = &pos.raycast_octree_hit {
            // Set grid axes.
            let axis = hit.face.normal_axis();
            this.show_grid(axis, hit.pos[axis].round());
            // Set position.
            this.target_viewpoint().set_center(hit.pos.clone());
        } else {
            // Hide grid.
            this.hide_grid();
        }
    }
    fn resize_selection_to_cursor(
        rect: &BigRect3D,
        start: &FixedVec3D,
        end: &BigRect3D,
        drag: &Drag<Self>,
    ) -> BigRect3D {
        super::selection::resize_selection_to_face(
            &rect,
            &start,
            &end,
            drag.initial_pos().unwrap().face,
        )
    }

    fn render(this: &mut GridView3D, params: RenderParams<'_>) -> Result<RenderResult> {
        if this.automaton.ndtree.root_ref().is_empty() && this.selection.is_none() {
            // Provide a surface to place some initial cells on.
            this.show_grid_at_viewpoint(Axis::Y);
        }

        let mouse = params.mouse;
        let screen_pos = mouse.pos.map(|pixel| this.screen_pos(pixel));
        let pos_to_highlight = this.cell_to_highlight(&mouse);
        let rect_cell_to_highlight = this.render_cell_rect_to_highlight(&mouse);

        let mut frame = GridViewRender3D::new(params, this.viewpoint());

        // Draw main cells.
        frame.draw_cells(CellDrawParams {
            ndtree: &this.automaton.ndtree,
            alpha: 1.0,
            rect: None,
            interactive: true,
        })?;
        // Draw selection cells.
        if let Some(selection) = &this.selection {
            if let Some(ndtree) = &selection.cells {
                frame.draw_cells(CellDrawParams {
                    ndtree,
                    alpha: 1.0,
                    rect: Some(&selection.rect),
                    interactive: false,
                })?;
            }
        }

        // Draw gridlines.
        if let Some((axis, coord)) = this.grid() {
            frame.add_gridlines_overlay(axis, coord.clone());
        }

        // Draw mouse display.
        if let Some(pos) = &pos_to_highlight {
            match mouse.display_mode {
                MouseDisplayMode::Draw(draw_mode) => {
                    frame.add_hover_draw_overlay(&pos.cell, pos.face, draw_mode);
                }
                MouseDisplayMode::Select => {
                    frame.add_hover_select_overlay(&pos.cell, pos.face);
                }
                _ => (),
            }
        }

        if let Some(selection) = &this.selection {
            // Draw selection highlight.
            if selection.cells.is_some() {
                frame.add_selection_cells_highlight_overlay(&selection.rect);
            } else {
                frame.add_selection_region_highlight_overlay(&selection.rect);
            }

            // Draw absolute selection resize preview after drawing selection.
            if mouse.display_mode == MouseDisplayMode::ResizeSelectionToCursor {
                if this.is_dragging() {
                    // We are already resizing the selection; just use the
                    // current selection.
                    frame.add_selection_resize_preview_overlay(&selection.rect);
                } else if let (Some(resize_start), Some(resize_end)) = (
                    screen_pos
                        .as_ref()
                        .and_then(|pos| pos.absolute_selection_resize_start_pos()),
                    rect_cell_to_highlight,
                ) {
                    // Show what *would* happen if the user resized the
                    // selection.
                    frame.add_selection_resize_preview_overlay(
                        &super::selection::resize_selection_to_face(
                            &selection.rect,
                            &resize_start,
                            &resize_end,
                            pos_to_highlight.unwrap().face,
                        ),
                    );
                }
            }

            // Draw relative selection resize preview after drawing selection.
            if let MouseDisplayMode::ResizeSelectionFace(face) = mouse.display_mode {
                frame.add_selection_face_resize_overlay(&selection.rect, face);
            }
        }

        if CONFIG.lock().gfx.ndtree_visualization {
            if let Some(screen_pos) = &screen_pos {
                if let Some(hit) = &screen_pos.raycast {
                    frame.add_ndtree_visualization(
                        this.viewpoint().render_cell_layer() + Layer(1),
                        this.automaton.ndtree.layer(),
                        this.automaton.ndtree.base_pos(),
                        &hit.inside_cell(),
                    );
                }
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
    pub fn show_grid_at_viewpoint(&mut self, axis: Axis) {
        let coord = self.target_viewpoint().center()[Axis::Y].floor();
        self.show_grid(axis, coord);
    }
    pub fn show_grid(&mut self, axis: Axis, coord: BigInt) {
        self.dim_data.grid_axis = Some(axis);
        self.dim_data.grid_coord = coord;
    }
    pub fn hide_grid(&mut self) {
        self.dim_data.grid_axis = None;
    }
    pub fn grid(&self) -> Option<(Axis, &BigInt)> {
        match self.dim_data.grid_axis {
            Some(axis) => Some((axis, &self.dim_data.grid_coord)),
            None => None,
        }
    }
}
