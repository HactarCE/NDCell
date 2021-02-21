use anyhow::Result;

use ndcell_core::prelude::*;

use super::generic::{GenericGridView, GridViewDimension};
use super::render::{CellDrawParams, GridViewRender2D, RenderParams, RenderResult};
use super::screenpos::{ScreenPos2D, ScreenPosTrait};
use super::viewpoint::{Viewpoint, Viewpoint2D};
use crate::mouse::MouseDisplayMode;

pub type GridView2D = GenericGridView<Dim2D>;

impl GridViewDimension for Dim2D {
    type Viewpoint = Viewpoint2D;
    type ScreenPos = ScreenPos2D;

    type Data = ();

    fn focus(this: &mut GenericGridView<Self>, pos: &ScreenPos2D) {
        this.target_viewpoint().set_center(pos.pos());
    }

    fn render(this: &mut GridView2D, params: RenderParams<'_>) -> Result<RenderResult> {
        let mouse = params.mouse;
        let screen_pos = mouse.pos.map(|pixel| this.screen_pos(pixel));
        let pos_to_highlight = this.cell_to_highlight(&mouse);
        let rect_cell_to_highlight = this.render_cell_rect_to_highlight(&mouse);

        let mut frame = GridViewRender2D::new(params, this.viewpoint());

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
        frame.add_gridlines_overlay();

        // Draw mouse display.
        if let Some(pos) = &pos_to_highlight {
            match mouse.display_mode {
                MouseDisplayMode::Draw(draw_mode) => {
                    frame.add_hover_draw_overlay(&pos.cell, draw_mode);
                }
                MouseDisplayMode::Select => {
                    frame.add_hover_select_overlay(&pos.cell);
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
                    screen_pos.and_then(|pos| pos.absolute_selection_resize_start_pos()),
                    rect_cell_to_highlight,
                ) {
                    // Show what *would* happen if the user resized the
                    // selection.
                    frame.add_selection_resize_preview_overlay(
                        &super::selection::resize_selection_absolute(
                            &selection.rect,
                            &resize_start,
                            &resize_end,
                        ),
                    );
                }
            }

            // Draw relative selection resize preview after drawing selection.
            if let MouseDisplayMode::ResizeSelectionEdge(edge) = mouse.display_mode {
                frame.add_selection_edge_resize_overlay(&selection.rect, edge);
            }
        }

        frame.finish()
    }

    fn screen_pos(this: &GridView2D, pixel: FVec2D) -> ScreenPos2D {
        let layer = this.viewpoint().render_cell_layer();
        let pos = this.viewpoint().cell_transform().pixel_to_global_pos(pixel);
        ScreenPos2D { pixel, layer, pos }
    }
}
