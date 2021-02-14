use anyhow::{Context, Result};

use ndcell_core::prelude::*;

use super::generic::{GenericGridView, GridViewDimension, SelectionDragHandler};
use super::render::{CellDrawParams, GridViewRender2D, RenderParams, RenderResult};
use super::screenpos::{ScreenPos, ScreenPos2D};
use super::viewpoint::{Viewpoint, Viewpoint2D};
use super::{DragHandler, DragType};
use crate::commands::*;
use crate::mouse::MouseDisplay;
use crate::Scale;

pub type GridView2D = GenericGridView<GridViewDim2D>;
type SelectionDragHandler2D = SelectionDragHandler<GridViewDim2D>;

#[derive(Debug, Default)]
pub struct GridViewDim2D;
impl GridViewDimension for GridViewDim2D {
    type D = Dim2D;
    type Viewpoint = Viewpoint2D;
    type ScreenPos = ScreenPos2D;

    fn do_view_command(this: &mut GridView2D, command: ViewCommand) -> Result<()> {
        // Handle `FitView` specially because it depends on the cell contents of
        // the automaton.
        if matches!(command, ViewCommand::FitView) {
            if let Some(pattern_bounding_rect) = this.automaton.ndtree.bounding_rect() {
                // Set position.
                let NdVec([x, y]) = pattern_bounding_rect.center();
                this.do_command(ViewCommand::GoTo2D {
                    x: Some(x.into()),
                    y: Some(y.into()),
                    relative: false,
                    scaled: false,
                })?;

                // Set scale.
                let pattern_size = pattern_bounding_rect.size();
                let target_size = this.viewpoint().target_dimensions();
                let scale = Scale::from_fit(pattern_size, target_size);
                this.do_command(ViewCommand::GoToScale(scale.floor()))?;
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
                    Box::new(move |gridview: &mut GridView2D, cursor_pos| {
                        interpolator_drag_handler(&mut gridview.viewpoint_interpolator, cursor_pos)
                    }) as DragHandler<GridView2D>,
                    None,
                );
            }
        }

        Ok(())
    }

    fn render(this: &mut GridView2D, params: RenderParams<'_>) -> Result<RenderResult> {
        let mouse = params.mouse;
        let screen_pos = mouse.pos.map(|pixel| this.screen_pos(pixel));

        let mut frame = GridViewRender2D::new(params, this.viewpoint());

        // Draw main cells.
        frame.draw_cells(CellDrawParams {
            ndtree: &this.automaton.ndtree,
            alpha: 1.0,
            rect: None,
        })?;
        // Draw selection cells.
        if let Some(selection) = &this.selection {
            if let Some(ndtree) = &selection.cells {
                frame.draw_cells(CellDrawParams {
                    ndtree,
                    alpha: 1.0,
                    rect: Some(&selection.rect),
                })?;
            }
        }

        // Draw gridlines.
        frame.add_gridlines_overlay();

        // Draw mouse display.
        if let Some(screen_pos) = &screen_pos {
            match mouse.display {
                MouseDisplay::Draw(mode) => {
                    if let Some(cell_pos) = screen_pos.cell_to_draw(mode) {
                        frame.add_hover_draw_overlay(&cell_pos, mode);
                    }
                }
                MouseDisplay::Select => {
                    let cell_pos = screen_pos.cell();
                    frame.add_hover_select_overlay(&cell_pos);
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
        // Draw absolute selection resize preview after drawing selection.
        if mouse.display == MouseDisplay::ResizeSelectionAbsolute {
            if let (Some(current_selection), Some(screen_pos)) = (&this.selection, &screen_pos) {
                if this.is_dragging() {
                    // We are already resizing the selection; just use the
                    // current selection.
                    frame.add_selection_resize_preview_overlay(&current_selection.rect);
                } else if let Some(resize_destination) = screen_pos.render_cell_to_select() {
                    // Show what *would* happen if the user resized the
                    // selection.
                    frame.add_selection_resize_preview_overlay(
                        &super::selection::resize_selection_absolute(
                            &current_selection.rect,
                            &screen_pos.pos(),
                            &resize_destination,
                        ),
                    );
                }
            }
        }
        // Draw relative selection resize preview after drawing selection.
        if let MouseDisplay::ResizeSelectionEdge(direction) = mouse.display {
            if let Some(current_selection) = &this.selection {
                frame.add_selection_edge_resize_overlay(&current_selection.rect, direction);
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
