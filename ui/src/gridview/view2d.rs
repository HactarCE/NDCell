use anyhow::{Context, Result};
use log::warn;

use ndcell_core::prelude::*;

use super::generic::{GenericGridView, GridViewDimension, SelectionDragHandler};
use super::render::{CellDrawParams, GridViewRender2D, RenderParams, RenderResult};
use super::screenpos::{ScreenPos, ScreenPos2D};
use super::viewpoint::{Viewpoint, Viewpoint2D};
use super::{DragHandler, DragOutcome, DragType};
use crate::commands::*;
use crate::mouse::MouseDisplay;
use crate::{Direction, Scale};

pub type GridView2D = GenericGridView<GridViewDim2D>;
type SelectionDragHandler2D = SelectionDragHandler<GridViewDim2D>;

macro_rules! ignore_command {
    ($c:expr) => {{
        warn!("Ignoring {:?} in GridView3D", $c);
        return None;
    }};
}

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
    fn make_select_drag_handler(
        this: &mut GenericGridView<Self>,
        command: SelectDragCommand,
        initial_pos: ScreenPos2D,
    ) -> Option<SelectionDragHandler2D> {
        match command {
            SelectDragCommand::NewRect => Some(make_new_rect_selection_drag_handler(initial_pos)),
            SelectDragCommand::Resize2D(direction) => {
                Some(make_resize_selection_drag_handler(initial_pos, direction))
            }
            SelectDragCommand::Resize3D(_) => ignore_command!(command),
            SelectDragCommand::ResizeToCell => {
                Some(make_resize_selection_to_cell_drag_handler(initial_pos))
            }
            SelectDragCommand::MoveSelection => Some(make_move_selection_drag_handler(initial_pos)),
            SelectDragCommand::MoveCells => Some(make_move_or_copy_selected_cells_drag_handler(
                initial_pos,
                false,
            )),
            SelectDragCommand::CopyCells => Some(make_move_or_copy_selected_cells_drag_handler(
                initial_pos,
                true,
            )),
        }
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
                    if let Some(cell_pos) = screen_pos.draw_cell(mode, None) {
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
                let resize_preview_rect = if this.is_dragging() {
                    // We are already resizing the selection; just use the
                    // current selection.
                    current_selection.rect.clone()
                } else {
                    // Show what *would* happen if the user resized the
                    // selection.
                    super::selection::resize_selection_absolute(
                        &current_selection.rect,
                        &screen_pos.pos(),
                        &screen_pos.rect(),
                    )
                };
                frame.add_selection_resize_preview_overlay(&resize_preview_rect);
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

fn make_new_rect_selection_drag_handler(initial_pos: ScreenPos2D) -> SelectionDragHandler2D {
    Box::new(move |this, new_pos| {
        this.set_selection_rect(Some(NdRect::span_rects(initial_pos.rect(), new_pos.rect())));
        Ok(DragOutcome::Continue)
    })
}
fn make_resize_selection_drag_handler(
    initial_pos: ScreenPos2D,
    direction: Direction,
) -> SelectionDragHandler2D {
    let mut initial_selection = None;

    Box::new(move |this, new_pos| {
        initial_selection = initial_selection.take().or_else(|| this.deselect());
        if let Some(s) = &initial_selection {
            this.set_selection_rect(Some(super::selection::resize_selection_relative(
                &s.rect,
                &initial_pos.pos(),
                &new_pos.pos(),
                direction.vector(),
            )));
            Ok(DragOutcome::Continue)
        } else {
            // There is no selection to resize.
            Ok(DragOutcome::Cancel)
        }
    })
}
fn make_resize_selection_to_cell_drag_handler(initial_pos: ScreenPos2D) -> SelectionDragHandler2D {
    let mut initial_selection = None;

    Box::new(move |this, new_pos| {
        initial_selection = initial_selection.take().or_else(|| this.deselect());
        if let Some(s) = &initial_selection {
            this.set_selection_rect(Some(super::selection::resize_selection_absolute(
                &s.rect,
                &initial_pos.pos(),
                &new_pos.rect(),
            )));
            Ok(DragOutcome::Continue)
        } else {
            // There is no selection to resize.
            Ok(DragOutcome::Cancel)
        }
    })
}
fn make_move_selection_drag_handler(initial_pos: ScreenPos2D) -> SelectionDragHandler2D {
    let mut initial_selection = None;

    Box::new(move |this, new_pos| {
        initial_selection = initial_selection.take().or_else(|| this.deselect());
        if let Some(s) = &initial_selection {
            let delta = (new_pos.pos() - initial_pos.pos()).round();
            this.set_selection_rect(Some(s.rect.clone() + delta));
            Ok(DragOutcome::Continue)
        } else {
            // There is no selection to move.
            Ok(DragOutcome::Cancel)
        }
    })
}
fn make_move_or_copy_selected_cells_drag_handler(
    initial_pos: ScreenPos2D,
    copy: bool,
) -> SelectionDragHandler2D {
    let mut initial_selection = None;

    Box::new(move |this, new_pos| {
        initial_selection = initial_selection.take().or_else(|| {
            if copy {
                this.grab_copy_of_selected_cells();
            } else {
                this.grab_selected_cells();
            }
            this.selection.take()
        });
        if let Some(s) = &initial_selection {
            let delta = (new_pos.pos() - initial_pos.pos()).round();
            this.selection = Some(s.move_by(delta));
            Ok(DragOutcome::Continue)
        } else {
            // There is no selection to move.
            Ok(DragOutcome::Cancel)
        }
    })
}
