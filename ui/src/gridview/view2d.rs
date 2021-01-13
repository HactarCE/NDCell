use anyhow::{Context, Result};
use log::warn;

use ndcell_core::prelude::*;

use super::generic::{GenericGridView, GridViewDimension};
use super::history::History;
use super::render::{CellDrawParams, GridViewRender2D, RenderParams, RenderResult};
use super::selection::Selection2D;
use super::viewpoint::{ScreenPos2D, Viewpoint, Viewpoint2D};
use super::{DragHandler, DragOutcome, DragType};
use crate::commands::*;
use crate::mouse::MouseDisplay;
use crate::{Scale, CONFIG};

pub type GridView2D = GenericGridView<GridViewDim2D>;

#[derive(Debug, Default)]
pub struct GridViewDim2D;
impl GridViewDimension for GridViewDim2D {
    type D = Dim2D;
    type Viewpoint = Viewpoint2D;

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
                );
            }
        }

        Ok(())
    }
    fn do_draw_command(this: &mut GridView2D, command: DrawCommand) -> Result<()> {
        match command {
            DrawCommand::SetState(new_selected_cell_state) => {
                this.selected_cell_state = new_selected_cell_state;
            }
            DrawCommand::Drag(c, cursor_start) => {
                // Don't draw if the scale is too small.
                if this.too_small_to_draw() {
                    return Ok(());
                }

                let initial_cell = match this.viewpoint().pixel_to_screen_pos(cursor_start) {
                    Some(pos) => pos.cell(),
                    None => return Ok(()),
                };

                let new_cell_state = c.mode.cell_state(
                    this.automaton.ndtree.get_cell(&initial_cell),
                    this.selected_cell_state,
                );

                let new_drag_handler: DragHandler<GridView2D> = match c.shape {
                    DrawShape::Freeform => {
                        make_freeform_draw_drag_handler(initial_cell, new_cell_state)
                    }
                    DrawShape::Line => {
                        // TODO: implement drawing straight line
                        warn!("Line drawing is not yet implemented!");
                        return Ok(());
                    }
                };

                this.reset_worker_thread();
                this.record();
                this.start_drag(DragType::Drawing, new_drag_handler);
            }
            DrawCommand::Confirm => {
                if this.is_drawing() {
                    this.stop_drag();
                }
            }
            DrawCommand::Cancel => {
                if this.is_drawing() {
                    this.stop_drag();
                    this.undo();
                }
            }
        }
        Ok(())
    }
    fn do_select_command(this: &mut GridView2D, command: SelectCommand) -> Result<()> {
        match command {
            SelectCommand::Drag(c, cursor_start) => {
                let maybe_initial_pos = this.viewpoint().pixel_to_screen_pos(cursor_start);
                let initial_pos = match maybe_initial_pos {
                    Some(pos) => pos,
                    None => return Ok(()),
                };

                let new_drag_handler = match c {
                    SelectDragCommand::NewRect => make_new_rect_selection_drag_handler(initial_pos),
                    SelectDragCommand::Resize { axes, .. } => {
                        make_resize_selection_drag_handler(initial_pos, axes)
                    }
                    SelectDragCommand::ResizeToCell => {
                        make_resize_selection_to_cell_drag_handler(initial_pos)
                    }
                    SelectDragCommand::MoveSelection => {
                        make_move_selection_drag_handler(initial_pos)
                    }
                    SelectDragCommand::MoveCells => {
                        make_move_or_copy_selected_cells_drag_handler(initial_pos, false)
                    }
                    SelectDragCommand::CopyCells => {
                        make_move_or_copy_selected_cells_drag_handler(initial_pos, true)
                    }
                };

                let wait_for_drag_threshold = match c {
                    SelectDragCommand::NewRect => true,
                    SelectDragCommand::Resize { .. } => true,
                    SelectDragCommand::ResizeToCell => false,
                    SelectDragCommand::MoveSelection => true,
                    SelectDragCommand::MoveCells => true,
                    SelectDragCommand::CopyCells => true,
                };

                let new_drag_handler_with_threshold: DragHandler<GridView2D> =
                    make_selection_drag_handler_with_threshold(
                        cursor_start,
                        wait_for_drag_threshold,
                        new_drag_handler,
                    );

                match c {
                    SelectDragCommand::NewRect
                    | SelectDragCommand::Resize { .. }
                    | SelectDragCommand::ResizeToCell
                    | SelectDragCommand::MoveSelection => {
                        if CONFIG.lock().hist.record_select {
                            this.reset_worker_thread();
                            this.record();
                        }
                    }
                    SelectDragCommand::MoveCells | SelectDragCommand::CopyCells => {
                        this.reset_worker_thread();
                        this.record();
                    }
                }

                if let SelectDragCommand::NewRect = c {
                    // Deselect immediately; don't wait for drag threshold.
                    this.deselect();
                }

                this.start_drag(DragType::Selecting, new_drag_handler_with_threshold);
            }

            SelectCommand::SelectAll => {
                this.deselect(); // take into account pasted cells
                this.set_selection(this.automaton.ndtree.bounding_rect().map(Selection2D::from))
            }
            SelectCommand::Deselect => {
                this.deselect();
            }

            SelectCommand::Copy(format) => {
                if this.selection.is_some() {
                    let result = this.export(format);
                    match result {
                        Ok(s) => crate::clipboard_compat::clipboard_set(s)?,
                        Err(msg) => warn!("Failed to generate {}: {}", format, msg),
                    }
                }
            }
            SelectCommand::Paste => {
                let old_sel_rect = this.selection_rect();

                this.record();
                let string_from_clipboard = crate::clipboard_compat::clipboard_get()?;
                let result =
                    Selection2D::from_str(&string_from_clipboard, this.automaton.ndtree.pool());
                match result {
                    Ok(sel) => {
                        this.set_selection(sel);
                        this.ensure_selection_visible();

                        // If selection size is the same, preserve position.
                        if let Some((old_rect, new_sel)) = old_sel_rect.zip(this.selection.as_mut())
                        {
                            if old_rect.size() == new_sel.rect.size() {
                                *new_sel = new_sel.move_by(old_rect.min() - new_sel.rect.min());
                            }
                        }
                    }
                    Err(errors) => warn!("Failed to load pattern: {:?}", errors),
                }
            }
            SelectCommand::Delete => {
                if this.selection.is_some() {
                    this.record();
                    this.selection.as_mut().unwrap().cells = None;
                    this.grab_selected_cells();
                    this.selection.as_mut().unwrap().cells = None;
                }
            }

            SelectCommand::Cancel => {
                if let Some(sel) = &this.selection {
                    if sel.cells.is_some() {
                        this.drop_selected_cells();
                    } else {
                        this.deselect();
                    }
                }
            }
        }
        Ok(())
    }

    fn render(this: &mut GridView2D, params: RenderParams<'_>) -> Result<RenderResult> {
        let mouse = params.mouse;
        let mouse_pos = this.viewpoint().try_pixel_to_screen_pos(mouse.pos);

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
        frame.draw_gridlines()?;

        // Draw mouse display.
        if let Some(hovered_cell) = &mouse_pos {
            match mouse.display {
                MouseDisplay::Draw => {
                    if !this.too_small_to_draw() {
                        frame.draw_hover_highlight(
                            &hovered_cell.cell(),
                            crate::colors::HOVERED_DRAW,
                        )?;
                    }
                }
                MouseDisplay::Select => {
                    frame.draw_hover_highlight(
                        &hovered_cell.cell(),
                        crate::colors::HOVERED_SELECT,
                    )?;
                }
                _ => (),
            }
        }
        // Draw selection highlight.
        if let Some(selection) = &this.selection {
            frame.draw_selection_highlight(selection.rect.clone(), selection.cells.is_none())?;
        }
        // Draw selection preview after drawing selection.
        if mouse.display == MouseDisplay::ResizeSelectionAbsolute && !this.is_dragging() {
            if let (Some(mouse_pos), Some(s)) = (mouse_pos, this.selection.as_ref()) {
                frame.draw_absolute_selection_resize_preview(s.rect.clone(), &mouse_pos)?;
            }
        }

        frame.finish()
    }
}

/// Returns `true` if the scale is too small to draw individual cells, or
/// `false` otherwise.
fn is_too_small_to_draw(viewpoint: &Viewpoint2D) -> bool {
    viewpoint.scale() < Scale::from_factor(r64(1.0))
}

fn make_freeform_draw_drag_handler(
    mut pos1: BigVec2D,
    new_cell_state: u8,
) -> DragHandler<GridView2D> {
    Box::new(move |this, new_cursor_pos| {
        if this.too_small_to_draw() {
            return Ok(DragOutcome::Cancel);
        }
        let pos2 = this
            .viewpoint()
            .cell_transform()
            .pixel_to_global_pos(new_cursor_pos)
            .map(|pos| pos.floor());
        if let Some(pos2) = &pos2 {
            for pos in ndcell_core::math::bresenham(pos1.clone(), pos2.clone()) {
                this.automaton.ndtree.set_cell(&pos, new_cell_state);
            }
            pos1 = pos2.clone();
            Ok(DragOutcome::Continue)
        } else {
            Ok(DragOutcome::Cancel)
        }
    })
}

type SelectionDragHandler = Box<dyn FnMut(&mut GridView2D, ScreenPos2D) -> Result<DragOutcome>>;

fn make_selection_drag_handler_with_threshold(
    cursor_start: FVec2D,
    wait_for_drag_threshold: bool,
    mut inner_drag_handler: SelectionDragHandler,
) -> DragHandler<GridView2D> {
    let drag_threshold = r64(CONFIG.lock().mouse.drag_threshold);

    // State variable to be moved into the closure and used by the
    // drag handler.
    let mut past_drag_threshold: bool = false;
    if !wait_for_drag_threshold {
        past_drag_threshold = true;
    }

    Box::new(move |this, new_cursor_pos| {
        if !((new_cursor_pos - cursor_start).abs() < FVec::repeat(drag_threshold)) {
            past_drag_threshold = true;
        }
        if past_drag_threshold {
            if let Some(new_pos) = this.viewpoint().pixel_to_screen_pos(new_cursor_pos) {
                inner_drag_handler(this, new_pos)
            } else {
                Ok(DragOutcome::Continue)
            }
        } else {
            Ok(DragOutcome::Continue)
        }
    })
}

fn make_new_rect_selection_drag_handler(initial_pos: ScreenPos2D) -> SelectionDragHandler {
    Box::new(move |this, new_pos| {
        this.set_selection_rect(Some(NdRect::span(initial_pos.cell(), new_pos.cell())));
        Ok(DragOutcome::Continue)
    })
}
fn make_resize_selection_drag_handler(
    initial_pos: ScreenPos2D,
    axes: AxisSet,
) -> SelectionDragHandler {
    let mut initial_selection = None;

    Box::new(move |this, new_pos| {
        initial_selection = initial_selection.take().or_else(|| this.deselect());
        if let Some(s) = &initial_selection {
            this.set_selection_rect(Some(super::selection::resize_selection_relative(
                &s.rect,
                &initial_pos.pos(),
                &new_pos.pos(),
                axes,
            )));
            Ok(DragOutcome::Continue)
        } else {
            // There is no selection to resize.
            Ok(DragOutcome::Cancel)
        }
    })
}
fn make_resize_selection_to_cell_drag_handler(initial_pos: ScreenPos2D) -> SelectionDragHandler {
    let mut initial_selection = None;

    Box::new(move |this, new_pos| {
        initial_selection = initial_selection.take().or_else(|| this.deselect());
        if let Some(s) = &initial_selection {
            this.set_selection_rect(Some(super::selection::resize_selection_absolute(
                &s.rect,
                &initial_pos.pos(),
                &new_pos.pos(),
            )));
            Ok(DragOutcome::Continue)
        } else {
            // There is no selection to resize.
            Ok(DragOutcome::Cancel)
        }
    })
}
fn make_move_selection_drag_handler(initial_pos: ScreenPos2D) -> SelectionDragHandler {
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
) -> SelectionDragHandler {
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

impl GridView2D {
    /// Moves the selection to the center of the screen along each axis for
    /// which it is outside the viewport.
    fn ensure_selection_visible(&mut self) {
        if let Some(mut sel) = self.selection.take() {
            // The number of render cells of padding to ensure.
            const PADDING: usize = 2;

            let (render_cell_layer, _) = self.viewpoint().render_cell_layer_and_scale();

            // Convert to render cells.
            let sel_rect = sel.rect.div_outward(&render_cell_layer.big_len());
            let visible_rect = self
                .viewpoint()
                .global_visible_rect()
                .div_outward(&render_cell_layer.big_len());

            let sel_min = sel_rect.min();
            let sel_max = sel_rect.max();
            let sel_center = sel_rect.center();
            let visible_min = visible_rect.min();
            let visible_max = visible_rect.max();
            let view_center = self.viewpoint().center().floor();

            for &ax in Dim2D::axes() {
                if sel_max[ax] < visible_min[ax].clone() + PADDING
                    || visible_max[ax] < sel_min[ax].clone() + PADDING
                {
                    // Move selection to center along this axis.
                    sel =
                        sel.move_by(NdVec::unit(ax) * (view_center[ax].clone() - &sel_center[ax]));
                }
            }

            self.set_selection(Some(sel));
        }
    }

    /// Returns `true` if the scale is too small to draw individual cells, or
    /// `false` otherwise.
    fn too_small_to_draw(&self) -> bool {
        self.viewpoint().scale() < Scale::from_factor(r64(1.0))
    }
}
