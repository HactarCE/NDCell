use anyhow::{bail, Context, Result};

use ndcell_core::prelude::*;

use super::algorithms::raycast;
use super::generic::{GenericGridView, GridViewDimension};
use super::render::{CellDrawParams, GridViewRender3D, RenderParams, RenderResult};
use super::viewpoint::{CellTransform3D, Viewpoint, Viewpoint3D};
use super::{DragHandler, DragType};
use crate::commands::*;
use crate::mouse::MouseDisplay;
use crate::Face;

pub type GridView3D = GenericGridView<GridViewDim3D>;

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

    fn do_view_command(this: &mut GridView3D, command: ViewCommand) -> Result<()> {
        // Handle `FocusPixel` specially because it depends on the cell contents
        // of the automaton.
        if let ViewCommand::FocusPixel(pixel) = command {
            if let Some(hit) = this.screen_pos(pixel).raycast() {
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
                );
            }
        }

        Ok(())
    }
    fn do_draw_command(_this: &mut GridView3D, _command: DrawCommand) -> Result<()> {
        bail!("unimplemented")
    }
    fn do_select_command(_this: &mut GridView3D, _command: SelectCommand) -> Result<()> {
        bail!("unimplemented")
    }

    fn render(this: &mut GridView3D, params: RenderParams<'_>) -> Result<RenderResult> {
        let mouse = params.mouse;
        let mouse_raycast = mouse.pos.and_then(|pixel| this.screen_pos(pixel).raycast());

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
        if let Some(hit) = mouse_raycast {
            match mouse.display {
                MouseDisplay::Draw if !this.viewpoint().too_small_to_draw() => {
                    frame.add_hover_draw_overlay(&hit.cell, hit.face);
                }
                MouseDisplay::Select => {
                    frame.add_hover_select_overlay(&hit.cell, hit.face);
                }
                _ => (),
            }
        }
        frame.finish()
    }
}
impl GridView3D {
    pub fn screen_pos(&self, pixel: FVec2D) -> ScreenPos3D {
        let vp = self.viewpoint();
        let xform = vp.cell_transform();

        let (start, delta) = xform.pixel_to_local_ray(pixel);

        let raycast_octree_hit;
        {
            // Get the `NdTreeSlice` containing all of the visible cells.
            let global_visible_rect = vp.global_visible_rect();
            let visible_octree = self.automaton.ndtree.slice_containing(&global_visible_rect);
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

        let raycast_gridlines_hit = self.grid().and_then(|(grid_axis, grid_coord)| {
            let grid_coord = xform.global_to_local_visible_coord(grid_axis, &grid_coord)?;
            raycast::intersect_plane(start, delta, grid_axis, r64(grid_coord as f64))
                .map(|h| RaycastHit::new(&xform, h, RaycastHitThing::Gridlines))
        });
        let raycast_selection_hit = None; // TODO

        printlnd!("gridl {:?}", raycast_gridlines_hit);

        ScreenPos3D {
            pixel,
            xform,
            raycast_octree_hit,
            raycast_gridlines_hit,
            raycast_selection_hit,
        }
    }

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

#[derive(Debug, Clone)]
pub struct ScreenPos3D {
    pixel: FVec2D,
    xform: CellTransform3D,
    pub raycast_octree_hit: Option<RaycastHit>,
    pub raycast_gridlines_hit: Option<RaycastHit>,
    pub raycast_selection_hit: Option<RaycastHit>,
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

    /// Returns the global position of the closest iteractive thing visible at
    /// the mouse cursor.
    pub fn raycast(&self) -> Option<RaycastHit> {
        // Select closest raycast.
        [
            &self.raycast_octree_hit,
            &self.raycast_gridlines_hit,
            &self.raycast_selection_hit,
        ]
        .iter()
        .min_by_key(|h| match h {
            Some(hit) => hit.distance,
            None => R64::max_value(),
        })
        .cloned()
        .cloned()
        .flatten()
    }
}

#[derive(Debug, Clone)]
pub struct RaycastHit {
    /// Point of intersection between ray and cell.
    pub pos: FixedVec3D,
    /// Position of intersected cell.
    pub cell: BigVec3D,
    /// Face of cell intersected.
    pub face: Face,
    /// Type of thing hit.
    pub thing: RaycastHitThing,
    /// Abstract distance to intersection.
    distance: R64,
}
impl RaycastHit {
    fn new(xform: &CellTransform3D, hit: raycast::Hit, thing: RaycastHitThing) -> Self {
        Self {
            pos: xform.local_to_global_float(hit.pos_float),
            cell: xform.local_to_global_int(hit.pos_int),
            face: hit.face,
            thing: thing,
            distance: hit.t0,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RaycastHitThing {
    Cell,
    Gridlines,
    Selelction,
}
