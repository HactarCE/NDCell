use anyhow::{Context, Result};
use glium::glutin::event::ModifiersState;
use glium::index::PrimitiveType;
use glium::{uniform, Surface};
use std::cell::RefMut;

use ndcell_core::prelude::*;
use Axis::{X, Y};

use super::consts::*;
use super::shaders;
use super::vertices::MouseTargetVertex;
use super::CellDrawParams;
use crate::gridview::*;
use crate::Scale;

pub struct GenericGridViewRender<'a, R: GridViewRenderDimension<'a>> {
    pub(super) cache: RefMut<'a, super::RenderCache>,
    pub(super) params: RenderParams<'a>,
    pub(super) dimensionality: R,

    /// Camera to render the grid from.
    pub(super) camera: &'a R::Camera,
    /// Global cell origin for "local" render cell vectors.
    pub(super) origin: BigVec<R::D>,

    /// Rectangle of cells in global space that is visible, rounded to render cell boundaries.
    pub(super) global_visible_rect: BigRect<R::D>,
    /// Rectangle of render cells relative to `origin` that is visible.
    pub(super) visible_rect: IRect<R::D>,
    /// Node layer of a render cell.
    pub(super) render_cell_layer: Layer,
    /// Scale to draw render cells at.
    pub(super) render_cell_scale: Scale,
    /// Transform from global cell space to render cell space (1 unit = 1 render
    /// cell; (0, 0) = `origin`), screen space ((-1, -1) = bottom left; (1, 1) =
    /// top right), and pixel space (1 unit = 1 pixel; (0, 0) = top left).
    pub(super) transform: NdCellTransform<R::D>,

    /// Mouse targets, indexed by ID.
    mouse_targets: Vec<MouseTargetData>,
    /// Vertex data for mouse targets.
    mouse_target_tris: Vec<MouseTargetVertex>,
}
impl<'a, R: GridViewRenderDimension<'a>> GenericGridViewRender<'a, R> {
    /// Creates a `GridViewRender` for a gridview.
    pub fn new(params: RenderParams<'a>, camera: &'a R::Camera) -> Result<Self> {
        let mut cache = super::CACHE.borrow_mut();

        // Initialize color and depth buffers.
        params
            .target
            .clear_color_srgb_and_depth(R::DEFAULT_COLOR, R::DEFAULT_DEPTH);

        // Initialize mouse picker.
        cache.picker.init(params.target.get_dimensions());

        // Determine the lowest layer of the quadtree that we must visited,
        // which is the layer of a "render cell," a quadtree node that is
        // rendered as one unit (one pixel in step #1).
        let (render_cell_layer, render_cell_scale) = camera.render_cell_layer_and_scale();
        // Compute the width of cells represented by each render cell.
        let render_cell_len = render_cell_layer.big_len();

        let origin = camera.pos().floor().div_floor(&render_cell_len) * &render_cell_len;

        // Determine the rectangle of visible cells in global coordinate space,
        // rounded to the nearest render cell.
        let global_visible_rect: BigRect<R::D> =
            render_cell_layer.round_rect(&camera.global_visible_rect());

        // Convert that rectangle of cells into a rectangle of render cells,
        // relative to `origin`.
        let visible_rect: IRect<R::D>;
        {
            // Get the rectangle of visible cells relative to the origin.
            let tmp_visible_rect = global_visible_rect.clone() - &origin;
            // Divide by `render_cell_len` to get the rectangle of visible
            // render cells relative to the origin.
            let tmp_visible_rect = tmp_visible_rect.div_outward(&render_cell_len);
            // Now it is safe to convert from `BigInt` to `isize`, because the
            // extent of visible render cells from the origin must be
            // reasonable.
            visible_rect = tmp_visible_rect.to_irect();
        }

        // Compute the transformation from individual cells all the way to
        // pixels.
        let transform = camera.cell_transform_with_base(origin.clone())?;

        Ok(Self {
            cache,
            params,
            dimensionality: R::default(),

            camera,
            origin,

            global_visible_rect,
            visible_rect,
            render_cell_layer,
            render_cell_scale,
            transform,

            mouse_targets: vec![],
            mouse_target_tris: vec![],
        })
    }

    /// Returns a `RenderResult` from this render.
    pub fn finish(mut self) -> Result<RenderResult> {
        Ok(RenderResult {
            mouse_target: self.render_mouse_targets()?,
        })
    }

    fn render_mouse_targets(&mut self) -> Result<Option<MouseTargetData>> {
        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;

        // Draw the triangles in batches, because the VBO might not be able to
        // hold all the vertices at once.
        let (mut picker_fbo, picker_viewport) = cache.picker.fbo();
        for tri_batch in self.mouse_target_tris.chunks(MOUSE_TARGET_BATCH_SIZE * 3) {
            let count = tri_batch.len();
            // Put the data in a slice of the VBO.
            let vbo_slice = vbos.mouse_target_verts(count / 3);
            vbo_slice.write(&tri_batch);

            picker_fbo
                .draw(
                    vbo_slice,
                    &glium::index::NoIndices(PrimitiveType::TrianglesList),
                    &shaders::PICKER.load(),
                    &uniform! { matrix: self.transform.gl_matrix() },
                    &glium::DrawParameters {
                        depth: glium::Depth {
                            test: glium::DepthTest::Overwrite,
                            ..Default::default()
                        },
                        viewport: Some(picker_viewport),
                        ..Default::default()
                    },
                )
                .context("Rendering selection mouse targets")?;
        }

        Ok(self
            .params
            .mouse
            .pos
            // Convert mouse position to `u32`.
            .and_then(|pos| pos[X].to_u32().zip(pos[Y].to_u32()))
            // Get mouse target ID underneath cursor.
            .map(|cursor_pos| self.cache.picker.get_pixel(cursor_pos) as usize)
            // Get mouse target using that ID (subtract 1 because 0 means no
            // target).
            .and_then(|i| self.mouse_targets.get(i.checked_sub(1)?))
            .cloned())
    }
    pub(super) fn add_mouse_target_quad(
        &mut self,
        modifiers: ModifiersState,
        cells: FRect2D,
        data: MouseTargetData,
    ) {
        self.mouse_targets.push(data);
        let target_id = self.mouse_targets.len() as u32; // IDs start at 1
        let NdVec([x1, y1]) = cells.min();
        let NdVec([x2, y2]) = cells.max();
        let corners = [
            NdVec([x1, y1]),
            NdVec([x2, y1]),
            NdVec([x1, y2]),
            NdVec([x2, y2]),
        ];
        self.add_mouse_target_tri(modifiers, [corners[0], corners[1], corners[2]], target_id);
        self.add_mouse_target_tri(modifiers, [corners[3], corners[2], corners[1]], target_id);
    }
    fn add_mouse_target_tri(
        &mut self,
        modifiers: ModifiersState,
        points: [FVec2D; 3],
        target_id: u32,
    ) {
        if self.params.modifiers == modifiers {
            let z = 0.0;
            for &point in &points {
                self.mouse_target_tris.push(MouseTargetVertex {
                    pos: [point[X].raw() as f32, point[Y].raw() as f32, z],
                    target_id,
                })
            }
        }
    }

    /// Returns the render cell position containing the global cell position if
    /// the cell is visible; otherwise, returns the position of the nearest
    /// render cell that is just off-screen.
    pub(super) fn clip_cell_pos_to_visible_render_cells(
        &self,
        cell_pos: &BigVec<R::D>,
    ) -> IVec<R::D> {
        let render_cell_pos =
            (cell_pos - &self.origin).div_floor(&self.render_cell_layer.big_len());
        // Clip to lower edge minus 2 cells for padding.
        let render_cell_pos =
            NdVec::max(&render_cell_pos, &(self.visible_rect.min() - 2).to_bigvec());
        // Clip to upper edge plus 2 cells for padding.
        let render_cell_pos =
            NdVec::min(&render_cell_pos, &(self.visible_rect.max() + 2).to_bigvec());

        render_cell_pos.to_ivec()
    }
    /// Clips the edges of a rectangle using
    /// `clip_cell_pos_to_visible_render_cells()`.
    pub(super) fn clip_cell_rect_to_visible_render_cells(
        &self,
        cells_rect: &BigRect<R::D>,
    ) -> IRect<R::D> {
        NdRect::span(
            self.clip_cell_pos_to_visible_render_cells(&cells_rect.min()),
            self.clip_cell_pos_to_visible_render_cells(&cells_rect.max()),
        )
    }

    /// Clips an ND-tree to just the part that is visible, and returns the slice
    /// of the ND-tree along with a rectangle of render cells relative to that
    /// slice to render.
    pub(super) fn clip_ndtree_to_visible_render_cells<'b>(
        &self,
        params: &'b CellDrawParams<'b, R::D>,
    ) -> Option<(NdTreeSlice<'b, R::D>, IRect<R::D>)> {
        // Clip the global rectangle of visible cells according to the draw
        // parameters.
        let global_visible_rect = match &params.rect {
            Some(rect) => match self
                .render_cell_layer
                .round_rect(&rect)
                .intersection(&self.global_visible_rect)
            {
                // Only draw the intersection of the viewport and the rectangle
                // in the draw parameters.
                Some(intersection) => intersection,
                // The rectangle in the draw parameters does not intersect the
                // viewport, so there is nothing to draw.
                None => return None,
            },
            // There is no rectangle in the parameters, so draw everything in the viewport.
            None => self.global_visible_rect.clone(),
        };

        // Get the `NdTreeSlice` containing all of the visible cells.
        let visible_ndtree = params.ndtree.slice_containing(&global_visible_rect);

        // Convert `global_visible_rect` from cells in global space to render
        // cells relative to `visible_ndtree`.
        let visible_rect = (global_visible_rect - &visible_ndtree.offset)
            .div_outward(&self.render_cell_layer.big_len())
            .to_irect();

        Some((visible_ndtree, visible_rect))
    }

    /// Returns the color to represent an ND-tree node.
    pub(super) fn ndtree_node_color(node: NodeRef<'_, R::D>) -> [u8; 4] {
        if let Some(cell_state) = node.single_state() {
            match cell_state {
                0_u8 => crate::colors::DEAD,
                1_u8 => crate::colors::LIVE,
                i => {
                    let [r, g, b] = colorous::TURBO
                        .eval_rational(257 - i as usize, 256)
                        .as_array();
                    [r, g, b, 255]
                }
            }
        } else {
            let ratio = if node.is_empty() {
                0.0
            } else {
                // Multiply then divide by 255 to keep some precision.
                let population_ratio = (node.population() * 255_usize / node.big_num_cells())
                    .to_f64()
                    .unwrap()
                    / 255.0;
                // Bias so that 50% is the minimum brightness if there are any
                // live cells.
                (population_ratio / 2.0) + 0.5
            };

            // Set alpha to live:dead ratio.
            let mut color = crate::colors::LIVE;
            color[3] = (color[3] as f64 * ratio) as u8;
            color
        }
    }
}

pub trait GridViewRenderDimension<'a>: Default {
    type D: Dim;
    type Camera: Camera<Self::D>;

    const DEFAULT_COLOR: (f32, f32, f32, f32);
    const DEFAULT_DEPTH: f32;
}

#[derive(Debug, Copy, Clone)]
pub(super) struct LineParams {
    /// Line width.
    pub width: f64,
    /// Whether to include the squares at the endpoints of this line.
    pub include_endpoints: bool,
    /// The axis this line is along.
    pub axis: Axis,
}
