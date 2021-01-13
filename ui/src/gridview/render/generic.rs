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
use crate::ext::*;
use crate::gridview::*;

pub struct GenericGridViewRender<'a, R: GridViewRenderDimension<'a>> {
    pub(super) cache: RefMut<'a, super::RenderCache>,
    pub(super) params: RenderParams<'a>,
    pub(super) dim: R,

    /// Viewpoint to render the grid from.
    pub(super) viewpoint: &'a R::Viewpoint,
    /// Transform from global space to local space (1 unit = 1 render cell; (0,
    /// 0) = `origin`), to NDC ((-1, -1) = bottom left; (1, 1) = top right), and
    ///    to pixel space (1 unit = 1 pixel; (0, 0) = top left).
    pub(super) xform: NdCellTransform<R::D>,
    /// Global rectangle of visible cells, rounded to render cell boundaries.
    pub(super) global_visible_rect: BigRect<R::D>,
    /// Local rectangle of visible render cells.
    pub(super) local_visible_rect: IRect<R::D>,

    /// Mouse targets, indexed by ID.
    mouse_targets: Vec<MouseTargetData>,
    /// Vertex data for mouse targets.
    mouse_target_tris: Vec<MouseTargetVertex>,
}
impl<'a, R: GridViewRenderDimension<'a>> GenericGridViewRender<'a, R> {
    /// Creates a `GridViewRender` for a gridview.
    pub fn new(params: RenderParams<'a>, viewpoint: &'a R::Viewpoint) -> Self {
        let mut cache = super::CACHE.borrow_mut();

        // Initialize color and depth buffers.
        params
            .target
            .clear_color_srgb_and_depth(R::DEFAULT_COLOR, R::DEFAULT_DEPTH);

        // Initialize mouse picker.
        cache.picker.init(params.target.get_dimensions());

        // Compute the transformation from individual cells all the way to
        // pixels.
        let xform = viewpoint.cell_transform();

        // Determine the rectangle of visible cells in global coordinate space,
        // rounded to the nearest render cell.
        let global_visible_rect: BigRect<R::D> = xform
            .render_cell_layer
            .round_rect(&viewpoint.global_visible_rect());
        let local_visible_rect: IRect<R::D> = xform
            .global_to_local_int_rect(&global_visible_rect)
            .expect("Unreasonable visible rectangle");

        R::init(Self {
            cache,
            params,
            dim: R::default(),

            viewpoint,
            xform,
            global_visible_rect,
            local_visible_rect,

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
                    &uniform! { matrix: self.xform.gl_matrix() },
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
                let [x, y] = point.to_f32_array();
                self.mouse_target_tris.push(MouseTargetVertex {
                    pos: [x, y, z],
                    target_id,
                })
            }
        }
    }

    /// Converts a global position to a local position, clamping it to the
    /// visible rectangle plus a few cells on each side for padding. This method
    /// always returns a local position that is either within the visible
    /// rectangle or just barely outside it.
    pub(super) fn clamp_int_pos_to_visible(&self, global_pos: &BigVec<R::D>) -> IVec<R::D> {
        // Padding = 2 render cells
        let padding = self.xform.render_cell_layer.big_len() * 2;

        let lower = self.global_visible_rect.min() - &padding;
        let global_pos = NdVec::max(global_pos, &lower);
        let upper = self.global_visible_rect.max() + &padding;
        let global_pos = NdVec::min(&global_pos, &upper);

        self.xform.global_to_local_int(&global_pos).unwrap()
    }
    /// Converts a global rectangle to local space using
    /// `clamp_int_pos_to_visible()`.
    pub(super) fn clip_int_rect_to_visible(&self, cells_rect: &BigRect<R::D>) -> IRect<R::D> {
        NdRect::span(
            self.clamp_int_pos_to_visible(&cells_rect.min()),
            self.clamp_int_pos_to_visible(&cells_rect.max()),
        )
    }

    /// Returns a slice of an ND-tree encompassing the visible area. It is safe
    /// to convert the offset of that slice to local coordinates.
    pub(super) fn clip_ndtree_to_visible<'b>(
        &self,
        params: &'b CellDrawParams<'b, R::D>,
    ) -> Option<NdTreeSlice<'b, R::D>> {
        // Clip the global rectangle of visible cells according to the draw
        // parameters.
        let global_visible_rect = match &params.rect {
            Some(rect) => match self
                .xform
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
            // There is no rectangle in the parameters, so draw everything in
            // the viewport.
            None => self.global_visible_rect.clone(),
        };

        // Get the `NdTreeSlice` containing all of the visible cells.
        let visible_ndtree = params.ndtree.slice_containing(&global_visible_rect);

        Some(visible_ndtree)
    }

    pub(super) fn gridline_cell_spacing_exponent(&self, max_pixel_spacing: f64) -> u32 {
        // Compute the global cell spacing between gridlines.
        let log2_max_pixel_spacing = r64(max_pixel_spacing).log2();
        let log2_max_cell_spacing = log2_max_pixel_spacing - self.viewpoint.scale().log2_factor();

        // Undo the `a * b^n` formula, rounding up to the nearest power of
        // `GRIDLINE_SPACING_BASE`.
        let log2_a = (GRIDLINE_SPACING_COEFF as f64).log2();
        let log2_b = (GRIDLINE_SPACING_BASE as f64).log2();
        ((log2_max_cell_spacing - log2_a) / log2_b)
            .ceil()
            .to_u32()
            .unwrap_or(0)
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
    type Viewpoint: Viewpoint<Self::D>;

    const DEFAULT_COLOR: (f32, f32, f32, f32);
    const DEFAULT_DEPTH: f32;

    fn init(this: GenericGridViewRender<'a, Self>) -> GenericGridViewRender<'a, Self> {
        this
    }
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
