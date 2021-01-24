//! 2D and 3D grid rendering.

use glium::glutin::event::ModifiersState;
use send_wrapper::SendWrapper;
use std::cell::RefCell;

use ndcell_core::prelude::*;

use crate::config::MouseDragBinding;
use crate::mouse::MouseState;

mod generic;
mod gl_ndtree;
mod ibos;
mod picker;
mod render2d;
mod render3d;
mod resizing;
mod shaders;
mod textures;
mod vbos;
mod vertices;

pub(super) use render2d::GridViewRender2D;
pub(super) use render3d::GridViewRender3D;

mod consts {
    /// Minimum pixel width of 2D lines.
    pub const LINE_MIN_PIXEL_WIDTH_2D: f64 = 1.0;
    /// Minimum pixel width of 3D lines.
    pub const LINE_MIN_PIXEL_WIDTH_3D: f64 = 0.75;

    /// Minimum pixel length of the crosshair gradient.
    pub const CROSSHAIR_GRADIENT_MIN_PIXEL_LEN: f64 = 16.0;
    /// Minimum render cell length of the crosshair gradient.
    pub const CROSSHAIR_GRADIENT_MIN_CELL_LEN: f64 = 1.0;

    /// Small value for avoiding Z fighting.
    pub const Z_EPSILON: f64 = 1.0 / 256.0;
    /// Padding added to integer cuboid overlays, measured in render cells.
    pub const CUBOID_OVERLAY_PADDING: f64 = GRIDLINE_WIDTH / 2.0;

    /// Width of gridlines, measured in cells.
    pub const GRIDLINE_WIDTH: f64 = 1.0 / 32.0;
    /// Width of hover outline, measured in cells.
    pub const HOVER_HIGHLIGHT_WIDTH: f64 = 2.0 * GRIDLINE_WIDTH;
    /// Width of selection outline, measured in cells.
    pub const SELECTION_HIGHLIGHT_WIDTH: f64 = 4.0 * GRIDLINE_WIDTH;
    /// Width of selection resize preview outline, measured in cells.
    pub const SELECTION_RESIZE_PREVIEW_WIDTH: f64 = 2.0 * GRIDLINE_WIDTH;

    /// Coefficient to use for gridline spacing.
    ///
    /// `a` in `a * b^n`
    pub const GRIDLINE_SPACING_COEFF: usize = 1;
    /// Exponential base to use for gridline spacing.
    ///
    /// `b` in `a * b^n`
    ///
    /// The value of `n` linearly influences the pixel spacing between
    /// gridlines, which is used to determine their opacity via GLSL
    /// `smoothstep()`; see `GRIDLINE_ALPHA_GRADIENT_LOW_PIXEL_SPACING` and
    /// `GRIDLINE_ALPHA_GRADIENT_HIGH_PIXEL_SPACING`.
    pub const GRIDLINE_SPACING_BASE: usize = 8;
    /// Maximum pixel spacing between gridlines with zero opacity (low end of
    /// gradient).
    pub const GRIDLINE_ALPHA_GRADIENT_LOW_PIXEL_SPACING: f64 = 4.0;
    /// Minimum pixel spacing between gridlines with full opacity (high end of
    /// gradient).
    pub const GRIDLINE_ALPHA_GRADIENT_HIGH_PIXEL_SPACING: f64 = 256.0;
    /// 2D gridline opacity multiplier when zoomed out beyond 1:1.
    ///
    /// This prevents gridlines from completely obscuring the presence of cells.
    pub const GRIDLINE_ALPHA_ZOOMED_OUT_MULT_2D: f64 = 0.75;

    /// Number of quads in each render batch.
    pub const QUAD_BATCH_SIZE: usize = 4096;
    /// Number of mouse target rectangles in each render batch.
    pub const MOUSE_TARGET_BATCH_SIZE: usize = 256;

    /// Direction that 3D light comes from (normalized in GLSL).
    pub const LIGHT_DIRECTION: [f32; 3] = [1.0, 7.0, -3.0];
    /// Proportion of 3D light that is ambient, as opposed to directional.
    pub const LIGHT_AMBIENTNESS: f32 = 0.4;
    /// Constant 3D lighting multiplier.
    pub const LIGHT_MULTIPLIER: f32 = 1.0;

    /// Proportional radius of the visible area beyond which there is fog.
    pub const FOG_START_FACTOR: f32 = 0.5;
}

lazy_static! {
    static ref CACHE: SendWrapper<RefCell<RenderCache>> =
        SendWrapper::new(RefCell::new(RenderCache::default()));
}

/// Parameters that may control the rendering process.
pub struct RenderParams<'a> {
    /// Render target.
    pub target: &'a mut glium::Frame,
    /// Mouse state.
    pub mouse: MouseState,
    /// Modifiers held on the keyboard.
    pub modifiers: ModifiersState,
}

/// Data generated when rendering a frame.
#[derive(Debug, Default, Clone)]
pub struct RenderResult {
    /// Target under the mouse cursor, if any.
    pub mouse_target: Option<MouseTargetData>,
}

pub(super) struct CellDrawParams<'a, D: Dim> {
    /// ND-tree to draw.
    pub ndtree: &'a NdTree<D>,
    /// Rectangular region of the ND-tree to draw.
    pub rect: Option<&'a BigRect<D>>,
    /// Alpha value for the whole ND-tree.
    pub alpha: f32,
}

/// How to handle a mouse hover or click on a particular location on the screen.
#[derive(Debug, Clone)]
pub struct MouseTargetData {
    /// Mouse binding for clicking the left mouse button over the target and
    /// dragging.
    pub binding: MouseDragBinding,
}

#[derive(Default)]
struct RenderCache {
    pub ibos: ibos::IboCache,
    pub vbos: vbos::VboCache,
    pub textures: textures::TextureCache,
    pub picker: picker::MousePicker,
    pub gl_quadtrees: gl_ndtree::GlQuadtreeCache,
    pub gl_octrees: gl_ndtree::GlOctreeCache,
}

pub fn post_frame_clean_cache() {
    let mut cache = CACHE.borrow_mut();
    cache.gl_quadtrees.post_frame_clean_cache();
}

pub fn hot_reload_shaders() {
    shaders::hot_reload_all();
}
