//! 2D and 3D grid rendering.

mod gl_quadtree;
pub mod grid2d;
pub mod grid3d;
mod ibos;
mod shaders;
mod textures;
mod vbos;
mod vertices;

mod consts {
    /// Exponential base to use when fading out gridlines. 16 = 16 small gridlines
    /// between each large gridline.
    pub const GRIDLINE_SPACING_BASE: usize = 16;
    /// Minimum number of pixels between gridlines.
    pub const MIN_GRIDLINE_SPACING: f64 = 4.0;
    /// Minimum number of pixels between gridlines with full opacity.
    pub const MAX_GRIDLINE_SPACING: f64 = 256.0;
    /// Maximum opacity of gridlines when zoomed out beyond one cell per pixel.
    ///
    /// This is so that gridlines do not completely obscure the presence of
    /// cells.
    pub const ZOOMED_OUT_MAX_GRID_ALPHA: f64 = 0.75;

    /// Color of the grid. This will be configurable in the future.
    pub const GRID_COLOR: [f32; 4] = [0.25, 0.25, 0.25, 1.0];
    /// Color given to the highlighted cell. This will be configurable in the
    /// future.
    pub const GRID_HIGHLIGHT_COLOR: [f32; 4] = [0.0, 0.5, 1.0, 1.0];
    /// Color for dead cells. This will be configurable in the future.
    pub const DEAD_COLOR: (u8, u8, u8) = (0, 0, 0);
    /// Color for live cells. This will be configurable in the future.
    pub const LIVE_COLOR: (u8, u8, u8) = (255, 255, 255);

    /// Number of cell overlay rectangles in each render batch.
    pub const CELL_OVERLAY_BATCH_SIZE: usize = 256;

    /// Depth at which to render gridlines.
    pub const GRIDLINE_DEPTH: f32 = 0.1;
    /// Depth at which to render highlight/crosshairs.
    pub const CURSOR_DEPTH: f32 = 0.2;

    /// A small offset used to force correct Z order or align things at the
    /// sub-pixel scale.
    pub const TINY_OFFSET: f32 = 1.0 / 16.0;
}
