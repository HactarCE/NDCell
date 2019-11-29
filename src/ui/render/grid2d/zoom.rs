use std::fmt;

/// How zoomed in the 2D viewport is.
#[derive(Debug, Copy, Clone)]
pub enum Zoom2D {
    /// Each cell takes up a square this many pixels on a side. For example,
    /// Zoom2D::Close(20) means that each cells takes up a 20x20 pixel space.
    Close(usize),
    /// Each pixel contains a square of cells with side length of this power of
    /// 2. For example Zoom2D::Far(3) means that each pixel represents an 8x8 of
    /// cells.
    ///
    /// Zoom2D::Close(1) and Zoom2D::Far(1) are theoretically equivalent.
    Far(usize),
}

impl Default for Zoom2D {
    fn default() -> Self {
        Self::Close(32)
    }
}

impl fmt::Display for Zoom2D {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Zoom2D::Close(n) => write!(f, "1:{}", n),
            Zoom2D::Far(n) => write!(f, "{}:1", 1 << n),
        }
    }
}

impl Zoom2D {
    /// Zoom in by a factor of 2.
    pub fn closer(self) -> Self {
        match self {
            Zoom2D::Close(n) => Zoom2D::Close(n * 2),
            Zoom2D::Far(1) => Zoom2D::Close(1),
            Zoom2D::Far(n) => Zoom2D::Far(n - 1),
        }
    }
    /// Zoom out by a factor of 2.
    pub fn farther(self) -> Self {
        match self {
            Zoom2D::Far(n) => Zoom2D::Far(n + 1),
            Zoom2D::Close(1) => Zoom2D::Far(1),
            Zoom2D::Close(n) => Zoom2D::Close(n / 2),
        }
    }
    pub fn cells_per_pixel(self) -> f32 {
        match self {
            Zoom2D::Close(pixels) => 1.0 / pixels as f32,
            Zoom2D::Far(cell_pow) => 2.0f32.powf(cell_pow as f32),
        }
    }
    pub fn pixels_per_cell(self) -> f32 {
        match self {
            Zoom2D::Close(pixels) => pixels as f32,
            Zoom2D::Far(cell_pow) => 0.5f32.powf(cell_pow as f32),
        }
    }
    pub fn pixels_per_render_cell(self) -> usize {
        match self {
            Zoom2D::Close(pixels) => pixels,
            Zoom2D::Far(_) => 1,
        }
    }
    pub fn node_layer(self) -> usize {
        match self {
            Zoom2D::Close(_) => 0,
            Zoom2D::Far(cell_pow) => cell_pow,
        }
    }
}
