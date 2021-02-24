use palette::Srgba;

#[derive(Debug)]
pub struct GfxConfig {
    pub dpi: f64,
    pub fps: f64,
    pub font_size: f32,

    pub msaa: Msaa,

    pub octree_perf_view: bool,

    pub cell_colors: [Srgba; 256],
}
impl Default for GfxConfig {
    fn default() -> Self {
        Self {
            dpi: 1.0,
            fps: 60.0,
            font_size: 16.0,

            msaa: Msaa::_8,

            octree_perf_view: false,

            cell_colors: crate::default_colors(),
        }
    }
}
impl GfxConfig {
    /// Returns the duration of one frame based on the configured FPS value.
    pub fn frame_duration(&self) -> std::time::Duration {
        std::time::Duration::from_secs_f64(1.0 / self.fps)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Msaa {
    Off = 0,
    _2 = 2,
    _4 = 4,
    _8 = 8,
}
