#[derive(Debug)]
pub struct GfxConfig {
    pub dpi: f64,
    pub fps: f64,
    pub font_size: f32,

    pub octree_perf_view: bool,
}
impl Default for GfxConfig {
    fn default() -> Self {
        Self {
            dpi: 1.0,
            fps: 60.0,
            font_size: 16.0,

            octree_perf_view: false,
        }
    }
}
impl GfxConfig {
    /// Returns the duration of one frame based on the configured FPS value.
    pub fn frame_duration(&self) -> std::time::Duration {
        std::time::Duration::from_secs_f64(1.0 / self.fps)
    }
}
