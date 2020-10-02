#[derive(Debug)]
pub struct GfxConfig {
    pub dpi: f64,
    pub fps: f64,
    pub font_size: f32,
}
impl Default for GfxConfig {
    fn default() -> Self {
        Self {
            dpi: 1.0,
            fps: 60.0,
            font_size: 16.0,
        }
    }
}
