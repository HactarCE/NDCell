use num::BigInt;

#[derive(Debug, Default)]
pub struct Config {
    pub gfx: GfxConfig,
    pub sim: SimConfig,
}

#[derive(Debug)]
pub struct GfxConfig {
    pub dpi: f64,
}
impl Default for GfxConfig {
    fn default() -> Self {
        Self { dpi: 1.0 }
    }
}

#[derive(Debug)]
pub struct SimConfig {
    pub step_size: BigInt,
    pub use_breakpoint: bool,
    pub breakpoint_gen: BigInt,
}
impl Default for SimConfig {
    fn default() -> Self {
        Self {
            step_size: 4.into(),
            use_breakpoint: false,
            breakpoint_gen: 0.into(),
        }
    }
}
