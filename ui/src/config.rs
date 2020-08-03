use num::BigInt;

#[derive(Debug, Default)]
pub struct Config {
    pub ctrl: CtrlConfig,
    pub gfx: GfxConfig,
    pub sim: SimConfig,
}

#[derive(Debug)]
pub struct CtrlConfig {
    pub interpolation_2d: Interpolation2D,
}
impl Default for CtrlConfig {
    fn default() -> Self {
        Self {
            interpolation_2d: Interpolation2D::default(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Interpolation2D {
    None,
    Linear { speed: f64 },
    Exponential { decay_constant: f64 },
}
impl Default for Interpolation2D {
    fn default() -> Self {
        Self::Exponential {
            decay_constant: 0.08,
        }
    }
}

#[derive(Debug)]
pub struct GfxConfig {
    pub fps: f64,
    pub dpi: f64,
}
impl Default for GfxConfig {
    fn default() -> Self {
        Self {
            fps: 60.0,
            dpi: 1.0,
        }
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
