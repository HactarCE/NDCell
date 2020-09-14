use ndcell_core::num::BigInt;

#[derive(Debug, Default)]
pub struct Config {
    pub ctrl: CtrlConfig,
    pub gfx: GfxConfig,
    pub sim: SimConfig,
}

#[derive(Debug)]
pub struct CtrlConfig {
    pub move_speed: f64,
    pub zoom_speed: f64,
    pub base_speed_1: f64,
    pub base_speed_2: f64,
    pub interpolation_2d: Interpolation2D,

    pub immersive: bool,
}
impl Default for CtrlConfig {
    fn default() -> Self {
        Self {
            move_speed: 1000.0,
            zoom_speed: 4.0,
            base_speed_1: 1.0,
            base_speed_2: 3.0,
            interpolation_2d: Interpolation2D::default(),

            immersive: true,
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
    // TODO: consider adding setter for step_size
}
