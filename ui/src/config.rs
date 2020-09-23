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
    pub scale_speed: f64,
    pub base_speed_1: f64,
    pub base_speed_2: f64,

    pub snap_pos_2d: bool,
    pub snap_scale_2d: bool,
    pub snap_pos_3d: bool,
    pub snap_scale_3d: bool,

    pub interpolation: Interpolation,

    pub immersive: bool,
}
impl Default for CtrlConfig {
    fn default() -> Self {
        Self {
            move_speed: 1000.0,
            scale_speed: 4.0,
            base_speed_1: 1.0,
            base_speed_2: 3.0,

            snap_pos_2d: true,
            snap_scale_2d: true,
            snap_pos_3d: true,
            snap_scale_3d: true,

            interpolation: Interpolation::default(),

            immersive: false,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Interpolation {
    None,
    Linear { speed: f64 },
    Exponential { decay_constant: f64 },
}
impl Default for Interpolation {
    fn default() -> Self {
        Self::Exponential {
            decay_constant: 0.08,
        }
    }
}

#[derive(Debug)]
pub struct GfxConfig {
    pub fps: f64,
    pub font_size: f32,
}
impl Default for GfxConfig {
    fn default() -> Self {
        Self {
            fps: 60.0,
            font_size: 16.0,
        }
    }
}

#[derive(Debug)]
pub struct SimConfig {
    pub step_size: BigInt,
    pub use_breakpoint: bool,
    pub breakpoint_gen: BigInt,
    pub max_memory: usize,
}
impl Default for SimConfig {
    fn default() -> Self {
        Self {
            step_size: 4.into(),
            use_breakpoint: false,
            breakpoint_gen: 0.into(),
            max_memory: 1024 * 1024 * 1024, // 1 GiB
        }
    }
    // TODO: consider adding setter for step_size
}
