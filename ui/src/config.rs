use ndcell_core::num::BigInt;

#[derive(Debug, Default)]
pub struct Config {
    pub ctrl: CtrlConfig,
    pub gfx: GfxConfig,
    pub sim: SimConfig,
}

#[derive(Debug)]
pub struct CtrlConfig {
    pub keybd_move_speed_2d: f64,
    pub keybd_move_speed_3d: f64,
    pub keybd_scale_speed_2d: f64,
    pub keybd_scale_speed_3d: f64,
    pub discrete_scale_speed_2d: f64,
    pub discrete_scale_speed_3d: f64,
    pub smooth_scroll_speed_2d: f64,
    pub smooth_scroll_speed_3d: f64,
    // TODO: make speed_modifier an attribute of the keybind
    pub speed_modifier: f64,

    pub snap_pos_2d: bool,
    pub snap_pos_3d: bool,
    pub snap_scale_2d: bool,
    pub snap_scale_3d: bool,

    pub up_axis_3d: UpAxis3D,
    pub fwd_axis_3d: ForwardAxis3D,

    pub interpolation: Interpolation,
}
impl Default for CtrlConfig {
    fn default() -> Self {
        Self {
            keybd_move_speed_2d: 1000.0,
            keybd_move_speed_3d: 250.0,
            keybd_scale_speed_2d: 4.0,
            keybd_scale_speed_3d: 2.0,
            discrete_scale_speed_2d: 1.0,
            discrete_scale_speed_3d: 0.5,
            smooth_scroll_speed_2d: 1.0,
            smooth_scroll_speed_3d: 0.5,
            speed_modifier: 3.0,

            snap_pos_2d: true,
            snap_pos_3d: true,
            snap_scale_2d: true,
            snap_scale_3d: false,

            up_axis_3d: UpAxis3D::default(),
            fwd_axis_3d: ForwardAxis3D::default(),

            interpolation: Interpolation::default(),
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

#[derive(Debug, Copy, Clone)]
pub enum UpAxis3D {
    /// "Up" is relative to the camera orientation.
    Camera,
    /// "Up" is fixed.
    Fixed,
}
impl Default for UpAxis3D {
    fn default() -> Self {
        Self::Fixed
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ForwardAxis3D {
    /// "Forward" is the direction the camera is pointing.
    Camera,
    /// "Forward" is the direction the camera is pointing, projected onto the
    /// flat plane.
    Flat,
    /// "Forward" is along the axis nearest to the direction the camera is
    /// pointing.
    Aligned,
    /// "Forward" is along the non-vertical axis nearest to the direction the
    /// camera is pointing.
    FlatAligned,
}
impl Default for ForwardAxis3D {
    fn default() -> Self {
        Self::FlatAligned
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
