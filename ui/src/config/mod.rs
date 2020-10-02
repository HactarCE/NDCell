mod ctrl;
mod gfx;
mod keys;
mod sim;

pub use ctrl::*;
pub use gfx::*;
pub use keys::*;
pub use sim::*;

#[derive(Debug, Default)]
pub struct Config {
    pub ctrl: CtrlConfig,
    pub keys: KeyConfig,
    pub gfx: GfxConfig,
    pub sim: SimConfig,
}
