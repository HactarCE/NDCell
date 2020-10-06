mod ctrl;
mod gfx;
mod hist;
mod keys;
mod sim;

pub use ctrl::*;
pub use gfx::*;
pub use hist::*;
pub use keys::*;
pub use sim::*;

#[derive(Debug, Default)]
pub struct Config {
    pub ctrl: CtrlConfig,
    pub gfx: GfxConfig,
    pub hist: HistoryConfig,
    pub keys: KeyConfig,
    pub sim: SimConfig,
}
