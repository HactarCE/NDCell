//! Rules and simulation algorithms.

mod hashlife;
pub mod rule;
mod simulate;

pub use hashlife::HashLife;
pub use simulate::{AsSimulate, Simulate};
