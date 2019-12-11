//! The simulation backend.
//!
//! This module contains everything needed to simulate cellular automata,
//! without displaying, importing, or exporting them.

use std::marker::PhantomData;

pub mod projection;
pub mod rule;
pub mod simulation;
pub mod space;

pub use projection::*;
pub use rule::{DummyRule, Rule};
pub use simulation::*;
pub use space::*;

#[cfg(test)]
mod tests;
