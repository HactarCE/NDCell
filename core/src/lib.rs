//! N-dimensional cellular automaton storage and simulation backend.

#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(rust_2018_idioms)]
#![warn(clippy::all)]
#![deny(clippy::correctness)]

#[macro_use]
extern crate pest_derive;

use enum_dispatch::enum_dispatch;
use std::convert::TryInto;
use std::sync::{Arc, Mutex};

mod io;
#[macro_use]
mod macros;
mod automaton;
pub mod axis;
mod dim;
pub mod math;
pub mod ndarray;
pub mod ndrect;
pub mod ndtree;
pub mod ndvec;
pub mod num;
pub mod projection;
pub mod sim;

// TODO: make a prelude

// TODO: probably don't `pub use crate::num::*;`

pub use crate::num::*;
pub use automaton::*;
pub use axis::Axis;
pub use dim::*;
pub use io::*;
pub use ndarray::*;
pub use ndrect::*;
pub use ndtree::*;
pub use ndvec::*;
pub use projection::*;
pub use rle::RleEncode;
pub use sim::rule::{DummyRule, Rule, TransitionFunction};
pub use sim::{ndsimulate::*, simulation::*};

#[cfg(test)]
mod tests;
