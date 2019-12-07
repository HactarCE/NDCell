//! Data structures to access to a lower-dimensional slice of an N-dimensional
//! automaton.

use crate::automaton::*;

mod generic;
mod info;
mod oct;
mod quad;
mod quad_impl;

pub use generic::*;
pub use info::*;
pub use oct::*;
pub use quad::*;
