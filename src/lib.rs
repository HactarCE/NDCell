//! N-dimensional cellular automaton simulation library

#![allow(dead_code)]
#![warn(missing_docs)]

#[macro_use]
extern crate pest_derive;

mod automaton;
mod math;

pub use automaton::*;
