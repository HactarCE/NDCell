//! Everything relating to the spatial "universe" of an automaton, including
//! grid topology and vector operations.

use std::cmp::Eq;
use std::default::Default;
use std::fmt::Debug;
use std::hash::Hash;

mod ndarray;
mod ndrect;
mod ndtree;
mod ndvec;

pub use ndarray::*;
pub use ndrect::*;
pub use ndtree::*;
pub use ndvec::*;
