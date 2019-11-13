//! Everything relating to the spatial "universe" of an automaton, including
//! grid topology and vector operations.

use std::cmp::Eq;
use std::default::Default;
use std::fmt::Debug;
use std::hash::Hash;

mod ndtree;
mod ndvec;

pub use ndtree::*;
pub use ndvec::*;

/// A "trait alias" for a cell type that has a "default" value and can be copied
/// for free or near-free.
pub trait CellType: Debug + Copy + Default + Eq + Hash {}
impl<T: Debug + Copy + Default + Eq + Hash> CellType for T {}
