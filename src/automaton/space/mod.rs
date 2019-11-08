//! Everything relating to the spatial "universe" of an automaton, including
//! grid topology and vector operations.

use std::cmp::Eq;
use std::default::Default;
use std::fmt::Debug;
use std::hash::Hash;

mod chunk;
mod coords;
mod coords_container;
mod grid;
mod region;

pub use chunk::*;
pub use coords::*;
pub use coords_container::*;
pub use grid::*;
pub use region::*;

/// A "trait alias" for ndarray::Dimension + std::cmp::Eq + std::hash::Hash so
/// that it can be used in HashMaps.
pub trait Dimension: ndarray::Dimension + Eq + Hash {}
impl<T: ndarray::Dimension + Eq + Hash> Dimension for T {}

/// A "trait alias" for a cell type that has a "default" value and can be copied
/// for free or near-free.
pub trait CellType: Debug + Copy + Default + Eq {}
impl<T: Debug + Copy + Default + Eq> CellType for T {}
