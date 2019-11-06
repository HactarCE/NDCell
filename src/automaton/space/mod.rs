//! Everything relating to the spatial "universe" of an automaton, including
//! grid topology and vector operations.

use std::cmp::Eq;
use std::default::Default;
use std::hash::Hash;

mod grid;
mod vector;

pub use grid::Grid;
pub use vector::Vector;

/// A "trait alias" for ndarray::Dimension + std::cmp::Eq + std::hash::Hash so
/// that it can be used in HashMaps.
pub trait Dimension: ndarray::Dimension + Eq + Hash {}
impl<T: ndarray::Dimension + Eq + Hash> Dimension for T {}

/// A "trait alias" for a cell type that has a "default" value and can be copied
/// for free or near-free.
pub trait Cell: Copy + Default + Eq {}
impl<T: Copy + Default + Eq> Cell for T {}
