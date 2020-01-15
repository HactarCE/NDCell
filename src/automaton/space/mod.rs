//! Everything relating to the spatial "universe" of an automaton, including
//! grid topology and vector operations.

use std::cmp::Eq;
use std::default::Default;
use std::fmt::Debug;
use std::hash::Hash;

// mod ndrect;
// mod ndtree;
mod ndvec;

// pub use ndrect::*;
// pub use ndtree::*;
// pub use ndvec::*;

// /// A "trait alias" for a cell type that has a "default" value and can be copied
// /// for free or near-free.
// pub trait CellType: Debug + Copy + Default + Eq + Hash {}
// impl<T: Debug + Copy + Default + Eq + Hash> CellType for T {}

// /// A trait to allow overloading of the contains() method.
// pub trait CanContain<I> {
//     /// Returns true if `inner` is within `self`; usually implemented for
//     /// structs that range over a set of cells on a grid with respect to cell
//     /// position vectors.
//     fn contains(&self, inner: I) -> bool;
// }
