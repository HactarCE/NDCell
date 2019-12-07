//! Information describing how to take an NdTree of a given number of dimensions
//! and get a lower-dimensional slice or projection of it.

use super::*;

mod generic;
mod oct;
mod quad;

pub use generic::*;
pub use oct::*;
pub use quad::*;
