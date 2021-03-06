//! Low-level N-dimensional cellular automaton storage and simulation backend.
//!
//! # Arrays
//!
//! N-dimensional arrays appear quite frequently in this crate. These are always
//! stored in generalized row-major order, so the X axis is always "least
//! significant." For example, a 3D 4x3x2 array would be laid out as follows,
//! with alphabetical order representing order in memory:
//!
//! ```text
//!         (z = 0)    (z = 1)
//! x =     0 1 2 3    0 1 2 3
//!
//! y = 0   A B C D    M N O P
//! y = 1   E F G H    Q R S T
//! y = 2   I J K L    U V W X
//! ```
//!
//! (`A` is stored at index 0, `B` at index 1, etc. up to `X` at index 23.)
//!
//! # Strides
//!
//! To index an array, we create a vector of **strides**; each stride is the
//! number of elements to traverse that is equivalent to one unit along that
//! axis. For example, the strides for the array above would be `[1, 4, 12]`.
//!
//! Conveniently, taking the dot product of the strides with a position vector
//! gives the integer index of that position in the array. For example, the dot
//! product of `[1, 4, 12]` and `[3, 0, 1]` is `15`, which is indeed the index
//! of the element at `[3, 0, 1]` (letter `P` in the array above).

#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(rust_2018_idioms)]
#![warn(clippy::all)]
#![deny(clippy::correctness)]

pub mod io;
#[macro_use]
mod macros;
pub mod automaton;
pub mod axis;
pub mod dim;
mod lazyvec;
pub mod ndarray;
pub mod ndrect;
pub mod ndtree;
pub mod ndvec;
pub mod num;
pub mod sim;

/// Fast non-cryptographic hasher.
type FastHashBuilder = std::hash::BuildHasherDefault<seahash::SeaHasher>;
/// `HashMap` using a fast non-cryptographic hasher.
type HashMap<K, V> = std::collections::HashMap<K, V, FastHashBuilder>;

/// Header used for Macrocell format.
const MACROCELL_HEADER: &str = concat!("[M2] (ndcell ", env!("CARGO_PKG_VERSION"), ")");

/// All public traits, for easy importing.
pub mod traits {
    pub use crate::dim::Dim;
    pub use crate::io::{CaFormatTrait, ResolveRule};
    pub use crate::ndrect::CanContain;
    pub use crate::ndtree::NodeRefTrait;
    pub use crate::num::{Float, FromPrimitive, Integer, Num, One, Signed, ToPrimitive, Zero};
    pub use crate::sim::rule::NdRule;
    pub use crate::sim::{AsSimulate, Simulate};
}

/// Prelude module.
pub mod prelude {
    pub use crate::traits::*;

    pub use crate::automaton::{
        Automaton, Automaton1D, Automaton2D, Automaton3D, Automaton4D, Automaton5D, Automaton6D,
        AutomatonMut, AutomatonRef, NdAutomaton,
    };
    pub use crate::axis::{Axis, AxisSet};
    pub use crate::dim::{Dim1D, Dim2D, Dim3D, Dim4D, Dim5D, Dim6D};
    pub use crate::io::{
        CaFormat, CaFormatError, Macrocell, MacrocellError, MacrocellResult, Rle, RleError,
        RleResult, TwoState,
    };
    pub use crate::ndrect::aliases::*;
    pub use crate::ndrect::NdRect;
    pub use crate::ndtree::aliases::*;
    pub use crate::ndtree::{
        ArcNode, FlatNdTree, FlatNdTreeNode, Layer, LeafNodeRef, NdTree, NdTreeSlice, NodePool,
        NodeRef, NodeRefEnum, NonLeafNodeRef, Region, SharedNodePool,
    };
    pub use crate::ndvec::aliases::*;
    pub use crate::ndvec::{AnyDimVec, NdVec};
    pub use crate::num::{r64, BigInt, BigUint, FixedPoint, Sign, R64};
    pub use crate::sim::hashlife;
    pub use crate::sim::rule::aliases::*;
    pub use crate::sim::rule::{Rule, TransitionFunction};
}

#[cfg(test)]
mod tests;
