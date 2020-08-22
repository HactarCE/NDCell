//! N-dimensional cellular automaton storage and simulation backend.

#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(rust_2018_idioms)]
#![warn(clippy::all)]
#![deny(clippy::correctness)]

#[macro_use]
extern crate pest_derive;

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

pub mod traits {
    pub use crate::automaton::NdProjectedAutomatonTrait;
    pub use crate::dim::Dim;
    pub use crate::io::rle::RleEncode;
    pub use crate::ndrect::CanContain;
    pub use crate::ndtree::Node;
    pub use crate::num::{Float, FromPrimitive, Integer, Num, One, Signed, ToPrimitive, Zero};
    pub use crate::projection::NdProjector;
    pub use crate::sim::{AsNdSimulate, NdSimulate};
}

pub mod prelude {
    pub use crate::traits::*;

    pub use crate::automaton::{
        Automaton, Automaton1D, Automaton2D, Automaton3D, Automaton4D, Automaton5D, Automaton6D,
        ProjectedAutomaton, ProjectedAutomaton1D, ProjectedAutomaton2D, ProjectedAutomaton3D,
        ProjectedAutomaton4D, ProjectedAutomaton5D, ProjectedAutomaton6D,
    };
    pub use crate::axis::Axis;
    pub use crate::dim::{Dim1D, Dim2D, Dim3D, Dim4D, Dim5D, Dim6D};
    pub use crate::ndrect::aliases::*;
    pub use crate::ndrect::NdRect;
    pub use crate::ndtree::{
        ArcNode, LeafNodeRef, NdTree, NdTreeSlice, NodeCache, NodeCacheAccess, NodeRef,
        NodeRefEnum, NodeRepr, NonLeafNodeRef,
    };
    pub use crate::ndvec::aliases::*;
    pub use crate::ndvec::{AnyDimVec, NdVec};
    pub use crate::num::{r64, BigInt, BigUint, R64};
    pub use crate::sim::Simulation;

    // TODO: consider including things from crate::automaton
}

#[cfg(test)]
mod tests;
