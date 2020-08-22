//! Individual nodes in an NdTree.
//!
//! This is based somewhat on hlife_algo.cpp in Golly.
//!
//! TODO: explain more in-depth how nodes work, why layer < base_layer is
//! allowed (and how they'll be GC'ed), etc.

//! Individual nodes in an NdTree.

mod cache;
pub mod math;
mod raw;
mod refs;
mod repr;
pub mod utils;

// TODO: reconsider node_math and node_utils
pub mod node_math {
    pub use super::math::*;
}
pub mod node_utils {
    pub use super::utils::*;
}
pub use cache::{ArcNode, NodeCache, NodeCacheAccess, NodeHasher};
pub use raw::{NodeFlags, RawNode};
pub use refs::{LeafNodeRef, Node, NodeChildren, NodeRef, NodeRefEnum, NonLeafNodeRef};
pub use repr::{NodeCells, NodeRepr};
