//! Individual nodes in an NdTree.
//!
//! This is based somewhat on hlife_algo.cpp in Golly.
//!
//! TODO: explain more in-depth how nodes work, why layer < base_layer is
//! allowed (and how they'll be GC'ed), etc.

//! Individual nodes in an NdTree.

mod cache;
mod raw;
mod refs;
mod repr;
pub mod utils;

// TODO: try to get rid of utils

pub use cache::{ArcNode, NodeCache, NodeCacheAccess, NodeHasher};
pub use raw::{NodeFlags, RawNode};
pub use refs::{LeafNodeRef, Node, NodeChildren, NodeRef, NodeRefEnum, NonLeafNodeRef};
pub use repr::{Layer, NodeRepr};
