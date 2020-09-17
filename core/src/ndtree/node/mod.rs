//! Individual nodes in an ND-tree.
//!
//! Each node is either a leaf node or a non-leaf node. Leaf nodes contain an
//! array of cells, while non-leaf nodes contain an array of pointers to smaller
//! nodes. Each node has a "layer," which is the base-2 log of the size of the
//! node along each axis.

mod cache;
pub mod cells;
mod layer;
mod raw;
mod refs;
mod set;

pub use cache::{ArcNode, HashLifeParams, NodeCache};
pub use layer::Layer;
pub use raw::RawNode;
pub use refs::{
    CachedNodeRefTrait, LeafNodeRef, NodeRef, NodeRefEnum, NodeRefTrait, NonLeafNodeRef,
};
use set::ShardedBoxedSet;

#[cfg(test)]
mod tests;
