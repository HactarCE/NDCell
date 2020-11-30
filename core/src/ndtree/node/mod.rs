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
mod result;
mod set;

pub use cache::{ArcNode, NodeCache, SimCacheGuard};
pub use layer::{Layer, LayerTooSmall};
pub use raw::RawNode;
pub use refs::{
    CachedNodeRefTrait, LeafNodeRef, NodeRef, NodeRefEnum, NodeRefTrait, NonLeafNodeRef,
};
pub use result::HashLifeResultParams;
use set::ShardedBoxedSet;

#[cfg(test)]
mod tests;
