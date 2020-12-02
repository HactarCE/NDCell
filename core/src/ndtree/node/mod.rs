//! Individual nodes in an ND-tree.
//!
//! Each node is either a leaf node or a non-leaf node. Leaf nodes contain an
//! array of cells, while non-leaf nodes contain an array of pointers to smaller
//! nodes. Each node has a "layer," which is the base-2 log of the size of the
//! node along each axis.

pub mod cells;
mod layer;
mod pool;
mod raw;
mod refs;
mod results;
mod set;

pub use layer::{Layer, LayerTooSmall};
pub use pool::{ArcNode, NodePool, SharedNodePool, SimCacheGuard};
use raw::RawNode;
pub use refs::{LeafNodeRef, NodeRef, NodeRefEnum, NodeRefTrait, NodeRefWithGuard, NonLeafNodeRef};
pub use results::HashLifeResultParams;
use set::ShardedBoxedSet;

#[cfg(test)]
mod tests;
