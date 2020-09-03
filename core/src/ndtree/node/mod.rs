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

pub use cache::{ArcNode, NodeCache, NodeHasher};
pub use layer::Layer;
pub use raw::RawNode;
pub use refs::{
    CachedNodeRefTrait, LeafNodeRef, NodeRef, NodeRefEnum, NodeRefTrait, NonLeafNodeRef,
};

#[cfg(test)]
mod tests;
