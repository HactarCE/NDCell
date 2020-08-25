//! Individual nodes in an NdTree.
//!
//! TODO: explain more in-depth how nodes work, leaf vs. non-leaf, why layer < base_layer is
//! allowed (and how they'll be GC'ed), etc.

mod cache;
pub mod cells;
mod raw;
mod refs;
mod repr;

pub use cache::{ArcNode, NodeCache, NodeCacheAccess, NodeHasher};
pub use raw::{NodeFlags, RawNode};
pub use refs::{LeafNodeRef, Node, NodeChildrenIter, NodeRef, NodeRefEnum, NonLeafNodeRef};
pub use repr::{Layer, NodeRepr};
