//! Type aliases for `NdTree`s.

use super::{NdTree, NdTreeSlice};
use crate::dim::*;

/// 1D grid represented as a bintree.
pub type NdTree1D = NdTree<Dim1D>;
/// 2D grid represented as a quadtree.
pub type NdTree2D = NdTree<Dim2D>;
/// 3D grid represented as an octree.
pub type NdTree3D = NdTree<Dim3D>;
/// 4D grid represented as a tree with branching factor 16.
pub type NdTree4D = NdTree<Dim4D>;
/// 5D grid represented as a tree with branching factor 32.
pub type NdTree5D = NdTree<Dim5D>;
/// 6D grid represented as a tree with branching factor 64.
pub type NdTree6D = NdTree<Dim6D>;

/// Slice of a 1D grid represented as a bintree.
pub type NdTreeSlice1D<'cache> = NdTreeSlice<'cache, Dim1D>;
/// Slice of a 2D grid represented as a quadtree.
pub type NdTreeSlice2D<'cache> = NdTreeSlice<'cache, Dim2D>;
/// Slice of a 3D grid represented as an octree.
pub type NdTreeSlice3D<'cache> = NdTreeSlice<'cache, Dim3D>;
/// Slice of a 4D grid represented as a tree with branching factor 16.
pub type NdTreeSlice4D<'cache> = NdTreeSlice<'cache, Dim4D>;
/// Slice of a 5D grid represented as a tree with branching factor 32.
pub type NdTreeSlice5D<'cache> = NdTreeSlice<'cache, Dim5D>;
/// Slice of a 6D grid represented as a tree with branching factor 64.
pub type NdTreeSlice6D<'cache> = NdTreeSlice<'cache, Dim6D>;
