//! Type aliases for `NdTree`s and related types.

use super::{FlatNdTree, NdTree, NdTreeSlice, Region};
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
pub type NdTreeSlice1D<'pool> = NdTreeSlice<'pool, Dim1D>;
/// Slice of a 2D grid represented as a quadtree.
pub type NdTreeSlice2D<'pool> = NdTreeSlice<'pool, Dim2D>;
/// Slice of a 3D grid represented as an octree.
pub type NdTreeSlice3D<'pool> = NdTreeSlice<'pool, Dim3D>;
/// Slice of a 4D grid represented as a tree with branching factor 16.
pub type NdTreeSlice4D<'pool> = NdTreeSlice<'pool, Dim4D>;
/// Slice of a 5D grid represented as a tree with branching factor 32.
pub type NdTreeSlice5D<'pool> = NdTreeSlice<'pool, Dim5D>;
/// Slice of a 6D grid represented as a tree with branching factor 64.
pub type NdTreeSlice6D<'pool> = NdTreeSlice<'pool, Dim6D>;

/// Flattened representation of a bintree.
pub type FlatNdTree1D<T> = FlatNdTree<Dim1D, T>;
/// Flattened representation of a quadtree.
pub type FlatNdTree2D<T> = FlatNdTree<Dim2D, T>;
/// Flattened representation of a  octree.
pub type FlatNdTree3D<T> = FlatNdTree<Dim3D, T>;
/// Flattened representation of a tree with branching factor 16.
pub type FlatNdTree4D<T> = FlatNdTree<Dim4D, T>;
/// Flattened representation of a tree with branching factor 32.
pub type FlatNdTree5D<T> = FlatNdTree<Dim5D, T>;
/// Flattened representation of a tree with branching factor 64.
pub type FlatNdTree6D<T> = FlatNdTree<Dim6D, T>;

/// Finite set of 1D cell positions.
pub type Region1D = Region<Dim1D>;
/// Finite set of 2D cell positions.
pub type Region2D = Region<Dim2D>;
/// Finite set of 3D cell positions.
pub type Region3D = Region<Dim3D>;
/// Finite set of 4D cell positions.
pub type Region4D = Region<Dim4D>;
/// Finite set of 5D cell positions.
pub type Region5D = Region<Dim5D>;
/// Finite set of 6D cell positions.
pub type Region6D = Region<Dim6D>;
