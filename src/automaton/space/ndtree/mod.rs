use seahash::SeaHasher;
use std::hash::Hasher;
use std::marker::PhantomData;
use std::rc::Rc;

mod cache;
mod index;
mod subtree;

use super::*;
use cache::NdTreeCache;
use subtree::*;

/// An N-dimensional generalization of a quadtree.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NdTree<T: CellType, D: Dim> {
    root: NdSubTree<T, D>,
    offset: NdVec<D>,
}

/// A 1D grid represented as a bintree.
pub type NdTree1D<T> = NdTree<T, Vec1D>;
/// A 2D grid represented as a quadtree.
pub type NdTree2D<T> = NdTree<T, Vec2D>;
/// A 3D grid represented as an octree.
pub type NdTree3D<T> = NdTree<T, Vec3D>;
/// A 4D grid represented as a tree with nodes of degree 16.
pub type NdTree4D<T> = NdTree<T, Vec4D>;
/// A 5D grid represented as a tree with nodes of degree 32.
pub type NdTree5D<T> = NdTree<T, Vec5D>;
/// A 6D grid represented as a tree with nodes of degree 64.
pub type NdTree6D<T> = NdTree<T, Vec6D>;
