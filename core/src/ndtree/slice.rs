use std::fmt;

use super::{node_math, Node, NodeRef};
use crate::axis::Axis::{X, Y};
use crate::dim::*;
use crate::ndrect::{BigRect, CanContain};
use crate::ndvec::{BigVec, NdVec};
use crate::num::ToPrimitive;

/// Immutable view into an NdTree.
#[derive(Debug, Clone)]
// TODO: note that a slice can be 1x1 while a tree cannot
pub struct NdTreeSlice<'cache, D: Dim> {
    /// Root NdTreeNode of this slice.
    pub root: NodeRef<'cache, D>,
    /// Position of the lower bound of the root node.
    pub offset: BigVec<D>,
}
// TODO: derive these instead
impl<D: Dim> PartialEq for NdTreeSlice<'_, D> {
    fn eq(&self, other: &Self) -> bool {
        self.root == other.root && self.offset == other.offset
    }
}
impl<D: Dim> Eq for NdTreeSlice<'_, D> {}

/// A 1D grid represented as a bintree.
pub type NdTreeSlice1D<'cache> = NdTreeSlice<'cache, Dim1D>;
/// A 2D grid represented as a quadtree.
pub type NdTreeSlice2D<'cache> = NdTreeSlice<'cache, Dim2D>;
/// A 3D grid represented as an octree.
pub type NdTreeSlice3D<'cache> = NdTreeSlice<'cache, Dim3D>;
/// A 4D grid represented as a tree with nodes of degree 16.
pub type NdTreeSlice4D<'cache> = NdTreeSlice<'cache, Dim4D>;
/// A 5D grid represented as a tree with nodes of degree 32.
pub type NdTreeSlice5D<'cache> = NdTreeSlice<'cache, Dim5D>;
/// A 6D grid represented as a tree with nodes of degree 64.
pub type NdTreeSlice6D<'cache> = NdTreeSlice<'cache, Dim6D>;

impl fmt::Display for NdTreeSlice<'_, Dim2D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.root.layer() > 8 {
            panic!("Cannot display node larger than 256x256");
        }
        let rect = self.rect();
        let mut line = String::with_capacity(self.root.big_len().to_usize().unwrap() * 2);
        for y in rect.axis_range(Y).rev() {
            line.clear();
            for x in rect.axis_range(X) {
                let state = self.get_cell(&NdVec([x, y.clone()])).unwrap();
                line.push(".#23456789".chars().nth(state as usize).unwrap_or('?'));
                line.push(' ');
            }
            line.pop();
            writeln!(f, "{}", line)?;
        }
        Ok(())
    }
}

impl<'cache, D: Dim> NdTreeSlice<'cache, D> {
    pub fn with_offset(root: NodeRef<'cache, D>, offset: BigVec<D>) -> Self {
        todo!("document");
        Self { root, offset }
    }

    /// Returns the NdRect bounding this slice.
    pub fn rect(&self) -> BigRect<D> {
        self.root.big_rect() + &self.offset
    }
    /// Returns the minimum position in this NdTree.
    pub fn min(&self) -> BigVec<D> {
        self.rect().min()
    }
    /// Returns the maximum position in this NdTree.
    pub fn max(&self) -> BigVec<D> {
        self.rect().max()
    }
    /// Returns the vector size of this NdTree.
    pub fn size(&self) -> BigVec<D> {
        self.rect().size()
    }

    /// Returns the cell value at the given position, if it is within the bounds
    /// of the slice.
    pub fn get_cell(&self, pos: &BigVec<D>) -> Option<u8> {
        if self.rect().contains(pos) {
            Some(self.root.cell_at_pos(&(pos - &self.offset)))
        } else {
            None
        }
    }

    pub fn subdivide(&self) -> Result<Vec<NdTreeSlice<D>>, (u8, &BigVec<D>)> {
        Ok(self
            .root
            .subdivide()
            .map_err(|cell| (cell, &self.offset))?
            .into_iter()
            .enumerate()
            .map(move |(child_index, subcube)| {
                let child_offset = node_math::leaf_node_cell_index_to_pos::<D>(1, child_index)
                    .to_bigvec()
                    << subcube.layer();
                Self::with_offset(subcube, child_offset + &self.offset)
            })
            .collect())
    }
}
