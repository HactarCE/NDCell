use std::fmt;

use super::{CachedNodeRefTrait, Layer, LeafNodeRef, NodeRef, NodeRefEnum, NodeRefTrait};
use crate::axis::Axis::{X, Y};
use crate::dim::*;
use crate::ndrect::{BigRect, CanContain};
use crate::ndvec::{BigVec, NdVec};
use crate::num::ToPrimitive;

/// Immutable view into an NdTree.
///
/// Note that like an `NdTree`, `NdTreeSlice` cannot be smaller than a base node.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NdTreeSlice<'cache, D: Dim> {
    /// Root node.
    pub root: NodeRef<'cache, D>,
    /// Position of the lower bound of the root node.
    pub offset: BigVec<D>,
}

impl fmt::Display for NdTreeSlice<'_, Dim2D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.root.layer() > Layer(8) {
            writeln!(f, "Node larger than 256x256")?;
            return Ok(());
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
    /// Returns the NdRect bounding the slice.
    #[inline]
    pub fn rect(&self) -> BigRect<D> {
        self.root.big_rect() + &self.offset
    }
    /// Returns the minimum position in the slice.
    #[inline]
    pub fn min(&self) -> BigVec<D> {
        self.rect().min()
    }
    /// Returns the maximum position in the slice.
    #[inline]
    pub fn max(&self) -> BigVec<D> {
        self.rect().max()
    }
    /// Returns the vector size of the slice.
    #[inline]
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

    /// Subdivides the slice into 2^NDIM slices half the size. If the slice
    /// contains only one cell, returns `Err()` containing that cell and its
    /// position.
    pub fn subdivide<'a: 'cache>(
        &'a self,
    ) -> Result<Vec<NdTreeSlice<'a, D>>, (LeafNodeRef<'a, D>, &'a BigVec<D>)> {
        match self.root.as_enum() {
            NodeRefEnum::Leaf(n) => Err((n, &self.offset)),
            NodeRefEnum::NonLeaf(n) => {
                Ok(n.children()
                    .enumerate()
                    .map(move |(child_index, subcube)| {
                        // TODO: consider adding child_offset() method on Layer
                        let child_offset =
                            Layer(1).leaf_pos(child_index).to_bigvec() << subcube.layer().to_u32();
                        Self {
                            root: subcube.into(),
                            offset: child_offset + &self.offset,
                        }
                    })
                    .collect())
            }
        }
    }
}
