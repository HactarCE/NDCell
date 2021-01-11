use itertools::Itertools;
use std::fmt;

use super::{Layer, NodeRefTrait, NodeRefWithGuard};
use crate::axis::{X, Y};
use crate::dim::*;
use crate::ndrect::{BigRect, CanContain};
use crate::ndvec::{BigVec, NdVec};
use crate::num::ToPrimitive;

/// Immutable view into an ND-tree.
///
/// Note that like `NdTree`, an `NdTreeSlice` cannot be smaller than a base node.
#[derive(Debug, Eq, PartialEq)]
pub struct NdTreeSlice<'pool, D: Dim> {
    /// Root node.
    pub root: NodeRefWithGuard<'pool, D>,
    /// Position of the lower bound of the root node.
    pub base_pos: BigVec<D>,
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

impl<'pool, D: Dim> NdTreeSlice<'pool, D> {
    /// Returns a rectangle encompassing the slice.
    #[inline]
    pub fn rect(&self) -> BigRect<D> {
        self.root.big_rect() + &self.base_pos
    }
    /// Returns the minimum position in the slice's node.
    #[inline]
    pub fn min(&self) -> BigVec<D> {
        self.base_pos.clone()
    }
    /// Returns the maximum position in the slice's node.
    #[inline]
    pub fn max(&self) -> BigVec<D> {
        self.rect().max()
    }
    /// Returns the vector size of the slice's node.
    #[inline]
    pub fn size(&self) -> BigVec<D> {
        self.rect().size()
    }

    /// Returns the cell value at the given position, if it is within the bounds
    /// of the slice.
    pub fn get_cell(&self, pos: &BigVec<D>) -> Option<u8> {
        if self.rect().contains(pos) {
            Some(self.root.cell_at_pos(&(pos - &self.base_pos)))
        } else {
            None
        }
    }

    /// Subdivides the slice into 2^NDIM slices half the size. If the slice
    /// contains only one cell, returns `Err()` containing that cell and its
    /// position.
    pub fn subdivide<'a: 'pool>(&'a self) -> Result<Vec<NdTreeSlice<'a, D>>, (u8, &'a BigVec<D>)> {
        match self.root.as_ref().subdivide() {
            Ok(children) => Ok(children
                .into_iter()
                .enumerate()
                .map(|(i, child)| Self {
                    root: NodeRefWithGuard::from(child),
                    base_pos: self.root.layer().big_child_offset(i) + &self.base_pos,
                })
                .collect_vec()),
            Err(cell_state) => Err((cell_state, &self.base_pos)),
        }
    }
}
