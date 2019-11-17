use std::ops::Index;

use super::*;

#[derive(Debug, Clone)]
pub struct NdTreeSlice<T: CellType, D: Dim> {
    root: NdSubTree<T, D>,
    offset: NdVec<D>,
    rect: NdRect<D>,
}

impl<T: CellType, D: Dim> NdTreeSlice<T, D> {
    /// Constructs a new NdTreeSlice given a node and an offset from that node.
    pub fn new(node: NdSubTree<T, D>, offset: NdVec<D>) -> Self {
        let rect = NdRect::span(offset, offset + node.len() as isize - 1);
        Self::with_rect(node, offset, rect)
    }
    /// Constructs an NdTreeSlice that is restricted to a given hyperrectangle.
    pub fn with_rect(node: NdSubTree<T, D>, offset: NdVec<D>, rect: NdRect<D>) -> Self {
        // TODO check that rectangle bounds are valid according to node.len()
        // and offset.
        Self {
            root: node,
            offset,
            rect,
        }
    }

    /// Returns the root node of this slice.
    pub fn root(&self) -> &NdSubTree<T, D> {
        &self.root
    }
    /// Returns the offset of this slice.
    pub fn offset(&self) -> NdVec<D> {
        self.offset
    }
    /// Returns the NdRect bounding this slice.
    pub fn rect(&self) -> NdRect<D> {
        self.rect
    }

    /// Returns the cell at the given position, if it is within the bounds of the slice.
    pub fn get_cell(&self, mut pos: NdVec<D>) -> Option<T> {
        pos -= self.offset;
        if self.rect.contains(pos) {
            Some(self.root.get_cell(pos))
        } else {
            None
        }
    }
}
