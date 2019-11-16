use std::cmp;

use super::*;

/// An N-dimensional hyperrectangle.
#[derive(Debug, Copy, Clone)]
pub struct NdRect<D: Dim> {
    /// One corner of the hyperrectangle.
    pub a: NdVec<D>,
    /// The other corner of the hyperrectangle.
    pub b: NdVec<D>,
}

impl<D: Dim> NdRect<D> {
    /// Returns the minimum (most negative) corner of this NdRect.
    pub fn min(&self) -> NdVec<D> {
        let mut ret = NdVec::origin();
        for ax in D::axes() {
            ret[ax] = cmp::min(self.a[ax], self.b[ax]);
        }
        ret
    }
    /// Returns the maximum (most positive) corner of this NdRect.
    pub fn max(&self) -> NdVec<D> {
        let mut ret = NdVec::origin();
        for ax in D::axes() {
            ret[ax] = cmp::max(self.a[ax], self.b[ax]);
        }
        ret
    }
    /// Returns true if the cell position is contained within this
    /// hyperrectangle.
    pub fn contains(&self, pos: NdVec<D>) -> bool {
        let min = self.min();
        let max = self.max();
        for ax in D::axes() {
            if pos[ax] < min[ax] || pos[ax] > max[ax] {
                return false;
            }
        }
        true
    }
    /// Constructs and equivalent NdRect with its corners swapped.
    pub fn swap(&self) -> Self {
        Self {
            a: self.b,
            b: self.a,
        }
    }
    /// Constructs an equivalent NdRect using the minimum and maximum corners.
    pub fn order(&self) -> Self {
        Self {
            a: self.min(),
            b: self.max(),
        }
    }
    /// Returns an iterator over all the positions in this hyperrectangle.
    pub fn iter(self) -> NdRectIter<D> {
        NdRectIter {
            rect: self.order(),
            next: self.a,
            done: false,
        }
    }
}

/// An iterator over a hyperrectangle of cell positions.
#[derive(Debug, Copy, Clone)]
pub struct NdRectIter<D: Dim> {
    rect: NdRect<D>,
    next: NdVec<D>,
    done: bool,
}

impl<D: Dim> Iterator for NdRectIter<D> {
    type Item = NdVec<D>;
    fn next(&mut self) -> Option<NdVec<D>> {
        if self.done {
            None
        } else {
            let ret = self.next;
            for ax in D::axes() {
                self.next[ax] += 1;
                if self.next[ax] > self.rect.b[ax] {
                    self.next[ax] = self.rect.a[ax];
                } else {
                    return Some(ret);
                }
            }
            self.done = true;
            None
        }
    }
}
