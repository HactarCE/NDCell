//! Iterator over `NdVec` positions in an integer `NdRect`.

use crate::dim::Dim;
use crate::num::{Integer, ToPrimitive};

use super::*;

impl<D: DimFor<N>, N: NdVecNum + Integer + ToPrimitive> IntoIterator for &NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    type Item = NdVec<D, N>;
    type IntoIter = Iter<D, N>;

    /// Returns an iterator over all the integer positions in the rectangle.
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.into()
    }
}

/// Iterator over positions in an integer hyperrectangle.
#[derive(Debug, Clone)]
pub struct Iter<D: DimFor<N>, N: NdVecNum> {
    start: NdVec<D, N>,
    end: NdVec<D, N>,
    next: Option<NdVec<D, N>>,
}
impl<D: DimFor<N>, N: NdVecNum> From<&NdRect<D, N>> for Iter<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    #[inline]
    fn from(rect: &NdRect<D, N>) -> Self {
        let start = rect.min();
        let end = rect.max();
        let next = Some(start.clone());
        Self { start, end, next }
    }
}
impl<D: DimFor<N>, N: NdVecNum + Integer + ToPrimitive> Iterator for Iter<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    type Item = NdVec<D, N>;

    #[inline]
    fn next(&mut self) -> Option<NdVec<D, N>> {
        let ret = self.next.clone();
        if let Some(ref mut next) = &mut self.next {
            for &ax in D::Dim::axes() {
                // Increment this axis.
                next[ax] += N::one();
                // If this axis overflows ...
                if next[ax] > self.end[ax] {
                    // ... then reset it and increment the next axis.
                    next[ax] = self.start[ax].clone();
                } else {
                    // If not, then we're done. We advanced along this axis with no
                    // overflow.
                    return ret;
                }
            }
            // If we got this far, then we overflowed on every axis. Return `None`
            // forever.
            self.next = None;
        }
        return ret;
    }
}
impl<D: DimFor<N>, N: NdVecNum + Integer> Iter<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    /// Returns the first position returned by the iterator.
    #[inline]
    pub fn start(&self) -> &NdVec<D, N> {
        &self.start
    }

    /// Returns the last position returned by the iterator.
    #[inline]
    pub fn end(&self) -> &NdVec<D, N> {
        &self.end
    }

    /// Moves the iterator to the given position, which will be returned from
    /// the next call to `next()`.
    #[inline]
    pub fn go_to(&mut self, pos: NdVec<D, N>) {
        self.next = Some(pos);
    }
}
