//! Iterator over `NdVec` positions in an integer `NdRect`.

use crate::dim::Dim;
use crate::num::{Integer, ToPrimitive};

use super::*;

impl<D: DimFor<N>, N: NdVecNum + Integer + ToPrimitive> IntoIterator for &NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    type Item = NdVec<D, N>;
    type IntoIter = NdRectIter<D, N>;

    /// Returns an iterator over all the integer positions in the rectangle.
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.into()
    }
}

/// Iterator over positions in an integer hyperrectangle.
#[derive(Debug, Clone)]
pub struct NdRectIter<D: DimFor<N>, N: NdVecNum + Integer>
where
    NdVec<D, N>: NdRectVec,
{
    start: NdVec<D, N>,
    end: NdVec<D, N>,
    next: Option<NdVec<D, N>>,
    remaining: Option<usize>,
}

impl<D: DimFor<N>, N: NdVecNum + Integer + ToPrimitive> From<&NdRect<D, N>> for NdRectIter<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    #[inline]
    fn from(rect: &NdRect<D, N>) -> Self {
        let start = rect.min();
        let end = rect.max();
        let next = Some(start.clone());
        let remaining = rect.count().to_usize();
        Self {
            start,
            end,
            next,
            remaining,
        }
    }
}

impl<D: DimFor<N>, N: NdVecNum + Integer> Iterator for NdRectIter<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    type Item = NdVec<D, N>;

    #[inline]
    fn next(&mut self) -> Option<NdVec<D, N>> {
        let ret = self.next.clone();
        if let Some(ref mut next) = &mut self.next {
            for &ax in D::Dim::axes() {
                next[ax] += N::one();
                if next[ax] > self.end[ax] {
                    next[ax] = self.start[ax].clone();
                } else {
                    return ret;
                }
            }
            self.next = None;
        }
        return ret;
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining.unwrap_or(usize::MAX), self.remaining)
    }
}