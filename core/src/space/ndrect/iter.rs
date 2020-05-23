use num::Integer;

use super::*;

/// An iterator over a hyperrectangle of cell positions.
#[derive(Debug, Clone)]
pub struct NdRectIter<D: DimFor<N>, N: NdVecNum + Integer>
where
    NdVec<D, N>: NdRectVec,
{
    start: NdVec<D, N>,
    end: NdVec<D, N>,
    next: Option<NdVec<D, N>>,
}

impl<D: DimFor<N>, N: NdVecNum + Integer> From<&NdRect<D, N>> for NdRectIter<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    fn from(rect: &NdRect<D, N>) -> Self {
        let start = rect.min();
        let end = rect.max();
        let next = Some(start.clone());
        Self { start, end, next }
    }
}

impl<D: DimFor<N>, N: NdVecNum + Integer> Iterator for NdRectIter<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    type Item = NdVec<D, N>;
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
}
