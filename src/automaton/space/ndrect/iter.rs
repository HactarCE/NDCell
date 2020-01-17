use num::Integer;

use super::*;

/// An iterator over a hyperrectangle of cell positions.
#[derive(Debug, Clone)]
pub struct NdRectIter<D: DimFor<N>, N: NdVecNum + Integer>
where
    NdVec<D, N>: NdRectVec,
{
    rect: NdRect<D, N>,
    next: NdVec<D, N>,
    done: bool,
}

impl<D: DimFor<N>, N: NdVecNum + Integer> From<NdRect<D, N>> for NdRectIter<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    fn from(rect: NdRect<D, N>) -> Self {
        let next = rect.min();
        let done = false;
        Self { rect, next, done }
    }
}

impl<D: DimFor<N>, N: NdVecNum + Integer> Iterator for NdRectIter<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    type Item = NdVec<D, N>;
    fn next(&mut self) -> Option<NdVec<D, N>> {
        if self.done {
            None
        } else {
            let ret = self.next.clone();
            for &ax in D::Dim::axes() {
                self.next[ax] += N::one();
                if self.next[ax] > self.rect.max()[ax] {
                    self.next[ax] = self.rect.min[ax].clone();
                } else {
                    return Some(ret);
                }
            }
            self.done = true;
            Some(ret)
        }
    }
}
