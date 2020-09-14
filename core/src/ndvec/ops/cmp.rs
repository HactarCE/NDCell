use std::cmp::Ordering;

use super::*;

impl<D: DimFor<N>, N: NdVecNum + PartialOrd> PartialOrd for NdVec<D, N> {
    /// Compares the corresponding elements of two vectors and returns `Some`
    /// `Ordering` if all axes have the same `Ordering`. Returns `None` if any
    /// axis differs or do not have an ordering.
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        D::Dim::axes()
            .iter()
            .map(|&ax| self[ax].partial_cmp(&other[ax]))
            .dedup()
            .exactly_one()
            .unwrap_or(None)
    }
}
