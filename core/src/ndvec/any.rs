//! Enumeration of `NdVec`s with different dimensionality.

use super::*;

/// Vector with any dimensionality and coordinates of type `N`.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum AnyDimVec<N: NdVecNum> {
    Vec1D(Vec1D<N>),
    Vec2D(Vec2D<N>),
    Vec3D(Vec3D<N>),
    Vec4D(Vec4D<N>),
    Vec5D(Vec5D<N>),
    Vec6D(Vec6D<N>),
}

impl<D: DimFor<N>, N: NdVecNum> From<NdVec<D, N>> for AnyDimVec<N> {
    #[inline]
    fn from(ndvec: NdVec<D, N>) -> Self {
        match_ndim!(match D::Dim {
            1 => AnyDimVec::Vec1D(NdVec::transmute(ndvec)),
            2 => AnyDimVec::Vec2D(NdVec::transmute(ndvec)),
            3 => AnyDimVec::Vec3D(NdVec::transmute(ndvec)),
            4 => AnyDimVec::Vec4D(NdVec::transmute(ndvec)),
            5 => AnyDimVec::Vec5D(NdVec::transmute(ndvec)),
            6 => AnyDimVec::Vec6D(NdVec::transmute(ndvec)),
        })
    }
}
