//! Enumeration of `NdVec`s with different dimensionality.

use super::*;

/// Vector with any dimensionality and coordinates of type `N`.
#[allow(missing_docs)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum AnyDimVec<N: NdVecNum> {
    Vec1D(Vec1D<N>),
    Vec2D(Vec2D<N>),
    Vec3D(Vec3D<N>),
    Vec4D(Vec4D<N>),
    Vec5D(Vec5D<N>),
    Vec6D(Vec6D<N>),
}
impl<N: NdVecNum> AnyDimVec<N> {
    /// Returns the number of dimensions of this vector.
    pub fn ndim(&self) -> usize {
        match self {
            AnyDimVec::Vec1D(_) => 1,
            AnyDimVec::Vec2D(_) => 2,
            AnyDimVec::Vec3D(_) => 3,
            AnyDimVec::Vec4D(_) => 4,
            AnyDimVec::Vec5D(_) => 5,
            AnyDimVec::Vec6D(_) => 6,
        }
    }
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
impl<D: DimFor<N>, N: NdVecNum> TryFrom<AnyDimVec<N>> for NdVec<D, N> {
    type Error = AnyDimVec<N>;

    #[inline]
    fn try_from(any_dim_vec: AnyDimVec<N>) -> Result<Self, Self::Error> {
        if D::Dim::NDIM == any_dim_vec.ndim() {
            Ok(match any_dim_vec {
                AnyDimVec::Vec1D(ndvec) => NdVec::transmute(ndvec),
                AnyDimVec::Vec2D(ndvec) => NdVec::transmute(ndvec),
                AnyDimVec::Vec3D(ndvec) => NdVec::transmute(ndvec),
                AnyDimVec::Vec4D(ndvec) => NdVec::transmute(ndvec),
                AnyDimVec::Vec5D(ndvec) => NdVec::transmute(ndvec),
                AnyDimVec::Vec6D(ndvec) => NdVec::transmute(ndvec),
            })
        } else {
            Err(any_dim_vec)
        }
    }
}
