use super::*;

/// An NdVec of unknown dimensionality.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq)]
pub enum AnyDimVec<N: NdVecNum> {
    Vec1D(Vec1D<N>),
    Vec2D(Vec2D<N>),
    Vec3D(Vec3D<N>),
    Vec4D(Vec4D<N>),
    Vec5D(Vec5D<N>),
    Vec6D(Vec6D<N>),
}

impl<D: DimFor<N>, N: NdVecNum> From<NdVec<D, N>> for AnyDimVec<N> {
    fn from(ndvec: NdVec<D, N>) -> Self {
        match D::Dim::NDIM {
            1 => AnyDimVec::Vec1D(unsafe {
                *std::mem::transmute::<Box<NdVec<D, N>>, Box<NdVec<Dim1D, N>>>(Box::new(ndvec))
            }),
            2 => AnyDimVec::Vec2D(unsafe {
                *std::mem::transmute::<Box<NdVec<D, N>>, Box<NdVec<Dim2D, N>>>(Box::new(ndvec))
            }),
            3 => AnyDimVec::Vec3D(unsafe {
                *std::mem::transmute::<Box<NdVec<D, N>>, Box<NdVec<Dim3D, N>>>(Box::new(ndvec))
            }),
            4 => AnyDimVec::Vec4D(unsafe {
                *std::mem::transmute::<Box<NdVec<D, N>>, Box<NdVec<Dim4D, N>>>(Box::new(ndvec))
            }),
            5 => AnyDimVec::Vec5D(unsafe {
                *std::mem::transmute::<Box<NdVec<D, N>>, Box<NdVec<Dim5D, N>>>(Box::new(ndvec))
            }),
            6 => AnyDimVec::Vec6D(unsafe {
                *std::mem::transmute::<Box<NdVec<D, N>>, Box<NdVec<Dim6D, N>>>(Box::new(ndvec))
            }),
            _ => unreachable!("Dimensions above 6 are not supported"),
        }
    }
}
