//! Projections that turn an NdTree into one with fewer dimensions.

use std::convert::TryInto;

mod simple;
mod slice2d;
mod slice3d;

use super::*;
pub use simple::SimpleProjection;
pub use slice2d::SliceProjection2D;
pub use slice3d::SliceProjection3D;

pub struct NdProjection<C: CellType, D: Dim, P: Dim>(pub Box<dyn NdProjector<C, D, P>>);
impl<C: CellType, D: Dim, P: Dim> Clone for NdProjection<C, D, P> {
    fn clone(&self) -> Self {
        let params = self.0.get_params();
        Self(params.try_into().expect("Failed to clone projection"))
    }
}
impl<C: CellType, D: Dim> Default for NdProjection<C, D, D> {
    fn default() -> Self {
        Self(Box::new(SimpleProjection))
    }
}
impl<C: CellType, D: Dim, P: Dim> NdProjection<C, D, P> {
    pub fn project(&self, tree: &NdTree<C, D>) -> NdTree<C, P> {
        self.0.project(tree)
    }
    pub fn get_params(&self) -> ProjectionParams {
        self.0.get_params()
    }
}

pub trait NdProjector<C: CellType, D: Dim, P: Dim> {
    fn project(&self, tree: &NdTree<C, D>) -> NdTree<C, P>;
    fn overwrite_projected(&self, destination: &mut NdTree<C, D>, source: &NdTree<C, P>);
    fn get_params(&self) -> ProjectionParams;
}

#[derive(Debug, Clone)]
pub enum ProjectionParams {
    Simple,
    Slice(NdVecEnum, AxesSelectEnum),
}

impl<'a, C: CellType, D: Dim, P: Dim> TryInto<Box<dyn NdProjector<C, D, P>>> for ProjectionParams {
    type Error = NdProjectionError;
    fn try_into(self) -> Result<Box<dyn NdProjector<C, D, P>>, Self::Error> {
        // This method is a little Sketchy™; there's a lot of pointer (Box)
        // casting using std::mem::transmute(), using my own runtime value
        // checking instead of Rust's compile-time type checking. Although all
        // of the "runtime" checks should be compile-time-optiimzed away.
        match self {
            ProjectionParams::Simple => {
                // Check that D = P.
                if D::NDIM == P::NDIM {
                    let ret: Box<dyn NdProjector<C, D, D>> = Box::new(SimpleProjection);
                    Ok(unsafe { std::mem::transmute(ret) })
                } else {
                    Err(NdProjectionError::WrongProjectedDim)
                }
            }

            ProjectionParams::Slice(slice_pos, axes) => {
                // Check D.
                let slice_pos: NdVec<D> = slice_pos
                    .try_into()
                    .map_err(|_| NdProjectionError::WrongNdTreeDim)?;
                match axes {
                    AxesSelectEnum::Axes2D { h, v } => {
                        // Check P.
                        if P::NDIM == 2 {
                            let ret = SliceProjection2D::new(slice_pos, h, v);
                            let ret: Box<dyn NdProjector<C, D, Dim2D>> = Box::new(ret);
                            Ok(unsafe { std::mem::transmute(ret) })
                        } else {
                            Err(NdProjectionError::WrongProjectedDim)
                        }
                    }
                    AxesSelectEnum::Axes3D { .. } => unimplemented!(),
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum NdProjectionError {
    WrongCellType,
    WrongNdTreeDim,
    WrongProjectedDim,
}

#[derive(Debug, Clone)]
pub enum NdVecEnum {
    Vec1D(Vec1D),
    Vec2D(Vec2D),
    Vec3D(Vec3D),
    Vec4D(Vec4D),
    Vec5D(Vec5D),
    Vec6D(Vec6D),
}
impl NdVecEnum {
    fn get_ndim(&self) -> usize {
        match self {
            Self::Vec1D(_) => 1,
            Self::Vec2D(_) => 2,
            Self::Vec3D(_) => 3,
            Self::Vec4D(_) => 4,
            Self::Vec5D(_) => 5,
            Self::Vec6D(_) => 6,
        }
    }
}

/// Converts an NdVec between arbitrary dimensionalities, as long as those
/// dimensionalities are the same. Only really useful when passing generic type
/// parameters.
///
/// This function is not marked as unsafe because it performs "runtime" checking
/// that the initial and final dimensionalities are the same, even though that
/// "runtime" checking is almost certianly compile-time-optimized away.
///
/// If the dimensionalities do not match, panics.
fn transmute_ndvec<D1: Dim, D2: Dim>(ndvec: NdVec<D1>) -> NdVec<D2> {
    if D1::NDIM == D2::NDIM {
        unsafe { *std::mem::transmute::<Box<NdVec<D1>>, Box<NdVec<D2>>>(Box::new(ndvec)) }
    } else {
        panic!(
            "Cannot convert NdVec<Dim{}D> into NdVec<Dim{}D>",
            D1::NDIM,
            D2::NDIM
        )
    }
}

impl<'a, D: Dim> From<NdVec<D>> for NdVecEnum {
    fn from(inner: NdVec<D>) -> Self {
        match D::NDIM {
            1 => Self::Vec1D(transmute_ndvec(inner)),
            2 => Self::Vec2D(transmute_ndvec(inner)),
            3 => Self::Vec3D(transmute_ndvec(inner)),
            4 => Self::Vec4D(transmute_ndvec(inner)),
            5 => Self::Vec5D(transmute_ndvec(inner)),
            6 => Self::Vec6D(transmute_ndvec(inner)),
            _ => unreachable!("Dimensions above 6 are not supported"),
        }
    }
}
impl<'a, D: Dim> TryInto<NdVec<D>> for NdVecEnum {
    type Error = ();
    fn try_into(self) -> Result<NdVec<D>, ()> {
        if self.get_ndim() == D::NDIM {
            Ok(match self {
                Self::Vec1D(inner) => transmute_ndvec(inner),
                Self::Vec2D(inner) => transmute_ndvec(inner),
                Self::Vec3D(inner) => transmute_ndvec(inner),
                Self::Vec4D(inner) => transmute_ndvec(inner),
                Self::Vec5D(inner) => transmute_ndvec(inner),
                Self::Vec6D(inner) => transmute_ndvec(inner),
            })
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum AxesSelectEnum {
    Axes2D { h: Axis, v: Axis },
    Axes3D { h: Axis, v: Axis, n: Axis },
}
