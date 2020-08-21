//! Projections for `NdTree`s.

// TODO: consider renaming module to 'project' and including ProjectedAutomaton

use std::convert::TryInto;

mod simple;
mod slice2d;
mod slice3d;

use super::*;
pub use simple::SimpleProjection;
pub use slice2d::SliceProjection2D;
pub use slice3d::SliceProjection3D;

/// A container for any type of NdProjector.
pub struct NdProjection<D: Dim, P: Dim>(pub Box<dyn NdProjector<D, P>>);
impl<D: Dim, P: Dim> Clone for NdProjection<D, P> {
    fn clone(&self) -> Self {
        let params = self.0.params();
        Self(params.try_into().expect("Failed to clone projection"))
    }
}
impl<D: Dim> Default for NdProjection<D, D> {
    fn default() -> Self {
        Self(Box::new(SimpleProjection))
    }
}
impl<D: Dim, P: Dim> NdProjector<D, P> for NdProjection<D, P> {
    fn project_tree(&self, tree: &NdTree<D>) -> NdTree<P> {
        self.0.project_tree(tree)
    }
    fn unproject_pos(&self, pos: &BigVec<P>) -> BigVec<D> {
        self.0.unproject_pos(pos)
    }
    fn overwrite_projected(&self, destination: &mut NdTree<D>, source: &NdTree<P>) {
        self.0.overwrite_projected(destination, source);
    }
    fn params(&self) -> ProjectionParams {
        self.0.params()
    }
}

/// A method for extracting or constructing a P-dimensional slice from a
/// D-dimensional automaton.
pub trait NdProjector<D: Dim, P: Dim>: Send {
    /// Projects a D-dimensional NdTree into a P-dimensional NdTree.
    fn project_tree(&self, tree: &NdTree<D>) -> NdTree<P>;
    /// Unprojects a P-dimensional point back into D-dimensional space.
    fn unproject_pos(&self, pos: &BigVec<P>) -> BigVec<D>;
    /// Modifies part of a projected NdTree.
    fn overwrite_projected(&self, destination: &mut NdTree<D>, source: &NdTree<P>);
    /// Returns the ProjectionParams that describe this projection.
    fn params(&self) -> ProjectionParams;
}

/// A set of parameters that fully describes an NdProjection.
///
/// This provides a way to retrieve an NdProjector's parameters while still
/// storing it as a trait object.
#[derive(Debug, Clone)]
pub enum ProjectionParams {
    /// A SimpleProjection.
    Simple,
    /// A SliceProjection2D.
    Slice2D(BigVecEnum, (Axis, Axis)),
    /// A SliceProjection3D.
    Slice3D(BigVecEnum, (Axis, Axis, Axis)),
}
impl<'a, D: Dim, P: Dim> TryInto<Box<dyn NdProjector<D, P>>> for ProjectionParams {
    type Error = NdProjectionError;
    fn try_into(self) -> Result<Box<dyn NdProjector<D, P>>, Self::Error> {
        // This method is a little Sketchy™; there's a lot of pointer (Box)
        // casting using std::mem::transmute(), using my own runtime value
        // checking instead of Rust's compile-time type checking. Although all
        // of the "runtime" checks should be compile-time-optimized away.
        match self {
            ProjectionParams::Simple => {
                // Check that D = P.
                if D::NDIM == P::NDIM {
                    let ret: Box<dyn NdProjector<D, D>> = Box::new(SimpleProjection);
                    Ok(unsafe { std::mem::transmute(ret) })
                } else {
                    Err(NdProjectionError::WrongProjectedDim)
                }
            }
            ProjectionParams::Slice2D(slice_pos, (h, v)) => {
                // Check D.
                let slice_pos: BigVec<D> = slice_pos
                    .try_into()
                    .map_err(|_| NdProjectionError::WrongNdTreeDim)?;
                // Check P.
                if P::NDIM == 2 {
                    let ret = SliceProjection2D::new(slice_pos, h, v);
                    let ret: Box<dyn NdProjector<D, Dim2D>> = Box::new(ret);
                    Ok(unsafe { std::mem::transmute(ret) })
                } else {
                    Err(NdProjectionError::WrongProjectedDim)?
                }
            }
            ProjectionParams::Slice3D(_slice_pos, (_h, _v, _n)) => unimplemented!(),
        }
    }
}

/// An error preventing conversion from ProjectionParams to NdProjector.
#[allow(missing_docs)]
#[derive(Debug, Copy, Clone)]
pub enum NdProjectionError {
    WrongCellType,
    WrongNdTreeDim,
    WrongProjectedDim,
}

/// A BigVec of an unknown dimensionality, for use in ProjectionParams.
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum BigVecEnum {
    Vec1D(BigVec1D),
    Vec2D(BigVec2D),
    Vec3D(BigVec3D),
    Vec4D(BigVec4D),
    Vec5D(BigVec5D),
    Vec6D(BigVec6D),
}
impl BigVecEnum {
    /// Returns the number of dimensions of the BigVec.
    fn ndim(&self) -> usize {
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

impl<'a, D: Dim> From<BigVec<D>> for BigVecEnum {
    fn from(inner: BigVec<D>) -> Self {
        match_ndim!(match D {
            1 => Self::Vec1D(NdVec::transmute(inner)),
            2 => Self::Vec2D(NdVec::transmute(inner)),
            3 => Self::Vec3D(NdVec::transmute(inner)),
            4 => Self::Vec4D(NdVec::transmute(inner)),
            5 => Self::Vec5D(NdVec::transmute(inner)),
            6 => Self::Vec6D(NdVec::transmute(inner)),
        })
    }
}
impl<'a, D: Dim> TryInto<BigVec<D>> for BigVecEnum {
    type Error = ();
    fn try_into(self) -> Result<BigVec<D>, ()> {
        if self.ndim() == D::NDIM {
            Ok(match self {
                Self::Vec1D(inner) => NdVec::transmute(inner),
                Self::Vec2D(inner) => NdVec::transmute(inner),
                Self::Vec3D(inner) => NdVec::transmute(inner),
                Self::Vec4D(inner) => NdVec::transmute(inner),
                Self::Vec5D(inner) => NdVec::transmute(inner),
                Self::Vec6D(inner) => NdVec::transmute(inner),
            })
        } else {
            Err(())
        }
    }
}
