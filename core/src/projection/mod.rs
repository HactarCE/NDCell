//! Projections for `NdTree`s.

// TODO: consider renaming module to 'project' and including ProjectedAutomaton

use parking_lot::RwLock;
use std::convert::TryInto;
use std::fmt;
use std::sync::Arc;

mod simple;
mod slice2d;
mod slice3d;

use crate::axis::Axis;
use crate::dim::{Dim, Dim2D, Dim3D};
use crate::ndtree::{NdTree, NodeCache};
use crate::ndvec::{AnyDimBigVec, BigVec, NdVec};
pub use simple::SimpleProjection;
pub use slice2d::SliceProjection2D;
pub use slice3d::SliceProjection3D;

/// A container for any type of NdProjector.
#[derive(Debug)]
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
    fn projected_cache<'a>(&'a self, tree: &'a NdTree<D>) -> &'a Arc<RwLock<NodeCache<P>>> {
        self.0.projected_cache(tree)
    }
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
pub trait NdProjector<D: Dim, P: Dim>: fmt::Debug + Send {
    /// Returns a reference to the node cache used for projected nodes.
    fn projected_cache<'a>(&'a self, tree: &'a NdTree<D>) -> &'a Arc<RwLock<NodeCache<P>>>;
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
    Slice2D(AnyDimBigVec, (Axis, Axis)),
    /// A SliceProjection3D.
    Slice3D(AnyDimBigVec, (Axis, Axis, Axis)),
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
    WrongNdTreeDim,
    WrongProjectedDim,
}
