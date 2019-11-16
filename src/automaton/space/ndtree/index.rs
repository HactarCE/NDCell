use crate::automaton::space::{Dim, NdVec};

impl<D: Dim> NdVec<D> {
    /// The number of branches for this many dimensions (2^d).
    pub(super) const BRANCHES: usize = 1 << D::NDIM;
    /// The bitmask for branch indices.
    pub(super) const BRANCH_IDX_BITMASK: usize = Self::BRANCHES - 1;
    /// Computes the "branch index" for this position at the given layer.
    ///
    /// Each nth layer corresponds to the nth bit of each axis, which can either
    /// be 0 or 1. The "branch index" is a number between 0 and 2^d-1 composed
    /// from these bits; each bit in the branch index is taken from a different
    /// axis. It's like a bitwise NdVec.
    pub(super) fn branch_index(&self, layer: usize) -> usize {
        let mut ret = 0;
        for ax in D::axes() {
            ret <<= 1;
            ret |= (self[ax] as usize >> layer) & 1;
        }
        ret
    }
    /// Computes the "branch index" for the branch opposite this position at the given layer.
    pub(super) fn branch_index_opposite(&self, layer: usize) -> usize {
        self.branch_index(layer) ^ (Self::BRANCHES - 1)
    }
}
