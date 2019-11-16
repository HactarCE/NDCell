use crate::automaton::space::{Dim, NdVec};

impl<D: Dim> NdVec<D> {
    pub(super) const BRANCHES: usize = 1 << D::NDIM;
    pub(super) fn branch_index_top(&self) -> usize {
        let mut ret = 0;
        for ax in D::axes() {
            ret <<= 1;
            if self[ax] >= 0 {
                ret += 1;
            }
        }
        ret
    }
    pub(super) fn branch_index(&self, layer: usize) -> usize {
        let mut ret = 0;
        for ax in D::axes() {
            ret <<= 1;
            ret |= (self[ax] as usize >> layer) & 1;
        }
        ret
    }
}
