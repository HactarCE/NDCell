use std::cmp;

use super::*;

#[derive(Debug, Copy, Clone)]
pub struct NdRect<D: Dim> {
    pub a: NdVec<D>,
    pub b: NdVec<D>,
}

// #[derive(Debug, Copy, Clone)]
// pub struct NdRectIter<D: Dim> {
//     rect: NdRect<D>,
//     current: NdVec<D>,
// }

impl<D: Dim> NdRect<D> {
    pub fn min(&self) -> NdVec<D> {
        let mut ret = NdVec::origin();
        for ax in D::axes() {
            ret[ax] = cmp::min(self.a[ax], self.b[ax]);
        }
        ret
    }
    pub fn max(&self) -> NdVec<D> {
        let mut ret = NdVec::origin();
        for ax in D::axes() {
            ret[ax] = cmp::max(self.a[ax], self.b[ax]);
        }
        ret
    }
    pub fn flip(&self) -> Self {
        Self {
            a: self.b,
            b: self.a,
        }
    }
    pub fn order(&self) -> Self {
        Self {
            a: self.min(),
            b: self.max(),
        }
    }
    // pub fn iter(self) -> NdRectIter<D> {
    //     NdRectIter {
    //         rect: self,
    //         current: self.a,
    //     }
    // }
}
