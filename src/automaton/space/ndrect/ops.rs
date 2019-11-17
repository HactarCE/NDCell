use std::ops::*;

use super::*;

impl<D: Dim> Add<NdVec<D>> for NdRect<D> {
    type Output = Self;
    fn add(self, offset: NdVec<D>) -> Self {
        Self {
            min: self.min + offset,
            max: self.max + offset,
        }
    }
}

impl<D: Dim> Sub<NdVec<D>> for NdRect<D> {
    type Output = Self;
    fn sub(self, offset: NdVec<D>) -> Self {
        Self {
            min: self.min - offset,
            max: self.max - offset,
        }
    }
}
