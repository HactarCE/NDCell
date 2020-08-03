// TODO: merge this with normal NdVecs and generalize to any number of dimensions

use super::*;

/// 2D vector consisting of a bigint component and a floating-point component.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FracVec2D {
    /// Integral component.
    pub int: BigVec2D,
    /// Fractional component, in the range [0, 1).
    pub frac: FVec2D,
}
impl FracVec2D {
    /// Mutate this vector to ensure that the fractional component is within [0,
    /// 1) without changing the value of the vector.
    pub fn normalize(&mut self) {
        let int_delta = self.frac.floor();
        self.frac -= int_delta;
        self.int += int_delta.as_ivec();
    }
    /// Returns a new vector with the fractional component within [0, 1) without
    /// changing the value of the vector.
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn normalized(mut self) -> Self {
        self.normalize();
        self
    }
}
impl From<FVec2D> for FracVec2D {
    fn from(frac: FVec2D) -> Self {
        let int = BigVec2D::default();
        Self { int, frac }.normalized()
    }
}
impl From<BigVec2D> for FracVec2D {
    fn from(int: BigVec2D) -> Self {
        let frac = FVec2D::default();
        Self { int, frac }
    }
}

impl std::ops::Add<&FracVec2D> for FracVec2D {
    type Output = Self;
    fn add(mut self, rhs: &Self) -> Self::Output {
        self += rhs;
        self
    }
}
impl std::ops::Sub<&FracVec2D> for FracVec2D {
    type Output = Self;
    fn sub(mut self, rhs: &Self) -> Self::Output {
        self -= rhs;
        self
    }
}

impl std::ops::AddAssign<&FracVec2D> for FracVec2D {
    fn add_assign(&mut self, rhs: &FracVec2D) {
        self.int += &rhs.int;
        self.frac += rhs.frac;
        self.normalize();
    }
}
impl std::ops::SubAssign<&FracVec2D> for FracVec2D {
    fn sub_assign(&mut self, rhs: &FracVec2D) {
        self.int -= &rhs.int;
        self.frac -= rhs.frac;
        self.normalize();
    }
}

impl AsFVec<Dim2D> for FracVec2D {
    fn as_fvec(&self) -> FVec<Dim2D> {
        self.int.as_fvec() + self.frac
    }
}
