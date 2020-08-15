use super::*;

impl<D: Dim> BigRect<D> {
    /// Converts this `BigRect` to an `IRect`, panicking if it does not fit.
    pub fn to_irect(&self) -> IRect<D> {
        IRect::new(self.start.to_ivec(), self.size.to_ivec())
    }
    /// Converts this `BigRect` to a `URect`, panicking if it does not fit.
    pub fn to_urect(&self) -> URect<D> {
        URect::new(self.start.to_uvec(), self.size.to_uvec())
    }
}

impl<D: Dim> IRect<D> {
    /// Converts this `IRect` to a `BigRect`.
    pub fn to_bigrect(&self) -> BigRect<D> {
        BigRect::new(self.start.to_bigvec(), self.size.to_bigvec())
    }
}
