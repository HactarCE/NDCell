use num::Num;

use super::*;

/// A vector of a given dimensionality; mostly used as a type argument to convey
/// the number of dimensions something has.
///
/// This is basically exactly the same as ndarray's Dimension trait, except it
/// uses any number type instead of usize (which is crucial for this
/// application). Similar to ndarray's Dimension trait, this trait should not
/// and cannot be implemented outside of this crate.
pub trait Dim<N: Num>: Debug + Clone + Default + Eq + Hash + private::Sealed {
    /// The number of dimensions (number of axes).
    const NDIM: usize;

    /// Returns a Vector of the axes of this many dimensions.
    fn axes() -> &'static [Axis] {
        ndim_axes(Self::NDIM)
    }

    /// Returns the coordinate along the given axis.
    fn get(&self, axis: Axis) -> &N;

    /// Returns a mutable reference to the coordinate along the given axis.
    fn get_mut(&mut self, axis: Axis) -> &mut N;

    /// Sets the coordinate along the given axis.
    fn set(&mut self, axis: Axis, value: N);

    /// Returns whether these coordinates consists entirely of zeros.
    fn is_zero(&self) -> bool {
        for &ax in Self::axes() {
            if self.get(ax) != N::zero() {
                return false;
            }
        }
        true
    }

    /// Returns the coordinates of the origin (i.e. all zeros).
    fn origin() -> Self;

    /// Returns true if the given axis belongs to this dimensionality.
    fn contains(axis: Axis) -> bool {
        (axis as usize) < Self::NDIM
    }
}

/// A basic 1D vector type.
pub type Dim1D<N> = [N; 1];
/// A basic 2D vector type.
pub type Dim2D<N> = [N; 2];
/// A basic 3D vector type.
pub type Dim3D<N> = [N; 3];
/// A basic 4D vector type.
pub type Dim4D<N> = [N; 4];
/// A basic 5D vector type.
pub type Dim5D<N> = [N; 5];
/// A basic 6D vector type.
pub type Dim6D<N> = [N; 6];

impl<N: Num> Dim<N> for Dim1D<N> {
    const NDIM: usize = 1;
    fn get(&self, axis: Axis) -> &N {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut N {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: N) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl<N: Num> Dim<N> for Dim2D<N> {
    const NDIM: usize = 2;
    fn get(&self, axis: Axis) -> &N {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut N {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: N) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl<N: Num> Dim<N> for Dim3D<N> {
    const NDIM: usize = 3;
    fn get(&self, axis: Axis) -> &N {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut N {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: N) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl<N: Num> Dim<N> for Dim4D<N> {
    const NDIM: usize = 4;
    fn get(&self, axis: Axis) -> &N {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut N {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: N) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl<N: Num> Dim<N> for Dim5D<N> {
    const NDIM: usize = 5;
    fn get(&self, axis: Axis) -> &N {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut N {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: N) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl<N: Num> Dim<N> for Dim6D<N> {
    const NDIM: usize = 6;
    fn get(&self, axis: Axis) -> &N {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut N {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: N) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}

// Make Dim a "sealed trait" https://rust-lang.github.io/api-guidelines/future-proofing.html#c-sealed
mod private {
    use super::*;

    pub trait Sealed {}
    impl<N: Num> Sealed for Dim1D<N> {}
    impl<N: Num> Sealed for Dim2D<N> {}
    impl<N: Num> Sealed for Dim3D<N> {}
    impl<N: Num> Sealed for Dim4D<N> {}
    impl<N: Num> Sealed for Dim5D<N> {}
    impl<N: Num> Sealed for Dim6D<N> {}
}
