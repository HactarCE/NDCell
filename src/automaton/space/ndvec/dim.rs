use super::*;

/// A vector of a given dimensionality; mostly used as a type argument to convey
/// the number of dimensions something has.
///
/// This is basically exactly the same as ndarray's Dimension trait, except it
/// uses isize instead of usize (which is crucial for this application).
pub trait Dim: Debug + Clone + Eq + Hash + Copy {
    /// The number of dimensions (number of axes).
    const NDIM: usize;

    /// Returns a Vector of the axes of this many dimensions.
    fn axes() -> Vec<Axis> {
        ndim_axes(Self::NDIM)
    }

    /// Returns the coordinate along the given axis.
    fn get(&self, axis: Axis) -> &isize;

    /// Returns a mutable reference to the coordinate along the given axis.
    fn get_mut(&mut self, axis: Axis) -> &mut isize;

    /// Sets the coordinate along the given axis.
    fn set(&mut self, axis: Axis, value: isize);

    /// Returns whether these coordinates consists entirely of zeros.
    fn is_zero(&self) -> bool {
        for ax in Self::axes() {
            if self.get(ax) != &0 {
                return false;
            }
        }
        true
    }

    /// Returns the coordinates of the origin (i.e. all zeros).
    fn origin() -> Self;
}

/// A basic 1D vector type.
pub type Dim1D = [isize; 1];
/// A basic 2D vector type.
pub type Dim2D = [isize; 2];
/// A basic 3D vector type.
pub type Dim3D = [isize; 3];
/// A basic 4D vector type.
pub type Dim4D = [isize; 4];
/// A basic 5D vector type.
pub type Dim5D = [isize; 5];
/// A basic 6D vector type.
pub type Dim6D = [isize; 6];

impl Dim for Dim1D {
    const NDIM: usize = 1;
    fn get(&self, axis: Axis) -> &isize {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut isize {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: isize) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Dim for Dim2D {
    const NDIM: usize = 2;
    fn get(&self, axis: Axis) -> &isize {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut isize {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: isize) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Dim for Dim3D {
    const NDIM: usize = 3;
    fn get(&self, axis: Axis) -> &isize {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut isize {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: isize) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Dim for Dim4D {
    const NDIM: usize = 4;
    fn get(&self, axis: Axis) -> &isize {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut isize {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: isize) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Dim for Dim5D {
    const NDIM: usize = 5;
    fn get(&self, axis: Axis) -> &isize {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut isize {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: isize) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Dim for Dim6D {
    const NDIM: usize = 6;
    fn get(&self, axis: Axis) -> &isize {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut isize {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: isize) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
