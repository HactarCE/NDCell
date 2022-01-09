//! Enumeration of axes.
//!
//! These are mainly used for indexing specific components in an `NdVec`.

pub use Axis::*;

/// Enumeration of the six dimensions supported by NDCell.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Axis {
    /// X axis (generally horizontal/"width").
    X = 0,
    /// Y axis (generally vertical/"height").
    Y = 1,
    /// Z axis (generally normal/"depth").
    Z = 2,
    /// W axis (4th dimension).
    W = 3,
    /// U axis (5th dimension).
    U = 4,
    /// V axis (6th dimension).
    V = 5,
}

impl Axis {
    /// Returns the name of the axis.
    #[inline]
    pub const fn name(self) -> &'static str {
        match self {
            Axis::X => "X",
            Axis::Y => "Y",
            Axis::Z => "Z",
            Axis::W => "W",
            Axis::U => "U",
            Axis::V => "V",
        }
    }

    /// Returns the bit in a child index corresponding to this axis.
    #[inline]
    pub const fn bit(self) -> usize {
        1 << self as usize
    }
}

/// List of axes in order.
pub const AXES: &[Axis] = &[Axis::X, Axis::Y, Axis::Z, Axis::W, Axis::U, Axis::V];

/// Returns a list of axes up to some number of dimensions.
#[inline]
pub fn ndim_axes(ndim: usize) -> &'static [Axis] {
    &AXES[..ndim]
}

/// Unordered set of axes.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
pub struct AxisSet(u8);
impl AxisSet {
    /// Creates an empty set of axes.
    pub const fn empty() -> Self {
        Self(0)
    }
    /// Creates a set containing a single axis.
    pub const fn single(axis: Axis) -> Self {
        Self(axis.bit() as u8)
    }
    /// Creates a set containing the axes for which `f` returns `true`, given a
    /// number of dimensions.
    pub fn from_fn(ndim: usize, mut f: impl FnMut(Axis) -> bool) -> Self {
        let mut ret = Self::empty();
        for &ax in ndim_axes(ndim) {
            if f(ax) {
                ret.add(ax);
            }
        }
        ret
    }

    /// Adds an axis to the set.
    pub fn add(&mut self, axis: Axis) {
        self.0 |= axis.bit() as u8;
    }
    /// Removes an axis from the set.
    pub fn remove(&mut self, axis: Axis) {
        self.0 &= !axis.bit() as u8;
    }
    /// Returns whether the set contains an axis.
    pub fn contains(self, axis: Axis) -> bool {
        self.0 & axis.bit() as u8 != 0
    }
}
impl From<Axis> for AxisSet {
    fn from(axis: Axis) -> Self {
        Self::single(axis)
    }
}
impl From<&[Axis]> for AxisSet {
    fn from(axes: &[Axis]) -> Self {
        Self(axes.iter().map(|&ax| ax.bit() as u8).fold(0, |a, b| a | b))
    }
}
impl std::iter::Iterator for AxisSet {
    type Item = Axis;

    fn next(&mut self) -> Option<Self::Item> {
        // `trailing_zeros()` gives the index of the least-significant set bit.
        let next_axis = *AXES.get(self.0.trailing_zeros() as usize)?;
        self.remove(next_axis);
        Some(next_axis)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_axis_iter() {
        assert_eq!(vec![Axis::X, Axis::Y, Axis::Z, Axis::W], ndim_axes(4))
    }
}
