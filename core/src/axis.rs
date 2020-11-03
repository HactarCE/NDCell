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
    pub fn name(self) -> &'static str {
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
    pub fn bit(self) -> usize {
        1 << self as usize
    }
}

/// List of axes in order.
pub const AXES: &'static [Axis] = &[Axis::X, Axis::Y, Axis::Z, Axis::W, Axis::U, Axis::V];

/// Returns a list of axes up to some number of dimensions.
#[inline]
pub fn ndim_axes(ndim: usize) -> &'static [Axis] {
    &AXES[..ndim]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_axis_iter() {
        assert_eq!(vec![Axis::X, Axis::Y, Axis::Z, Axis::W], ndim_axes(4))
    }
}
