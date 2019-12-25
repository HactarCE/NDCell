/// An enumeration of the six dimensions supported by this software.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Axis {
    /// The X axis (generally horizontal/"width").
    X = 0,
    /// The Y axis (generally vertical/"height").
    Y = 1,
    /// The Z axis (generally normal/"depth").
    Z = 2,
    /// The W axis (4th dimension).
    W = 3,
    /// The U axis (5th dimension).
    U = 4,
    /// The V axis (6th dimension).
    V = 5,
}

impl Axis {
    /// Returns the name of the given axis.
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

    /// Returns the bitmask of this axis in a branch index. (See
    /// NdTreeNode::branch_idx_at_layer() for more info.)
    pub fn branch_bit(self) -> usize {
        1 << self as usize
    }
}

impl From<usize> for Axis {
    fn from(axis_index: usize) -> Self {
        match axis_index {
            0 => Axis::X,
            1 => Axis::Y,
            2 => Axis::Z,
            3 => Axis::W,
            4 => Axis::U,
            5 => Axis::V,
            _ => unreachable!("Dimensions above 6 are not supported"),
        }
    }
}

const AXES: &'static [Axis] = &[Axis::X, Axis::Y, Axis::Z, Axis::W, Axis::U, Axis::V];

/// Returns a vector of axes given a number of dimensions.
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
