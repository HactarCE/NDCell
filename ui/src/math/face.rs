use std::fmt;

use ndcell_core::prelude::*;
use Axis::{X, Y, Z};

/// Axis-aligned 3D cube face.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Face {
    PosX,
    PosY,
    PosZ,
    NegX,
    NegY,
    NegZ,
}
impl fmt::Display for Face {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Face::*;
        match self {
            PosX => write!(f, "+X"),
            PosY => write!(f, "+Y"),
            PosZ => write!(f, "+Z"),
            NegX => write!(f, "-X"),
            NegY => write!(f, "-Y"),
            NegZ => write!(f, "-Z"),
        }
    }
}
impl Face {
    /// Returns a face with a positive normal along an axis.
    pub fn positive(axis: Axis) -> Self {
        use Face::*;
        match axis {
            X => PosX,
            Y => PosY,
            Z => PosZ,
            _ => panic!("Invalid 3D axis: {:?}", axis),
        }
    }
    /// Returns a face with a negative normal along an axis.
    pub fn negative(axis: Axis) -> Self {
        use Face::*;
        match axis {
            X => NegX,
            Y => NegY,
            Z => NegZ,
            _ => panic!("Invalid 3D axis: {:?}", axis),
        }
    }

    /// Returns the sign of the nonzero component of the normal vector.
    pub fn sign(self) -> Sign {
        use Face::*;
        match self {
            PosX | PosY | PosZ => Sign::Plus,
            NegX | NegY | NegZ => Sign::Minus,
        }
    }
    /// Returns the normal axis.
    pub fn normal_axis(self) -> Axis {
        use Face::*;
        match self {
            PosX | NegX => X,
            PosY | NegY => Y,
            PosZ | NegZ => Z,
        }
    }
    /// Returns the two perpendicular axes, in order.
    pub fn plane_axes(self) -> [Axis; 2] {
        use Face::*;
        match self {
            PosX => [Y, Z],
            PosY => [Z, X],
            PosZ => [X, Y],
            NegX => [Z, Y],
            NegY => [X, Z],
            NegZ => [Y, X],
        }
    }

    /// Returns the normal vector, which has a single nonzero component that is
    /// either +1 or -1.
    pub fn normal(self) -> [i8; 3] {
        use Face::*;
        match self {
            PosX => [1, 0, 0],
            PosY => [0, 1, 0],
            PosZ => [0, 0, 1],
            NegX => [-1, 0, 0],
            NegY => [0, -1, 0],
            NegZ => [0, 0, -1],
        }
    }
}
