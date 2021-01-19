use ndcell_core::prelude::*;

/// All cardinal and ordical directions.
pub const DIRECTIONS: [Direction; 8] = [
    Direction::N,
    Direction::NE,
    Direction::E,
    Direction::SE,
    Direction::S,
    Direction::SW,
    Direction::W,
    Direction::NW,
];

/// 2D cardinal or ordinal direction.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Direction {
    /// North.
    N,
    /// Northeast.
    NE,
    /// East.
    E,
    /// Southeast.
    SE,
    /// South.
    S,
    /// Southwest.
    SW,
    /// West.
    W,
    /// Northwest.
    NW,
}
impl Direction {
    /// Returns the X component of the vector in this direction (0, -1, or +1).
    pub fn x(self) -> isize {
        match self {
            Direction::N | Direction::S => 0,
            Direction::NE | Direction::E | Direction::SE => 1,
            Direction::SW | Direction::W | Direction::NW => -1,
        }
    }
    /// Returns the Y component of the vector in this direction (0, -1, or +1).
    pub fn y(self) -> isize {
        match self {
            Direction::E | Direction::W => 0,
            Direction::NW | Direction::N | Direction::NE => 1,
            Direction::SE | Direction::S | Direction::SW => -1,
        }
    }
    /// Returns the vector in this direction where each component is either 0,
    /// -1, or +1.
    pub fn vector(self) -> IVec2D {
        NdVec([self.x(), self.y()])
    }
    /// Returns the set of nonzero axes for the vector in this direction.
    pub fn axes(self) -> AxisSet {
        let mut ret = AxisSet::empty();
        if self.x() != 0 {
            ret.add(Axis::X);
        }
        if self.y() != 0 {
            ret.add(Axis::Y);
        }
        ret
    }
}
