use regex::Regex;
use std::convert::TryFrom;
use std::fmt;

use super::{NdRule, TransitionFunction};
use crate::dim::Dim2D;
use crate::ndrect::Rect2D;

/// A 2-state totalistic 2D range-1 Moore-neighborhood algorithm.
#[derive(Debug, Copy, Clone)]
pub struct MooreTotalistic2D {
    birth: [u8; 9],
    survival: [u8; 9],
}
impl Default for MooreTotalistic2D {
    fn default() -> Self {
        LIFE
    }
}

impl TryFrom<&str> for MooreTotalistic2D {
    type Error = ();

    fn try_from(s: &str) -> Result<Self, ()> {
        let regex = Regex::new(r"^[Bb](\d*)/?[Ss](\d*)$").unwrap();
        let captures = regex.captures(s).ok_or(())?;
        let mut conditions = [[0; 9]; 2];
        for i in 0..2 {
            for ch in captures[i].chars() {
                conditions[i][ch.to_string().parse::<usize>().unwrap()] = 1;
            }
        }
        Ok(Self {
            birth: conditions[0],
            survival: conditions[0],
        })
    }
}
impl fmt::Display for MooreTotalistic2D {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "B")?;
        for i in 0..=8 {
            if self.birth[i] != 0_u8 {
                write!(f, "{}", i)?;
            }
        }
        write!(f, "/S")?;
        for i in 0..=8 {
            if self.survival[i] != 0_u8 {
                write!(f, "{}", i)?;
            }
        }
        Ok(())
    }
}

impl NdRule<Dim2D> for MooreTotalistic2D {
    fn radius(&self) -> usize {
        1
    }
    fn transition_function(&self) -> TransitionFunction<'_, Dim2D> {
        Box::new(move |nbhd, rect| {
            super::transition_cell_array(rect, |pos| {
                // Count live neighbors.
                let nbhd_rect = Rect2D::span(pos - self.radius(), pos + self.radius());
                let live_neighbors = nbhd_rect
                    .iter()
                    .filter(|&neighbor_pos| nbhd[neighbor_pos] != 0)
                    .count();
                // Index LUT to get next cell state.
                if nbhd[pos] != 0 {
                    // Subtract 1 to exclude the center cell.
                    self.survival[live_neighbors - 1]
                } else {
                    self.birth[live_neighbors]
                }
            })
        })
    }
    fn max_state(&self) -> u8 {
        1
    }
}

/// Conway's Game of Life, simulated using a general 2-state totalistic
/// 2D range-1 Moore-neighborhood algorithm.
pub const LIFE: MooreTotalistic2D = MooreTotalistic2D {
    birth: [0, 0, 0, 1, 0, 0, 0, 0, 0],
    survival: [0, 0, 1, 1, 0, 0, 0, 0, 0],
};
