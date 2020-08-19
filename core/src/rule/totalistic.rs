use super::{Rule, TransitionFunction};
use crate::space::*;
use regex::Regex;
use std::convert::TryFrom;

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

impl Rule<Dim2D> for MooreTotalistic2D {
    fn radius(&self) -> usize {
        1
    }
    fn transition_function(&self) -> TransitionFunction<Dim2D> {
        Box::new(move |nbhd, pos| {
            // Count live neighbors.
            let nbhd_shape = Rect2D::span(pos - self.radius(), pos + self.radius());
            let mut live_neighbors = 0;
            for cell_coords in nbhd_shape.iter() {
                if nbhd[cell_coords] != 0 {
                    live_neighbors += 1;
                }
            }
            // Index LUT to get next cell state.
            if nbhd[NdVec::origin()] != 0 {
                live_neighbors -= 1;
                self.survival[live_neighbors]
            } else {
                self.birth[live_neighbors]
            }
        })
    }
}

/// Conway's Game of Life, simulated using a general 2-state totalistic
/// 2D range-1 Moore-neighborhood algorithm.
pub const LIFE: MooreTotalistic2D = MooreTotalistic2D {
    birth: [0, 0, 0, 1, 0, 0, 0, 0, 0],
    survival: [0, 0, 1, 1, 0, 0, 0, 0, 0],
};
