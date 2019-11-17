use super::Rule;
use crate::automaton::space::*;
use regex::Regex;
use std::convert::TryFrom;

/// A 2-state totalistic 2D range-1 Moore-neighborhood algorithm.
pub struct MooreTotalistic2D {
    birth: [bool; 9],
    survival: [bool; 9],
}

impl TryFrom<&str> for MooreTotalistic2D {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, ()> {
        let regex = Regex::new(r"^[Bb](\d*)/?[Ss](\d*)$").unwrap();
        let captures = regex.captures(s).ok_or(())?;
        let mut conditions = [[false; 9]; 2];
        for i in 0..2 {
            for ch in captures[i].chars() {
                conditions[i][ch.to_string().parse::<usize>().unwrap()] = true;
            }
        }
        Ok(Self {
            birth: conditions[0],
            survival: conditions[0],
        })
    }
}

impl Rule<bool, Dim2D> for MooreTotalistic2D {
    fn radius(&self) -> usize {
        1
    }

    fn transition(&self, napkin: &NdTree2D<bool>) -> bool {
        // Count live neighbors.
        let nbhood = Rect2D::moore(self.radius());
        let mut live_neighbors = 0;
        for cell_coords in nbhood.iter() {
            if napkin.get_cell(cell_coords) {
                live_neighbors += 1;
            }
        }
        // Index LUT to get next cell state.
        if napkin.get_cell(NdVec::origin()) {
            live_neighbors -= 1;
            self.survival[live_neighbors]
        } else {
            self.birth[live_neighbors]
        }
    }
}

/// Conway's Game of Life, simulated using a general 2-state totalistic
/// 2D range-1 Moore-neighborhood algorithm.
pub const LIFE: MooreTotalistic2D = MooreTotalistic2D {
    birth: [false, false, false, true, false, false, false, false, false],
    survival: [false, false, true, true, false, false, false, false, false],
};
