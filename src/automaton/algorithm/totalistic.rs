use super::Algorithm;
use crate::automaton::space::*;
use regex::Regex;
use std::convert::TryFrom;

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

impl Algorithm<bool, Vec2D> for MooreTotalistic2D {
    fn get_radius(&self) -> usize {
        1
    }

    fn transition(&self, napkin: &NdTree<bool, Vec2D>) -> bool {
        // Count live neighbors.
        let region = napkin.region;
        let mut live_neighbors = 0;
        for cell_coords in region.into_iter() {
            if napkin[cell_coords] {
                live_neighbors += 1;
            }
        }
        // Index LUT to get next cell state.
        if napkin[NdVec::origin()] {
            live_neighbors -= 1;
            self.survival[live_neighbors]
        } else {
            self.birth[live_neighbors]
        }
    }
}

pub const LIFE: MooreTotalistic2D = MooreTotalistic2D {
    birth: [false, false, false, true, false, false, false, false, false],
    survival: [false, false, true, true, false, false, false, false, false],
};
