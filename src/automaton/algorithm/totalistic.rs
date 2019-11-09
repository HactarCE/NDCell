use super::Algorithm;
use crate::automaton::space;
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

impl Algorithm<bool, space::Coords3D> for MooreTotalistic2D {}

pub const LIFE: MooreTotalistic2D = MooreTotalistic2D {
    birth: [false, false, false, true, false, false, false, false, false],
    survival: [false, false, true, true, false, false, false, false, false],
};
