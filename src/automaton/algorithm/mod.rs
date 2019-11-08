use crate::automaton::space::{CellType, Coords};

mod totalistic;

pub use totalistic::*;

pub trait Algorithm<T: CellType, C: Coords> {
    // fn transition()
}
