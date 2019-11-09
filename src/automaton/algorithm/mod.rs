use crate::automaton::space::{CellType, Dim};

mod totalistic;

pub use totalistic::*;

pub trait Algorithm<T: CellType, D: Dim> {
    // fn transition()
}
