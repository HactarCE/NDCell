use crate::automaton::space::{CellType, Coords};

pub trait Rule<T: CellType, C: Coords> {}
