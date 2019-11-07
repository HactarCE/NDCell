//! The simulation backend.
//!
//! This module contains everything needed to simulate cellular automata,
//! without displaying, importing, or exporting them.

mod algorithm;
pub mod space;

use algorithm::Rule;
use space::*;

use std::marker::PhantomData;

/// A cellular automaton simulation.
pub struct Automaton<T: CellType, C: Coords, R: Rule<T, C>> {
    phantom: PhantomData<(T, C)>,
    grid: Grid<T, C>,
    rule: R,
}
