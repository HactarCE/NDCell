//! The simulation backend.
//!
//! This module contains everything needed to simulate cellular automata,
//! without displaying, importing, or exporting them.

mod algorithm;
pub mod space;

use algorithm::Algorithm;
use space::*;

use std::marker::PhantomData;

/// A cellular automaton simulation.
pub struct Automaton<T: CellType, C: Coords, A: Algorithm<T, C>> {
    phantom: PhantomData<(T, C)>,
    grid: Grid<T, C>,
    algorithm: A,
}
