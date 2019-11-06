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
pub struct Automaton<C: Cell, V: Vector, R: Rule<C, V>> {
    phantom: PhantomData<(C, V)>,
    grid: Grid<C, V>,
    rule: R,
}
