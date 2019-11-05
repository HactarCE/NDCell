//! The simulation backend.
//!
//! This module contains everything needed to simulate cellular automata,
//! without displaying, importing, or exporting them.

mod algorithm;
pub mod space;

use algorithm::Rule;
use grid::{Cell, Dimension, Grid};

use std::marker::PhantomData;

/// A cellular automaton simulation.
pub struct Automaton<C: Cell, D: Dimension, G: Grid<C, D>, R: Rule<C, D>> {
    phantom: PhantomData<(C, D)>,
    grid: G,
    rule: R,
}
