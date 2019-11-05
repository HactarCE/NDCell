mod algorithm;
pub mod grid;

use algorithm::Rule;
use grid::{Cell, Dimension, Grid};

use std::marker::PhantomData;

pub struct Automaton<C: Cell, D: Dimension, G: Grid<C, D>, R: Rule<C, D>> {
    phantom: PhantomData<(C, D)>,
    grid: G,
    rule: R,
}
