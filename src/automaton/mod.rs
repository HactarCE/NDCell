mod algorithm;
mod grid;

use algorithm::Rule;
use grid::{Cell, Grid};

use ndarray::{Dimension, NdIndex};
use std::marker::PhantomData;

struct Automaton<C: Cell, D: Dimension, I: NdIndex<D>, G: Grid<C, D, I>, R: Rule<C, D>> {
    phantom_data: PhantomData<(C, D, I)>,
    grid: G,
    rule: R,
}
