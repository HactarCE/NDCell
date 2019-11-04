mod algorithm;
mod grid;

use algorithm::Rule;
use grid::Grid;

use ndarray::{Dimension, NdIndex};
use std::marker::PhantomData;

struct Automaton<C, D: Dimension, I: NdIndex<D>, G: Grid<C, D, I>, R: Rule<C, D>> {
    cell_type: PhantomData<C>,
    dimensionality: PhantomData<D>,
    index_type: PhantomData<I>,
    grid: G,
    rule: R,
}
