use ndarray;
use std::collections::HashMap;
use std::marker::PhantomData;

// struct Grid<D: ndarray::Dimension> {
//     // chunks: HashMap<Box<dyn ndarray::NdIndex<D>>, Box<dyn ndarray::Array<Cell, D>>>,
//     chunks: HashMap<dyn ndarray::NdIndex<D>, ndarray::Array<Cell, D>>,
// }

struct ArrayGrid<C, D: ndarray::Dimension> {
    array: ndarray::Array<C, D>,
}

trait Grid<C, D: ndarray::Dimension> {
    fn get_cell<I: ndarray::NdIndex<D>>(&self, index: I) -> Option<&C>;
}

impl<C, D: ndarray::Dimension> Grid<C, D> for ArrayGrid<C, D> {
    fn get_cell<I: ndarray::NdIndex<D>>(&self, index: I) -> Option<&C> {
        self.array.get(index)
    }
}

trait Rule<C, D: ndarray::Dimension> {}

struct Automaton<C, D: ndarray::Dimension, G: Grid<C, D>, R: Rule<C, D>> {
    grid: G,
    rule: R,
    cell_type: PhantomData<C>,
    dimensionality: PhantomData<D>,
}
