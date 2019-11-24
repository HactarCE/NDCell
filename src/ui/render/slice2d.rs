use crate::automaton::*;

pub trait AutomatonSlice2D {
    fn get_cell_state(&self, pos: Vec2D) -> bool;
    fn set_cell_state(&mut self, pos: Vec2D, new_state: bool);
    fn step(&mut self);
    fn get_population(&self) -> usize;
    fn get_max_layer(&self) -> usize;
    fn get_generations(&self) -> usize;
}

pub struct AutomatonBool2D<'a> {
    pub grid: NdTree<bool, Dim2D>,
    pub simulation: Simulation<'a, bool, Dim2D>,
    pub generations: usize,
}

impl<'a> AutomatonSlice2D for AutomatonBool2D<'a> {
    fn get_cell_state(&self, pos: Vec2D) -> bool {
        self.grid.get_cell(pos)
    }
    fn set_cell_state(&mut self, pos: Vec2D, new_state: bool) {
        self.grid.set_cell(pos, new_state);
    }
    fn step(&mut self) {
        self.simulation.step(&mut self.grid);
        self.generations += self.simulation.get_step_size();
    }
    fn get_population(&self) -> usize {
        self.grid.get_root().population
    }
    fn get_max_layer(&self) -> usize {
        self.grid.get_root().layer
    }
    fn get_generations(&self) -> usize {
        self.generations
    }
}
