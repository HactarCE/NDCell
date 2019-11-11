use crate::automaton::*;

pub trait AutomatonSlice2D {
    fn get_cell_state(&self, cell_coords: CellCoords2D) -> bool;
    fn set_cell_state(&mut self, cell_coords: CellCoords2D, new_state: bool);
    fn step(&mut self);
}

impl<A: Algorithm<bool, Coords2D>> AutomatonSlice2D for Automaton<bool, Coords2D, A> {
    fn get_cell_state(&self, cell_coords: CellCoords2D) -> bool {
        self.grid[cell_coords]
    }
    fn set_cell_state(&mut self, cell_coords: CellCoords2D, new_state: bool) {
        self.grid[cell_coords] = new_state;
    }
    fn step(&mut self) {
        Automaton::step(self)
    }
}
