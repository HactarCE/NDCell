use std::ops::Index;

use super::*;

/// The cell states in a given cell's neighborhood.
///
/// The term "napkin" as used in this context has [a convoluted
/// etymology](https://github.com/supposedly/nutshell#glossary-of-nutshell-specific-terms),
/// but in a [nutshell](https://github.com/supposedly/nutshell), it's the states
/// belonging to each of a cell's neighbors. While "neighborhood" refers to the
/// shape/region around a cell that determines its next state, "napkin" refers
/// to the actual states of the cells in the neighborhood.
pub struct Napkin<'a, T: CellType, D: Dim, R: Region<D>> {
    /// The grid that this napkin is taken from.
    pub grid: &'a Grid<T, D>,
    /// The region describing the shape of this napkin.
    pub region: &'a R,
    /// The transformation to apply to points.
    ///
    /// For example, if this is the napkin for a cell at (3, 2), then the
    /// transformation would be to add the vector (3, 2).
    pub transformation: &'a dyn Fn(CellCoords<D>) -> CellCoords<D>,
}

impl<'a, T: CellType, D: Dim, R: Region<D>> Index<CellCoords<D>> for Napkin<'a, T, D, R> {
    type Output = T;
    fn index(&self, cell_coords: CellCoords<D>) -> &T {
        if self.region.contains(cell_coords) {
            &self.grid[(self.transformation)(cell_coords)]
        } else {
            panic!("Cannot retrieve cell from outside region")
        }
    }
}

impl<'a, T: CellType, D: Dim, R: Region<D>> Napkin<'a, T, D, R> {
    fn get(&self, cell_coords: CellCoords<D>) -> Option<T> {
        if self.region.contains(cell_coords) {
            Some(self[cell_coords])
        } else {
            None
        }
    }
}
