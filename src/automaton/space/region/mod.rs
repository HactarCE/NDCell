use super::*;

// pub mod neighborhood;
mod rect;

pub use rect::*;

/// A set of cell locations.
pub trait Region<D: Dim>: IntoIterator<Item = CellCoords<D>> {
    /// Returns whether a given cell location is within the region.
    fn contains(&self, coords: CellCoords<D>) -> bool;
    /// Returns whether the region is empty (contains no cell locations).
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// Returns the number of cell locations in the region.
    fn len(&self) -> usize;
}
