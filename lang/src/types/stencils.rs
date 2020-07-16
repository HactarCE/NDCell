use std::collections::HashMap;
use std::rc::Rc;

use super::{CellStateFilter, LangCellState};

/// A pattern stencil.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stencil {
    /// Flattened cell configuration.
    pub cells: Vec<StencilCell>,
    /// Cell bindings.
    pub bindings: HashMap<StencilCell, StencilCellFilter>,
}

/// A single cell in a pattern stencil.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StencilCell {
    Number(LangCellState),
    Ident(Rc<String>),
    Other(char),
}
impl StencilCell {
    /// Returns the default binding for this cell.
    pub fn default_binding(&self, state_count: usize) -> StencilCellFilter {
        let filter = match self {
            StencilCell::Number(i) => CellStateFilter::single_cell_state(state_count, *i),
            StencilCell::Ident(_) => CellStateFilter::all(state_count),
            StencilCell::Other('.') => CellStateFilter::single_cell_state(state_count, 0),
            StencilCell::Other('#') => {
                CellStateFilter::all(state_count)
                    & !CellStateFilter::single_cell_state(state_count, 0)
            }
            StencilCell::Other(_) => CellStateFilter::all(state_count),
        };
        StencilCellFilter {
            filter,
            same: false,
        }
    }
}

/// A cell state filter for a stencil cell.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StencilCellFilter {
    /// Cell states to match.
    pub filter: CellStateFilter,
    /// Whether all cells with this symbol must be the same state.
    pub same: bool,
}
