use std::collections::HashMap;
use std::rc::Rc;

use crate::types::FnSignature;
use crate::{DEFAULT_NDIM, DEFAULT_STATE_COUNT};

/// Returns a list of cell states if the user does not specify, given an
/// optional number of states (defaults to DEFAULT_STATE_COUNT).
pub fn make_default_states(count: Option<usize>) -> Vec<CellState> {
    return vec![CellState::default(); count.unwrap_or(DEFAULT_STATE_COUNT)];
}

// TODO: move error points to RuleMeta

/// Metadata about a rule, such as the number of dimensions and a list of
/// possible cell states.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuleMeta {
    /// Raw source code.
    pub source_code: Rc<String>,
    /// Number of dimensions (from 1 to 6).
    pub ndim: u8,
    /// List of cell states.
    pub states: Vec<CellState>,
    /// Map of names and signatures of helper functions.
    pub helper_function_signatures: HashMap<String, FnSignature>,
    // /// Cell state tags.
    // tags: HashMap<String, Tag>,
}
impl Default for RuleMeta {
    fn default() -> Self {
        Self {
            source_code: Rc::new(String::new()),
            ndim: DEFAULT_NDIM,
            states: make_default_states(None),
            helper_function_signatures: HashMap::new(),
        }
    }
}
impl RuleMeta {
    /// Generates the given number of default cell states and uses those in this
    /// rule.
    pub fn with_default_cell_states(mut self, count: Option<usize>) -> Self {
        self.states = make_default_states(count);
        self
    }
}

/// A cell state.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CellState;
