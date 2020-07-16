use std::collections::HashMap;
use std::rc::Rc;

use crate::types::{FnSignature, PatternShape};
use crate::{DEFAULT_NBHD_RADIUS, DEFAULT_NDIM, DEFAULT_STATE_COUNT};

// TODO: move error points to RuleMeta

/// Metadata about a rule, such as the number of dimensions and a list of
/// possible cell states.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuleMeta {
    /// Raw source code.
    pub source_code: Rc<String>,
    /// Map of names and signatures of helper functions.
    pub helper_function_signatures: HashMap<String, FnSignature>,

    /// Number of dimensions (from 1 to 6).
    pub ndim: usize,
    /// Neighborhood shape.
    pub nbhd_shape: PatternShape,

    /// List of cell states.
    pub states: Vec<CellState>,
    // /// Cell state tags.
    // tags: HashMap<String, Tag>,
    /// Whether this rule has cell state lookup tables to apply rotations and
    /// reflections to individual cells. (This field will probably be replaced
    /// when cell state LUTs are actually implemented.)
    pub has_cell_state_luts: bool,
}
impl Default for RuleMeta {
    fn default() -> Self {
        Self::from_rule_description(Rc::new(String::new()), RuleDescription::default())
    }
}
impl RuleMeta {
    /// Constructs new rule metadata from a rule description.
    pub fn from_rule_description(source_code: Rc<String>, desc: RuleDescription) -> Self {
        let ndim = desc.ndim.unwrap_or(DEFAULT_NDIM);
        let radius = desc.radius.unwrap_or(DEFAULT_NBHD_RADIUS);
        let nbhd_shape = PatternShape::moore(ndim, radius);
        let states = match desc.states {
            RuleStatesDescription::None => make_default_states(DEFAULT_STATE_COUNT),
            RuleStatesDescription::Count(n) => make_default_states(n),
        };
        Self {
            source_code,
            helper_function_signatures: HashMap::new(),

            ndim,
            nbhd_shape,

            states,
            has_cell_state_luts: false,
        }
    }
}

/// Description of a rule, similar to what would be provided by the user with
/// directives. This is not meant for direct use, but as an intermediate step to
/// create a proper RuleMeta struct.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct RuleDescription {
    pub ndim: Option<usize>,
    pub radius: Option<usize>,
    pub states: RuleStatesDescription,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuleStatesDescription {
    /// There is no `@states` directive.
    None,
    /// `@states` is followed by a number.
    Count(usize),
}
impl Default for RuleStatesDescription {
    fn default() -> Self {
        Self::None
    }
}

/// Returns a list of default cell states, given a number of states.
fn make_default_states(state_count: usize) -> Vec<CellState> {
    return vec![CellState::default(); state_count];
}

/// A cell state.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CellState;
