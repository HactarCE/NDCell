const DEFAULT_NDIM: u8 = 2;
const DEFAULT_STATES: &[CellState] = &[CellState, CellState];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuleMeta {
    /// Number of dimensions (from 1 to 6).
    ndim: u8,
    /// List of cell states.
    states: Vec<CellState>,
    // /// Cell state tags.
    // tags: HashMap<String, Tag>,
}

impl RuleMeta {
    pub fn new() -> Self {
        Self {
            ndim: 2,
            states: vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CellState;
