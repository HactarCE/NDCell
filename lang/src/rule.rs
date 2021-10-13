use std::fmt;
use std::sync::Arc;

use crate::data::VectorSet;
use crate::exec::CompiledFunction;

/// Compiled transition function and rule metadata.
#[derive(Debug, Clone)]
pub struct CompiledRule {
    /// Transition function (contains `Arc`s).
    transition_function: CompiledFunction,
    /// Rule metadata.
    meta: Arc<RuleMeta>,
}
impl fmt::Display for CompiledRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.meta.name)
    }
}

/// Compiled rule metadata.
#[derive(Debug, Clone)]
pub struct RuleMeta {
    /// Rule name.
    pub name: String,
    /// Number of dimensions.
    pub ndim: usize,
    /// Number of states.
    pub nstates: usize,
    /// Neighborhood shape.
    pub nbhd: VectorSet,
}
