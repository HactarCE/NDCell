//! Type aliases for `NdRule`s.

use std::sync::Arc;

use super::NdRule;
use crate::dim::*;

/// 1D cellular automaton rule.
pub type Rule1D = Arc<dyn NdRule<Dim1D>>;
/// 2D cellular automaton rule.
pub type Rule2D = Arc<dyn NdRule<Dim2D>>;
/// 3D cellular automaton rule.
pub type Rule3D = Arc<dyn NdRule<Dim3D>>;
/// 4D cellular automaton rule.
pub type Rule4D = Arc<dyn NdRule<Dim4D>>;
/// 5D cellular automaton rule.
pub type Rule5D = Arc<dyn NdRule<Dim5D>>;
/// 6D cellular automaton rule.
pub type Rule6D = Arc<dyn NdRule<Dim6D>>;
