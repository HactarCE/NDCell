use ndcell_core::prelude::*;

/// Axis-aligned plane in 3D global space.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Plane {
    pub axis: Axis,
    pub coordinate: FixedPoint,
}
