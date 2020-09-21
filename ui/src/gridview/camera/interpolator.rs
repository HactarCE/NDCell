//! Camera interpolation.

use std::marker::PhantomData;

use ndcell_core::dim::Dim;
use ndcell_core::num::{r64, ToPrimitive};

use super::Camera;
use crate::config::Interpolation;

/// Distance beneath which to "snap" to the target, for interpolation strategies
/// like exponential decay that never actually reach their target.
const DISTANCE_THRESHOLD: f64 = 0.1;

#[derive(Debug, Default, Copy, Clone)]
pub struct Interpolator<D: Dim, C: Camera<D>> {
    pub current: C,
    pub target: C,
    state: (),
    _marker: PhantomData<D>,
}
impl<D: Dim, C: Camera<D>> From<C> for Interpolator<D, C> {
    fn from(camera: C) -> Self {
        Self {
            current: camera.clone(),
            target: camera.clone(),
            state: (),
            _marker: PhantomData,
        }
    }
}

/// Stateful interpolation.
///
/// This abstracts away dimensionality, so it can be used as a traiit object.
pub trait Interpolate {
    /// Returns the distance between the current state and the target state.
    fn distance(&self) -> f64;

    /// Advances the state by one frame using the given interpolation strategy.
    ///
    /// Returns `true` if the target has been reached, or `false` otherwise.
    fn advance(&mut self, fps: f64, interpolation: Interpolation) -> bool;
}
impl<D: Dim, C: Camera<D>> Interpolate for Interpolator<D, C> {
    fn distance(&self) -> f64 {
        C::distance(&self.current, &self.target)
            .to_f64()
            .unwrap_or(f64::MAX)
    }

    fn advance(&mut self, fps: f64, interpolation: Interpolation) -> bool {
        if self.current == self.target || self.distance() < DISTANCE_THRESHOLD {
            true
        } else {
            let t = match interpolation {
                Interpolation::None => 1.0,
                Interpolation::Linear { speed } => speed / fps / self.distance(),
                Interpolation::Exponential { decay_constant } => 1.0 / fps / decay_constant,
            };
            self.current = C::lerp(
                &self.current,
                &self.target,
                // Clamp to 0 <= t <= 1. `min()` comes first so that `NaN`s will
                // become `1.0`.
                r64(t.min(1.0).max(0.0)),
            );
            false
        }
    }
}
