//! Viewpoint interpolation.

use std::fmt;
use std::marker::PhantomData;
use std::time::Duration;

use ndcell_core::prelude::*;

use super::{DragOutcome, DragUpdateViewFn, Viewpoint};
use crate::commands::DragViewCmd;
use crate::config::Interpolation;

/// Distance beneath which to "snap" to the target, for interpolation strategies
/// like exponential decay that never actually reach their target.
const DISTANCE_THRESHOLD: f64 = 0.001;

#[derive(Default)]
pub struct Interpolator<D, V> {
    pub current: V,
    pub target: V,
    state: (),
    _marker: PhantomData<D>,
}
impl<D: Dim, V: Viewpoint<D>> fmt::Debug for Interpolator<D, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.current, f)
    }
}
impl<D: Dim, V: Viewpoint<D>> From<V> for Interpolator<D, V> {
    fn from(viewpoint: V) -> Self {
        Self {
            current: viewpoint.clone(),
            target: viewpoint,
            state: (),
            _marker: PhantomData,
        }
    }
}

impl<D: Dim, V: Viewpoint<D>> Interpolator<D, V> {
    /// Returns the distance between the current state and the target state.
    pub fn distance(&self) -> f64 {
        V::distance(&self.current, &self.target)
            .to_f64()
            .unwrap_or(f64::MAX)
    }

    /// Advances the state by one frame using the given interpolation strategy.
    ///
    /// Returns `true` if the target has been reached, or `false` otherwise.
    pub fn advance(&mut self, frame_duration: Duration, interpolation: Interpolation) -> bool {
        let seconds_elapsed = frame_duration.as_secs_f64();

        if self.current == self.target {
            true
        } else if self.distance() < DISTANCE_THRESHOLD {
            self.current = self.target.clone();
            true
        } else {
            let t = match interpolation {
                Interpolation::None => 1.0,
                Interpolation::Linear {
                    speed: distance_per_second,
                } => distance_per_second * seconds_elapsed / self.distance(),
                Interpolation::Exponential { decay_constant } => seconds_elapsed / decay_constant,
            };
            self.current = V::lerp(
                &self.current,
                &self.target,
                // Clamp to 0 <= t <= 1. `min()` comes first so that `NaN`s will
                // become `1.0`.
                r64(t.min(1.0).max(0.0)),
            );
            false
        }
    }

    /// Sets the display scaling factor of the underlying viewpoint.
    pub fn set_dpi(&mut self, dpi: f32) {
        self.current.set_dpi(dpi);
        self.target.set_dpi(dpi);
    }
    /// Updates the target dimensions.
    pub fn set_target_dimensions(&mut self, target_dimensions: (u32, u32)) {
        self.target.set_target_dimensions(target_dimensions);
        self.current.set_target_dimensions(target_dimensions);
    }

    /// Returns the drag update function for a drag view command.
    pub fn make_drag_update_fn(
        &self,
        command: DragViewCmd,
        cursor_start: FVec2D,
    ) -> Option<DragUpdateViewFn<Self>> {
        match command {
            DragViewCmd::Orbit3D => self.target.drag_orbit_3d(cursor_start),
            DragViewCmd::Pan => self.target.drag_pan(cursor_start),
            DragViewCmd::PanHorizontal3D => self.target.drag_pan_horizontal_3d(cursor_start),
            DragViewCmd::Scale => self.target.drag_scale(cursor_start),
        }
        .map(Self::wrap_drag_update_fn)
    }

    /// Wraps a drag update function to operate on an `Interpolator` instead of
    /// a `Viewpoint` directly.
    fn wrap_drag_update_fn(mut h: DragUpdateViewFn<V>) -> DragUpdateViewFn<Self> {
        Box::new(move |this, cursor_pos| {
            // Skip interpolation for dragging.
            h(&mut this.target, cursor_pos)?;
            h(&mut this.current, cursor_pos)?;
            Ok(DragOutcome::Continue)
        })
    }
}
