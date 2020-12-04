//! Camera interpolation.

use anyhow::Result;
use std::any::Any;
use std::convert::TryFrom;
use std::fmt;
use std::marker::PhantomData;

use ndcell_core::prelude::*;

use super::{Camera, CellTransform, DragHandler, DragOutcome, NdCellTransform};
use crate::commands::ViewCommand;
use crate::config::{Config, Interpolation};

/// Distance beneath which to "snap" to the target, for interpolation strategies
/// like exponential decay that never actually reach their target.
const DISTANCE_THRESHOLD: f64 = 0.001;

#[derive(Default)]
pub struct Interpolator<D, C> {
    pub current: C,
    pub target: C,
    state: (),
    _marker: PhantomData<D>,
}
impl<D: Dim, C: Camera<D>> fmt::Debug for Interpolator<D, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.current, f)
    }
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
/// This abstracts away dimensionality, so it can be used as a trait object.
pub trait Interpolate: Any {
    /// Sets the display scaling factor of the underlying camera.
    fn set_dpi(&mut self, dpi: f32);

    /// Returns the distance between the current state and the target state.
    fn distance(&self) -> f64;

    /// Advances the state by one frame using the given interpolation strategy.
    ///
    /// Returns `true` if the target has been reached, or `false` otherwise.
    fn advance(&mut self, fps: f64, interpolation: Interpolation) -> bool;

    /// Update the pixel size of the viewport.
    fn update_target_dimensions(&mut self, target_dimensions: (u32, u32));
}
impl<D: Dim, C: Camera<D>> Interpolate for Interpolator<D, C>
where
    for<'a> &'a NdCellTransform<D>: TryFrom<&'a CellTransform>,
{
    fn set_dpi(&mut self, dpi: f32) {
        self.current.set_dpi(dpi);
        self.target.set_dpi(dpi);
    }

    fn distance(&self) -> f64 {
        C::distance(&self.current, &self.target)
            .to_f64()
            .unwrap_or(f64::MAX)
    }

    fn advance(&mut self, fps: f64, interpolation: Interpolation) -> bool {
        if self.current == self.target {
            true
        } else if self.distance() < DISTANCE_THRESHOLD {
            self.current = self.target.clone();
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

    fn update_target_dimensions(&mut self, target_dimensions: (u32, u32)) {
        self.target.set_target_dimensions(target_dimensions);
        self.current.set_target_dimensions(target_dimensions);
    }
}

impl<D: Dim, C: Camera<D>> Interpolator<D, C> {
    /// Executes a movement command, interpolating if necessary.
    pub fn do_view_command(
        &mut self,
        command: ViewCommand,
        config: &Config,
    ) -> Result<Option<DragHandler<Self>>> {
        Ok(self
            .target
            .do_view_command(command, config)?
            // Turn the DragHandler<C> into a DragHandler<Interpolator<D, C>>.
            .map(|mut camera_drag_handler| -> DragHandler<Self> {
                Box::new(move |this, cursor_pos| {
                    // Skip interpolation for dragging.
                    camera_drag_handler(&mut this.target, cursor_pos)?;
                    camera_drag_handler(&mut this.current, cursor_pos)?;
                    Ok(DragOutcome::Continue)
                })
            }))
    }
}
