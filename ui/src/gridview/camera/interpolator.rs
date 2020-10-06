//! Camera interpolation.

use anyhow::Result;
use std::convert::TryFrom;
use std::fmt;
use std::marker::PhantomData;

use ndcell_core::prelude::*;

use super::{Camera, CameraDragHandler, CellTransform, NdCellTransform};
use crate::commands::{DragCommand, ViewCommand};
use crate::config::{Config, Interpolation};

/// Distance beneath which to "snap" to the target, for interpolation strategies
/// like exponential decay that never actually reach their target.
const DISTANCE_THRESHOLD: f64 = 0.001;

#[derive(Default)]
pub struct Interpolator<D, C> {
    pub current: C,
    pub target: C,
    state: (),
    drag_handler: Option<CameraDragHandler<C>>,
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
            drag_handler: None,
            _marker: PhantomData,
        }
    }
}

/// Stateful interpolation.
///
/// This abstracts away dimensionality, so it can be used as a trait object.
pub trait Interpolate {
    /// Returns the distance between the current state and the target state.
    fn distance(&self) -> f64;

    /// Advances the state by one frame using the given interpolation strategy.
    ///
    /// Returns `true` if the target has been reached, or `false` otherwise.
    fn advance(&mut self, fps: f64, interpolation: Interpolation) -> bool;

    /// Whether the user is currently dragging the viewport by holding down a
    /// mouse button.
    fn is_dragging(&self) -> bool;

    /// Update the pixel size of the viewport.
    fn update_target_dimensions(&mut self, target_dimensions: (u32, u32));

    /// Executes a movement command, interpolating if necessary.
    ///
    /// Returns an `Err` if the dimensionality of `cell_transform` does not
    /// match the dimensionality of the camera.
    fn do_move_command(&mut self, command: ViewCommand, config: &Config) -> Result<()>;
}
impl<D: Dim, C: Camera<D>> Interpolate for Interpolator<D, C>
where
    for<'a> &'a NdCellTransform<D>: TryFrom<&'a CellTransform>,
{
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

    fn is_dragging(&self) -> bool {
        self.drag_handler.is_some()
    }

    fn update_target_dimensions(&mut self, target_dimensions: (u32, u32)) {
        self.target.set_target_dimensions(target_dimensions);
        self.current.set_target_dimensions(target_dimensions);
    }

    fn do_move_command(&mut self, command: ViewCommand, config: &Config) -> Result<()> {
        match command {
            ViewCommand::Drag(DragCommand::Continue { cursor_pos }) => {
                if let Some(h) = &mut self.drag_handler {
                    // Skip interpolation.
                    h(&mut self.target, cursor_pos);
                    h(&mut self.current, cursor_pos);
                }
            }

            ViewCommand::Drag(DragCommand::Stop) => {
                self.drag_handler = None;
            }

            c @ ViewCommand::Drag(_) => {
                self.drag_handler = self.target.do_move_command(c, config)?;
            }

            c => {
                self.target.do_move_command(c, config)?;
            }
        };

        Ok(())
    }
}
