use anyhow::{anyhow, Context, Result};
use cgmath::prelude::*;
use cgmath::{Deg, Euler, Matrix3, Quaternion, Rad};
use log::warn;

use ndcell_core::axis::{X, Y, Z};
use ndcell_core::prelude::*;

use super::{Camera, CellTransform, CellTransform3D, Scale};
use crate::config::{Config, CtrlConfig, ForwardAxis3D, Interpolation, UpAxis3D};
use crate::gridview::commands::MoveCommand;

#[derive(Debug, Clone, PartialEq)]
pub struct Camera3D {
    /// Scale.
    scale: Scale,
    /// Rotation.
    orientation: Euler<Rad<f32>>,
    /// Translation.
    pivot: FixedVec3D,
}

impl Default for Camera3D {
    fn default() -> Self {
        Self {
            scale: Scale::default(),
            orientation: Euler::new(
                Self::DEFAULT_PITCH.into(),
                Self::DEFAULT_YAW.into(),
                Deg(0.0).into(),
            ),
            pivot: FixedVec3D::repeat(r64(0.5).into()),
        }
    }
}

impl Camera3D {
    /// Number of scaled units away from the pivot to position the camera.
    pub const DISTANCE_TO_PIVOT: f64 = 512.0;

    pub const DEFAULT_PITCH: Deg<f32> = Deg(30.0);
    pub const DEFAULT_YAW: Deg<f32> = Deg(20.0);

    /// Returns the orientation of the camera.
    pub fn orientation(&self) -> Euler<Rad<f32>> {
        self.orientation
    }
    /// Returns the orientation of the camera.
    pub fn set_orientation(&mut self, orientation: impl Into<Euler<Rad<f32>>>) {
        self.orientation = orientation.into();
        // Clamp pitch to -90..+90.
        self.orientation.x = self.orientation.x.normalize_signed();
        if self.orientation.x > Rad::turn_div_4() {
            self.orientation.x = Rad::turn_div_4();
        }
        if self.orientation.x < -Rad::turn_div_4() {
            self.orientation.x = -Rad::turn_div_4();
        }
        // Clamp yaw to 0..360.
        self.orientation.y = self.orientation.y.normalize();
        // Set roll to 0.
        self.orientation.z = Rad(0.0);
    }
    /// Returns the unit vector along which the camera is looking.
    pub fn look_vector(&self) -> FVec3D {
        fvec3d_from_euler(Z, self.orientation())
    }
    /// Returns the real position of the camera (not the pivot).
    pub fn camera_pos(&self) -> FixedVec3D {
        let distance = self.scale.factor() * FixedPoint::from(r64(Self::DISTANCE_TO_PIVOT));
        &self.pivot + self.look_vector().to_fixedvec() * &distance
    }

    fn flat_orientation(&self) -> Euler<Rad<f32>> {
        let mut rot = self.orientation();
        rot.x = Rad(0.0); // pitch = 0
        rot
    }

    /// Returns the unit vector for forward motion.
    pub fn forward_vector(&self, config: ForwardAxis3D) -> FVec3D {
        let rot = match config {
            ForwardAxis3D::Camera | ForwardAxis3D::Aligned => self.orientation(),
            ForwardAxis3D::Flat | ForwardAxis3D::FlatAligned => self.flat_orientation(),
        };
        let look = fvec3d_from_euler(Z, rot);
        match config {
            ForwardAxis3D::Camera | ForwardAxis3D::Flat => look,
            ForwardAxis3D::Aligned | ForwardAxis3D::FlatAligned => {
                let ax = look.max_axis(|_ax, component| component.abs());
                FVec3D::unit(ax) * look[ax].signum()
            }
        }
    }
    /// Returns the unit vector for upward motion.
    pub fn up_vector(&self, config: UpAxis3D) -> FVec3D {
        match config {
            UpAxis3D::Camera => fvec3d_from_euler(Y, self.orientation()),
            UpAxis3D::Fixed => FVec3D::unit(Y),
        }
    }
    /// Returns the unit vector for sideways motion.
    pub fn right_vector(&self, config: ForwardAxis3D) -> FVec3D {
        let look = fvec3d_from_euler(X, self.orientation());
        match config {
            ForwardAxis3D::Camera | ForwardAxis3D::Flat => look,
            ForwardAxis3D::Aligned | ForwardAxis3D::FlatAligned => {
                let ax = look.max_axis(|_ax, component| component.abs());
                FVec3D::unit(ax) * look[ax].signum()
            }
        }
    }

    pub fn move_pivot_by_pixels(&mut self, mut delta: FixedVec3D, config: &Config) {
        delta = self.scale().pixels_to_cells(delta);
        let right = self.right_vector(config.ctrl.fwd_axis_3d);
        let up = self.up_vector(config.ctrl.up_axis_3d);
        let fwd = self.forward_vector(config.ctrl.fwd_axis_3d);
        println!("{:?}, {:?}, {:?}", right, up, fwd);
        self.pivot += right.to_fixedvec() * &delta[X];
        self.pivot += up.to_fixedvec() * &delta[Y];
        self.pivot += fwd.to_fixedvec() * &delta[Z];
    }
}

impl Camera<Dim3D> for Camera3D {
    fn pos(&self) -> &FixedVec<Dim3D> {
        &self.pivot
    }
    fn set_pos(&mut self, pos: FixedVec<Dim3D>) {
        self.pivot = pos
    }
    fn snap_pos(&mut self, _config: &Config) {
        self.pivot = self.pivot.floor().0.to_fixedvec() + 0.5;
    }

    fn scale(&self) -> Scale {
        self.scale
    }
    fn set_scale(&mut self, scale: Scale) {
        self.scale = scale.clamp();
    }

    fn lerp(a: &Self, b: &Self, t: R64) -> Self {
        let mut ret = a.clone();

        // The math to interpolate scale and translation is exactly the same as
        // in `Camera2D::lerp()`, so read the comments there.

        let delta_scale_factor = b.scale / a.scale;
        ret.scale_by_factor(delta_scale_factor.powf(t), None);

        let avg_scale = super::average_lerped_scale(a.scale, b.scale);
        let total_pixels_delta = avg_scale.cells_to_pixels(&b.pivot - &a.pivot);

        let zt = super::average_lerped_scale(a.scale, ret.scale);
        let pixels_delta = &total_pixels_delta * &FixedPoint::from(t);
        let cells_delta = zt.pixels_to_cells(pixels_delta);
        ret.pivot += cells_delta;

        // Interpolate rotation by converting to quaternions, doing spherical
        // interpolation, and then converting back to euler angles.
        let ori_a = Quaternion::from(a.orientation);
        let ori_b = Quaternion::from(b.orientation);
        ret.set_orientation(Quaternion::slerp(ori_a, ori_b, t.raw() as f32));

        ret
    }

    fn do_move_command(
        &mut self,
        command: MoveCommand,
        config: &Config,
        cell_transform: &CellTransform,
    ) -> Result<()> {
        match command {
            MoveCommand::GoTo3D {
                mut x,
                mut y,
                mut z,
                yaw,
                pitch,
                relative,
                scaled,
            } => {
                if scaled {
                    x = x.map(|x| self.scale.pixels_to_cells(x));
                    y = y.map(|y| self.scale.pixels_to_cells(y));
                    z = z.map(|y| self.scale.pixels_to_cells(y));
                }
                if relative {
                    self.pivot[X] += x.unwrap_or_default();
                    self.pivot[Y] += y.unwrap_or_default();
                    self.pivot[Z] += z.unwrap_or_default();
                    self.set_orientation(Euler::new(
                        self.orientation().x + pitch.unwrap_or(Rad(0.0)),
                        self.orientation().y + yaw.unwrap_or(Rad(0.0)),
                        Rad(0.0),
                    ));
                } else {
                    if let Some(x) = x {
                        self.pivot[X] = x;
                    }
                    if let Some(y) = y {
                        self.pivot[Y] = y;
                    }
                    if let Some(z) = z {
                        self.pivot[Z] = z;
                    }
                    self.set_orientation(Euler::new(
                        pitch.unwrap_or(self.orientation().x),
                        yaw.unwrap_or(self.orientation().y),
                        Rad(0.0),
                    ));
                }
            }
            MoveCommand::GoToScale(scale) => {
                self.set_scale(scale);
            }

            MoveCommand::Pan { start, end } => {}
            MoveCommand::Orbit { start, end } => {}
            MoveCommand::Scale {
                log2_factor,
                invariant_pos: _,
            } => {
                self.scale_by_log2_factor(r64(log2_factor), None);
            }

            MoveCommand::SnapPos => self.snap_pos(config),
            MoveCommand::SnapScale { invariant_pos: _ } => self.snap_scale(None),

            MoveCommand::GoTo2D { .. } => warn!("Ignoring {:?} in Camera3D", command),
        }

        Ok(())
        // match command {
        //     MoveCommand::SnapPos => {
        //         if config.ctrl.snap_pos_3d {
        //             new_viewport.snap_pos()
        //         }
        //     }
        //     MoveCommand::Scale { log2_factor } => {
        //         new_viewport.scale_by_log2_factor(log2_factor, None)
        //     }
        //     MoveCommand::SetScale { scale } => new_viewport.scale_to(scale, None),
        //     MoveCommand::SnapScale => {
        //         if config.ctrl.snap_scale_3d {
        //             new_viewport.snap_scale(None)
        //         }
        //     }

        //     MoveCommand::Move2D(_) => warn!("Ignoring {:?} in GridView3D", command),

        //     MoveCommand::Move3D(c) => match c {
        //         MoveCommand3D::PanPixels { start, end } => {}
        //         MoveCommand3D::MovePixels(delta) => {
        //             new_viewport.move_pivot_by_pixels(delta, config)
        //         }
        //         MoveCommand3D::SetPos(pos) => new_viewport.set_pos(pos),

        //         MoveCommand3D::RotPixels(_) => {}
        //         MoveCommand3D::SetPitch(_) => {}
        //         MoveCommand3D::SetYaw(_) => {}

        //         MoveCommand3D::Scale {
        //             log2_factor,
        //             invariant_pos,
        //         } => new_viewport.scale_by_factor(log2_factor.exp2(), invariant_pos),
        //         MoveCommand3D::SetScale {
        //             scale,
        //             invariant_pos,
        //         } => new_viewport.scale_to(scale, invariant_pos),
        //         MoveCommand3D::SnapScale { invariant_pos } => {
        //             if config.ctrl.snap_scale_3d {
        //                 new_viewport.snap_scale(invariant_pos)
        //             }
        //         }
        //     },
        // }
    }
}

fn fvec3d_from_euler(axis: Axis, euler: impl Into<Euler<Rad<f32>>>) -> FVec3D {
    let unit_vec = match axis {
        X => cgmath::Vector3::unit_x(),
        Y => cgmath::Vector3::unit_y(),
        Z => cgmath::Vector3::unit_z(),
        _ => panic!("Invalid 3D axis"),
    };
    let v = Matrix3::from(euler.into()) * unit_vec;
    NdVec([r64(v.x as f64), r64(v.y as f64), r64(v.z as f64)])
}
