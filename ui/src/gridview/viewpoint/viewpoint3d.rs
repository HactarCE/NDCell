use anyhow::Result;
use cgmath::prelude::*;
use cgmath::{Basis3, Decomposed, Deg, Matrix4};
use log::warn;

use ndcell_core::prelude::*;
use Axis::{X, Y, Z};

use super::{
    CellTransform3D, DragHandler, DragOutcome, ProjectionType, Scale, Viewpoint, MIN_TARGET_SIZE,
};
use crate::commands::{ViewCommand, ViewDragCommand};
use crate::config::{ForwardAxis3D, UpAxis3D};
use crate::CONFIG;

#[derive(Debug, Clone, PartialEq)]
pub struct Viewpoint3D {
    /// Width and height of the render target.
    target_dimensions: (u32, u32),
    /// Display scaling factor.
    dpi: f32,

    /// Scale.
    scale: Scale,
    /// Yaw (-180..+180).
    yaw: Deg<f32>,
    /// Pitch (-90..+90).
    pitch: Deg<f32>,
    /// Cell coordinates at the center of the viewpoint.
    pivot: FixedVec3D,
}

impl Default for Viewpoint3D {
    fn default() -> Self {
        Self {
            target_dimensions: (MIN_TARGET_SIZE, MIN_TARGET_SIZE),
            dpi: 1.0,

            scale: Scale::default(),
            yaw: Self::DEFAULT_YAW,
            pitch: Self::DEFAULT_PITCH,
            pivot: FixedVec3D::repeat(r64(0.5).into()),
        }
    }
}

impl Viewpoint3D {
    /// Number of scaled units away from the pivot to position the camera.
    pub const DISTANCE_TO_PIVOT: f32 = 512.0;

    pub const DEFAULT_PITCH: Deg<f32> = Deg(30.0);
    pub const DEFAULT_YAW: Deg<f32> = Deg(20.0);

    /// Radius of visible cells, measured in "scaled units". (See `Scale` docs.)
    pub const VIEW_RADIUS: f32 = 5000.0;

    /// Returns the yaw of the camera.
    pub fn yaw(&self) -> Deg<f32> {
        self.yaw
    }
    /// Sets the yaw of the camera.
    pub fn set_yaw(&mut self, yaw: Deg<f32>) {
        // Clamp yaw to -180..+180.
        self.yaw = yaw.normalize_signed();
    }
    /// Returns the pitch of the camera.
    pub fn pitch(&self) -> Deg<f32> {
        self.pitch
    }
    /// Sets the pitch of the camera.
    pub fn set_pitch(&mut self, pitch: Deg<f32>) {
        // Clamp pitch to -90..+90.
        self.pitch = pitch.normalize_signed();
        if self.pitch > Deg(90.0) {
            self.pitch = Deg(90.0);
        }
        if self.pitch < Deg(-90.0) {
            self.pitch = Deg(-90.0);
        }
    }

    /// Returns the orientation of the camera.
    pub fn orientation(&self) -> Basis3<f32> {
        Basis3::from_angle_x(self.pitch) * Basis3::from_angle_y(self.yaw)
    }
    /// Returns the orientation of the camera projected to the horizontal plane.
    fn flat_orientation(&self) -> Basis3<f32> {
        // Pitch = 0; only apply yaw
        Basis3::from_angle_y(self.yaw)
    }

    /// Returns the unit vector along which the camera is looking.
    pub fn look_vector(&self) -> FVec3D {
        fvec3d_from_basis3(Z, self.orientation())
    }
    /// Returns the position of the camera (not the pivot).
    pub fn camera_pos(&self) -> FixedVec3D {
        let distance =
            self.scale.inv_factor() * FixedPoint::from(r64(Self::DISTANCE_TO_PIVOT as f64));
        &self.pivot + self.look_vector().to_fixedvec() * &distance
    }

    /// Returns the unit vector for forward motion.
    pub fn forward_vector(&self, config: ForwardAxis3D) -> FVec3D {
        let rot = match config {
            ForwardAxis3D::Camera | ForwardAxis3D::Aligned => self.orientation(),
            ForwardAxis3D::Flat | ForwardAxis3D::FlatAligned => self.flat_orientation(),
        };
        let look = fvec3d_from_basis3(Z, rot);
        match config {
            ForwardAxis3D::Camera | ForwardAxis3D::Flat => look,
            ForwardAxis3D::Aligned | ForwardAxis3D::FlatAligned => {
                let ax = look.abs().max_axis();
                FVec3D::unit(ax) * look[ax].signum()
            }
        }
    }
    /// Returns the unit vector for upward motion.
    pub fn up_vector(&self, config: UpAxis3D) -> FVec3D {
        match config {
            UpAxis3D::Camera => fvec3d_from_basis3(Y, self.orientation()),
            UpAxis3D::Fixed => FVec3D::unit(Y),
        }
    }
    /// Returns the unit vector for sideways motion.
    pub fn right_vector(&self, config: ForwardAxis3D) -> FVec3D {
        let look = fvec3d_from_basis3(X, self.orientation());
        match config {
            ForwardAxis3D::Camera | ForwardAxis3D::Flat => look,
            ForwardAxis3D::Aligned | ForwardAxis3D::FlatAligned => {
                let ax = look.abs().max_axis();
                FVec3D::unit(ax) * look[ax].signum()
            }
        }
    }
}

impl Viewpoint<Dim3D> for Viewpoint3D {
    fn target_dimensions(&self) -> (u32, u32) {
        self.target_dimensions
    }
    fn set_target_dimensions(&mut self, (target_w, target_h): (u32, u32)) {
        self.target_dimensions = (
            std::cmp::max(MIN_TARGET_SIZE, target_w),
            std::cmp::max(MIN_TARGET_SIZE, target_h),
        );
    }
    fn dpi(&self) -> f32 {
        self.dpi
    }
    fn set_dpi(&mut self, dpi: f32) {
        self.dpi = dpi;
    }

    fn center(&self) -> &FixedVec<Dim3D> {
        &self.pivot
    }
    fn set_pos(&mut self, pos: FixedVec<Dim3D>) {
        self.pivot = pos
    }
    fn snap_pos(&mut self) {
        self.pivot = self.pivot.round().to_fixedvec();
    }

    fn scale(&self) -> Scale {
        self.scale
    }
    fn set_scale(&mut self, scale: Scale) {
        self.scale = scale.clamp();
    }

    fn _extra_distance_dimension(a: &Self, b: &Self) -> FixedPoint {
        let pitch_diff = (a.pitch() - b.pitch()).normalize_signed().0;
        let yaw_diff = (a.yaw() - b.yaw()).normalize_signed().0;
        let squared_pitch = pitch_diff * pitch_diff;
        let squared_yaw = yaw_diff * yaw_diff;
        FixedPoint::from(r64(
            super::ROT_DEGREES_PER_2X_SCALE * (squared_pitch + squared_yaw).sqrt() as f64
        ))
    }

    fn lerp(a: &Self, b: &Self, t: R64) -> Self {
        let mut ret = a.clone();

        // The math to interpolate scale and translation is exactly the same as
        // in `Viewpoint2D::lerp()`, so read the comments there.

        let delta_scale_factor = b.scale / a.scale;
        ret.scale_by_factor(delta_scale_factor.powf(t), None);

        let avg_scale = super::average_lerped_scale(a.scale, b.scale);
        let total_pixels_delta = avg_scale.cells_to_units(&b.pivot - &a.pivot);

        let zt = super::average_lerped_scale(a.scale, ret.scale);
        let pixels_delta = &total_pixels_delta * &FixedPoint::from(t);
        let cells_delta = zt.units_to_cells(pixels_delta);
        ret.pivot += cells_delta;

        // Interpolate using Euler angles, not proper spherical interpolation.
        // This way there's never any roll.
        let yaw_a = a.yaw();
        let yaw_b = b.yaw();
        let yaw_diff = (yaw_b - yaw_a).normalize_signed();
        ret.set_yaw(yaw_a + yaw_diff * t.raw() as f32);
        let pitch_a = a.pitch();
        let pitch_b = b.pitch();
        let pitch_diff = pitch_b - pitch_a;
        ret.set_pitch(pitch_a + pitch_diff * t.raw() as f32);

        ret
    }

    fn cell_transform(&self) -> CellTransform3D {
        let (render_cell_layer, render_cell_scale) = self.render_cell_layer_and_scale();
        let camera_transform = Matrix4::from(Decomposed {
            scale: 1.0,
            rot: self.orientation(),
            disp: cgmath::vec3(0.0, 0.0, -Self::DISTANCE_TO_PIVOT),
        });

        CellTransform3D::new(
            self.pivot.clone(),
            render_cell_layer,
            render_cell_scale,
            camera_transform,
            ProjectionType::Perspective,
            self.target_dimensions(),
        )
    }

    fn global_visible_rect(&self) -> BigRect3D {
        let (render_cell_layer, render_cell_scale) = self.render_cell_layer_and_scale();
        let pivot = self.center().floor();
        let inv_render_cell_scale_factor = render_cell_scale.inv_factor().to_f32().unwrap();
        let render_cell_radius = 0.5 + Self::VIEW_RADIUS * inv_render_cell_scale_factor;
        let render_cell_radius = render_cell_radius.ceil() as usize;
        let cell_radius = BigInt::from(render_cell_radius) << render_cell_layer.to_u32();
        render_cell_layer.round_rect(&BigRect3D::centered(pivot, &cell_radius))
    }

    fn do_view_command(&mut self, command: ViewCommand) -> Result<Option<DragHandler<Self>>> {
        let config = CONFIG.lock();

        match command {
            ViewCommand::Drag(c, cursor_start) => match c {
                ViewDragCommand::Orbit => {
                    let orbit_factor = r64(config.ctrl.mouse_orbit_speed / config.gfx.dpi);
                    let old_yaw = self.yaw();
                    let old_pitch = self.pitch();
                    Ok(Some(Box::new(move |this, cursor_end| {
                        let delta = (cursor_end - cursor_start) * orbit_factor;
                        this.set_yaw(old_yaw + Deg(delta[X].raw() as f32));
                        this.set_pitch(old_pitch + Deg(delta[Y].raw() as f32));
                        Ok(DragOutcome::Continue)
                    })))
                }

                ViewDragCommand::Pan => {
                    let z = Self::DISTANCE_TO_PIVOT;
                    let start = self.cell_transform().pixel_to_global_pos(cursor_start, z);
                    Ok(Some(Box::new(move |this, cursor_end| {
                        let end = this.cell_transform().pixel_to_global_pos(cursor_end, z);
                        this.pivot += &start - &end;
                        Ok(DragOutcome::Continue)
                    })))
                }
                ViewDragCommand::PanAligned => {
                    todo!("pan aligned");
                }
                ViewDragCommand::PanAlignedVertical => {
                    todo!("pan aligned vertical");
                }
                ViewDragCommand::PanHorizontal => {
                    let y = self.pivot[Y].clone();
                    let start = self
                        .cell_transform()
                        .pixel_to_global_pos_in_plane(cursor_start, (Y, &y));
                    Ok(Some(Box::new(move |this, cursor_end| {
                        let end = this
                            .cell_transform()
                            .pixel_to_global_pos_in_plane(cursor_end, (Y, &y));
                        if let (Some(start), Some(end)) = (&start, &end) {
                            this.pivot += start - end;
                        }
                        Ok(DragOutcome::Continue)
                    })))
                }

                ViewDragCommand::Scale => todo!("Scale using click & drag"),
            },

            ViewCommand::GoTo2D { .. } => {
                warn!("Ignoring {:?} in Viewpoint3D", command);
                Ok(None)
            }
            ViewCommand::GoTo3D {
                mut x,
                mut y,
                mut z,
                yaw,
                pitch,
                relative,
                scaled,
            } => {
                if scaled {
                    x = x.map(|x| self.scale.units_to_cells(x));
                    y = y.map(|y| self.scale.units_to_cells(y));
                    z = z.map(|y| self.scale.units_to_cells(y));
                }
                if relative {
                    let right = self.right_vector(config.ctrl.fwd_axis_3d);
                    let up = self.up_vector(config.ctrl.up_axis_3d);
                    let fwd = self.forward_vector(config.ctrl.fwd_axis_3d);
                    self.pivot += right.to_fixedvec() * x.unwrap_or_default();
                    self.pivot += up.to_fixedvec() * y.unwrap_or_default();
                    self.pivot += fwd.to_fixedvec() * z.unwrap_or_default();
                    self.set_yaw(self.yaw() + pitch.unwrap_or(Deg(0.0)));
                    self.set_pitch(self.pitch() + yaw.unwrap_or(Deg(0.0)));
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
                    if let Some(yaw) = yaw {
                        self.set_yaw(yaw);
                    }
                    if let Some(pitch) = pitch {
                        self.set_pitch(pitch);
                    }
                }
                Ok(None)
            }
            ViewCommand::GoToScale(scale) => {
                self.set_scale(scale);
                Ok(None)
            }

            ViewCommand::Scale {
                log2_factor,
                invariant_pos: _,
            } => {
                self.scale_by_log2_factor(r64(log2_factor), None);
                Ok(None)
            }

            ViewCommand::SnapPos => {
                self.snap_pos();
                Ok(None)
            }
            ViewCommand::SnapScale { invariant_pos: _ } => {
                self.snap_scale(None);
                Ok(None)
            }

            ViewCommand::FitView => {
                todo!("fit view 3D");
            }
        }
    }
}

fn fvec3d_from_basis3(axis: Axis, rot: impl Rotation3<Scalar = f32>) -> FVec3D {
    let unit_vec = match axis {
        X => cgmath::Vector3::unit_x(),
        Y => cgmath::Vector3::unit_y(),
        Z => cgmath::Vector3::unit_z(),
        _ => panic!("Invalid 3D axis"),
    };
    let v = rot.invert().rotate_vector(unit_vec);
    NdVec([r64(v.x as f64), r64(v.y as f64), r64(v.z as f64)])
}
