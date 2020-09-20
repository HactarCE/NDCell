//! 3D grid rendering.
//!
//! Currently, only solid colors are supported, however I plan to add custom
//! models and maybe textures in the future.

use cgmath::prelude::*;
use cgmath::{Deg, Matrix4};
use glium::index::PrimitiveType;
use glium::Surface;

mod shaders;

use super::*;
use crate::DISPLAY;

/// Number of cubes to render in each render batch.
const CUBE_BATCH_SIZE: usize = 256;

/// Vertical field-of-view.
const FOV: Deg<f32> = Deg(60.0);
/// Near clipping plane.
const NEAR_PLANE: f32 = 1.0 / 256.0;
/// Far clipping plane.
const FAR_PLANE: f32 = 1024.0;

#[derive(Default)]
pub struct RenderCache {}

pub struct RenderInProgress<'a> {
    octree: NdTree3D,
    /// Camera to render the scene from.
    camera: &'a Camera3D,
    /// Target to render to.
    target: &'a mut glium::Frame,
    /// Combined view and perspective matrix.
    matrix: Matrix4<f32>,
}
impl<'a> RenderInProgress<'a> {
    pub fn new(
        g: &'a GridView3D,
        _node_cache: &'a NodeCache<Dim3D>,
        target: &'a mut glium::Frame,
    ) -> Self {
        target.clear_depth(f32::INFINITY);
        let (target_w, target_h) = target.get_dimensions();
        let aspect_ratio = target_w as f32 / target_h as f32;
        let perspective_matrix = cgmath::PerspectiveFov {
            fovy: FOV.into(),
            aspect: aspect_ratio,
            near: NEAR_PLANE,
            far: FAR_PLANE,
        };

        let camera = &g.camera;
        // let view_matrix = Matrix4::from_translation(cgmath::Vector3::new(0.0, 0.0, -5.0));
        let view_matrix = cgmath::Decomposed {
            scale: 1.0,
            rot: cgmath::Basis3::from_angle_x(camera.pitch())
                * cgmath::Basis3::from_angle_y(camera.yaw()),
            disp: cgmath::Vector3::new(0.0, 0.0, -camera.log2_distance.exp2() as f32),
        };

        Self {
            octree: g.automaton.projected_tree(),
            camera,
            target,
            matrix: Matrix4::from(perspective_matrix) * Matrix4::from(view_matrix),
        }
    }

    pub fn draw_cells(&mut self) {
        let matrix: [[f32; 4]; 4] = self.matrix.into();

        #[derive(Debug, Copy, Clone)]
        struct Vert {
            pos: [f32; 3],
            color: [f32; 4],
        };
        implement_vertex!(Vert, pos, color);

        let cube: Vec<_> = vec![
            ([0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 1.0]),
            ([0.0, 0.0, 1.0], [0.0, 0.0, 1.0, 1.0]),
            ([0.0, 1.0, 0.0], [0.0, 1.0, 0.0, 1.0]),
            ([0.0, 1.0, 1.0], [0.0, 1.0, 1.0, 1.0]),
            ([1.0, 0.0, 0.0], [1.0, 0.0, 0.0, 1.0]),
            ([1.0, 0.0, 1.0], [1.0, 0.0, 1.0, 1.0]),
            ([1.0, 1.0, 0.0], [1.0, 1.0, 0.0, 1.0]),
            ([1.0, 1.0, 1.0], [1.0, 1.0, 1.0, 1.0]),
        ]
        .into_iter()
        .map(|(pos, color)| Vert { pos, color })
        .collect();

        let cube_vbo = glium::VertexBuffer::new(&**DISPLAY, &cube).unwrap();
        let cube_ibo = glium::IndexBuffer::new(
            &**DISPLAY,
            PrimitiveType::TrianglesList,
            &[
                0, 1, 2, 3, 2, 1, // x-
                7, 6, 5, 4, 5, 6, // x+
                0, 1, 4, 5, 4, 1, // y-
                7, 6, 3, 2, 3, 6, // y+
                0, 2, 4, 6, 4, 2, // z-
                7, 5, 3, 1, 3, 5_u16, // z+
            ],
        )
        .unwrap();
        self.target.clear_color_srgb(0.5, 0.5, 0.5, 1.0);
        self.target
            .draw(
                &cube_vbo,
                &cube_ibo,
                &shaders::POINTS,
                &uniform! {
                    matrix: matrix,
                },
                &glium::DrawParameters {
                    smooth: Some(glium::Smooth::Nicest),
                    depth: glium::Depth {
                        test: glium::DepthTest::IfLessOrEqual,
                        write: true,
                        ..glium::Depth::default()
                    },
                    ..Default::default()
                },
            )
            .expect("Failed to draw cube");
    }
}
