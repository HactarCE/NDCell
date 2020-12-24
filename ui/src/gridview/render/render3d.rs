//! 3D grid rendering.
//!
//! Currently, only solid colors are supported, however I plan to add custom
//! models and maybe textures in the future.

use anyhow::Result;
use cgmath::Matrix4;
use glium::index::PrimitiveType;
use glium::Surface;

use ndcell_core::prelude::*;
use Axis::{X, Y, Z};

use super::generic::{GenericGridViewRender, GridViewRenderDimension};
use super::shaders;
use super::CellDrawParams;
use crate::gridview::*;
use crate::DISPLAY;

pub(in crate::gridview) type GridViewRender3D<'a> = GenericGridViewRender<'a, RenderDim3D>;

/// Number of cubes to render in each render batch.
const CUBE_BATCH_SIZE: usize = 256;

#[derive(Default)]
pub(in crate::gridview) struct RenderDim3D;
impl GridViewRenderDimension<'_> for RenderDim3D {
    type D = Dim3D;
    type Camera = Camera3D;

    const DEFAULT_COLOR: (f32, f32, f32, f32) = crate::colors::BACKGROUND_3D;
    const DEFAULT_DEPTH: f32 = f32::INFINITY;
}

impl GridViewRender3D<'_> {
    /// Draw an ND-tree to scale on the target.
    pub fn draw_cells(&mut self, params: CellDrawParams<'_, Dim3D>) -> Result<()> {
        let rainbow_cube_matrix: [[f32; 4]; 4] = self.transform.gl_matrix();
        let camera_pos = self.camera.pos().floor().0 - &self.origin;
        let selection_cube_matrix: [[f32; 4]; 4] = (self.transform.projection_transform
            * self.transform.render_cell_transform
            * Matrix4::from_translation(cgmath::vec3(
                camera_pos[X].to_f32().unwrap(),
                camera_pos[Y].to_f32().unwrap(),
                camera_pos[Z].to_f32().unwrap(),
            )))
        .into();

        #[derive(Debug, Copy, Clone)]
        struct Vert {
            pos: [f32; 3],
            color: [f32; 4],
        };
        implement_vertex!(Vert, pos, color);

        let rainbow_cube_verts: Vec<_> = vec![
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

        let selection_cube_verts: Vec<_> = vec![
            ([-0.1, -0.1, -0.1], [1.0, 1.0, 1.0, 0.5]),
            ([-0.1, -0.1, 1.1], [1.0, 1.0, 1.0, 0.5]),
            ([-0.1, 1.1, -0.1], [1.0, 1.0, 1.0, 0.5]),
            ([-0.1, 1.1, 1.1], [1.0, 1.0, 1.0, 0.5]),
            ([1.1, -0.1, -0.1], [1.0, 1.0, 1.0, 0.5]),
            ([1.1, -0.1, 1.1], [1.0, 1.0, 1.0, 0.5]),
            ([1.1, 1.1, -0.1], [1.0, 1.0, 1.0, 0.5]),
            ([1.1, 1.1, 1.1], [1.0, 1.0, 1.0, 0.5]),
        ]
        .into_iter()
        .map(|(pos, color)| Vert { pos, color })
        .collect();

        let cube_vbo = glium::VertexBuffer::new(&**DISPLAY, &rainbow_cube_verts).unwrap();
        let cube_ibo = glium::IndexBuffer::new(
            &**DISPLAY,
            PrimitiveType::TrianglesList,
            &[
                1, 2, 3, 2, 1, 0, // x-
                7, 6, 5, 4, 5, 6, // x+
                0, 1, 4, 5, 4, 1, // y-
                6, 3, 2, 3, 6, 7, // y+
                2, 4, 6, 4, 2, 0, // z-
                7, 5, 3, 1, 3, 5_u16, // z+
            ],
        )
        .unwrap();
        self.params
            .target
            .draw(
                &cube_vbo,
                &cube_ibo,
                &shaders::RGBA,
                &uniform! {
                    matrix: rainbow_cube_matrix,
                },
                &glium::DrawParameters {
                    depth: glium::Depth {
                        test: glium::DepthTest::IfLessOrEqual,
                        write: true,
                        ..glium::Depth::default()
                    },
                    backface_culling: glium::BackfaceCullingMode::CullClockwise,
                    smooth: Some(glium::Smooth::Nicest),
                    ..Default::default()
                },
            )
            .expect("Failed to draw cube");

        cube_vbo.write(&selection_cube_verts);
        self.params
            .target
            .draw(
                &cube_vbo,
                &cube_ibo,
                &shaders::RGBA,
                &uniform! {
                    matrix: selection_cube_matrix,
                },
                &glium::DrawParameters {
                    depth: glium::Depth {
                        test: glium::DepthTest::IfLessOrEqual,
                        write: true,
                        ..glium::Depth::default()
                    },
                    blend: glium::Blend::alpha_blending(),
                    backface_culling: glium::BackfaceCullingMode::CullClockwise,
                    smooth: Some(glium::Smooth::Nicest),
                    ..Default::default()
                },
            )
            .expect("Failed to draw cube");

        Ok(())
    }
}
