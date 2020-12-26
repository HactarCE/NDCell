//! OpenGL index buffer objects.

use glium::index::{IndexBuffer, IndexBufferSlice, PrimitiveType};
use itertools::Itertools;

use super::consts::QUAD_BATCH_SIZE;
use crate::DISPLAY;

pub struct IboCache {
    quad_indices: IndexBuffer<u32>,
}
impl Default for IboCache {
    fn default() -> Self {
        Self {
            quad_indices: IndexBuffer::immutable(
                &**DISPLAY,
                PrimitiveType::TrianglesList,
                &(0..QUAD_BATCH_SIZE as u32)
                    .flat_map(|i| [0, 1, 2, 3, 2, 1].iter().map(move |&j| 4 * i + j))
                    .collect_vec(),
            )
            .expect("Failed to create index buffer"),
        }
    }
}
impl IboCache {
    pub fn quad_indices<'a>(&'a self, quad_count: usize) -> IndexBufferSlice<'a, u32> {
        self.quad_indices.slice(0..(6 * quad_count)).unwrap()
    }
}
