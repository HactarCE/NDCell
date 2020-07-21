use glium::index::{IndexBuffer, IndexBufferSlice, PrimitiveType};
use itertools::Itertools;
use send_wrapper::SendWrapper;

use super::CELL_OVERLAY_BATCH_SIZE;
use crate::DISPLAY;

lazy_static! {
    static ref RECT_INDICES: SendWrapper<IndexBuffer<u32>> = SendWrapper::new(
        IndexBuffer::immutable(
            &**DISPLAY,
            PrimitiveType::TrianglesList,
            &(0..CELL_OVERLAY_BATCH_SIZE as u32)
                .flat_map(|i| [0, 1, 2, 3, 2, 1].iter().map(move |&j| 4 * i + j))
                .collect_vec(),
        )
        .expect("Failed to create index buffer")
    );
}

pub fn rect_indices(rect_count: usize) -> IndexBufferSlice<'static, u32> {
    RECT_INDICES.slice(0..(6 * rect_count)).unwrap()
}
