//! OpenGL textures.

use glium::framebuffer::SimpleFrameBuffer;
use glium::texture::SrgbTexture2d;

use super::resizing::Resizing;
use crate::DISPLAY;

pub struct TextureCache {
    cells: Resizing<SrgbTexture2d>,
}
impl Default for TextureCache {
    fn default() -> Self {
        Self {
            cells: Resizing::with_generator(|w, h| {
                SrgbTexture2d::empty(&**DISPLAY, w, h).expect("Failed to create texture")
            }),
        }
    }
}
impl TextureCache {
    /// Returns the 1:1 render cells texture, along with a framebuffer and the
    /// viewport rectangle within it.
    pub fn cells<'a>(
        &'a mut self,
        width: u32,
        height: u32,
    ) -> (&'a SrgbTexture2d, SimpleFrameBuffer<'a>, glium::Rect) {
        self.cells.set_min_size(width, height);

        let texture = self.cells.unwrap();
        let fbo =
            SimpleFrameBuffer::new(&**DISPLAY, texture).expect("Failed to create frame buffer");
        let viewport = glium::Rect {
            left: 0,
            bottom: 0,
            width,
            height,
        };

        (texture, fbo, viewport)
    }
}
