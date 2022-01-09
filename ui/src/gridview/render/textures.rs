//! OpenGL textures.

use glium::framebuffer::SimpleFrameBuffer;
use glium::texture::{MipmapsOption, SrgbTexture2d, Texture2d, UncompressedFloatFormat};

use super::resizing::Resizing;
use crate::DISPLAY;

type ResizingSrgbTexture = Resizing<SrgbTexture2d>;
type ResizingTexture = Resizing<Texture2d>;

impl Default for ResizingSrgbTexture {
    fn default() -> Self {
        Self::with_generator(|w, h| {
            SrgbTexture2d::empty(&**DISPLAY, w, h).expect("Failed to create texture")
        })
    }
}
impl Default for ResizingTexture {
    fn default() -> Self {
        Self::with_generator(|w, h| {
            Texture2d::empty_with_format(
                &**DISPLAY,
                UncompressedFloatFormat::F32,
                MipmapsOption::NoMipmap,
                w,
                h,
            )
            .expect("Failed to create texture")
        })
    }
}

#[derive(Default)]
pub struct TextureCache {
    cells_2d: ResizingSrgbTexture,
    octree_init_t: ResizingTexture,
}
impl TextureCache {
    /// Returns the 1:1 render cells texture, along with a framebuffer and the
    /// viewport rectangle within it.
    pub fn cells_2d(
        &mut self,
        width: u32,
        height: u32,
    ) -> (&SrgbTexture2d, SimpleFrameBuffer<'_>, glium::Rect) {
        self.cells_2d.set_min_size(width, height);

        let texture = self.cells_2d.unwrap();
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

    /// Returns the octree depth texture, along with a framebuffer and the
    /// viewport rectangle within it.
    pub fn octree_init_t(
        &mut self,
        width: u32,
        height: u32,
    ) -> (&Texture2d, SimpleFrameBuffer<'_>, glium::Rect) {
        self.octree_init_t.set_min_size(width, height);

        let texture = self.octree_init_t.unwrap();
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
