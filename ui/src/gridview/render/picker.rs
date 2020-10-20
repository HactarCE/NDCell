//! OpenGL pixel buffer objects to detect what the mouse is hovering over.

use glium::framebuffer::{DepthRenderBuffer, SimpleFrameBuffer};
use glium::texture::pixel_buffer::PixelBuffer;
use glium::texture::{DepthFormat, MipmapsOption, UncompressedUintFormat, UnsignedTexture2d};

use crate::DISPLAY;

#[derive(Default)]
pub struct CachedMousePicker {
    cached: Option<MousePicker>,
    current_size: Option<(u32, u32)>,
}
impl CachedMousePicker {
    pub fn at_size(&mut self, (w, h): (u32, u32)) -> &MousePicker {
        if self.current_size != Some((w, h)) {
            self.cached = Some(MousePicker {
                pbo: PixelBuffer::new_empty(&**DISPLAY, 1),
                texture: UnsignedTexture2d::empty_with_format(
                    &**DISPLAY,
                    UncompressedUintFormat::U32,
                    MipmapsOption::NoMipmap,
                    w,
                    h,
                )
                .expect("Failed to create pixel buffer texture"),
                depth: DepthRenderBuffer::new(&**DISPLAY, DepthFormat::F32, w, h)
                    .expect("Failed to create pixel buffer depth buffer"),
            });
        }
        self.unwrap()
    }
    pub fn reset(&mut self) {
        *self = Self::default()
    }
    pub fn try_unwrap(&self) -> Option<&MousePicker> {
        self.cached.as_ref()
    }
    pub fn unwrap(&self) -> &MousePicker {
        self.try_unwrap().unwrap()
    }
}

pub struct MousePicker {
    pbo: PixelBuffer<u32>,
    texture: UnsignedTexture2d,
    depth: DepthRenderBuffer,
}
impl MousePicker {
    pub fn make_fbo<'a>(&'a self) -> SimpleFrameBuffer<'a> {
        SimpleFrameBuffer::with_depth_buffer(&**DISPLAY, &self.texture, &self.depth)
            .expect("Failed to create frame buffer")
    }
    pub fn get_pixel(&self, (cursor_x, cursor_y): (u32, u32)) -> u32 {
        let (tex_w, tex_h) = self.texture.dimensions();

        let single_pixel_rect = glium::Rect {
            left: cursor_x,
            bottom: tex_h.saturating_sub(cursor_y + 1),
            width: 1,
            height: 1,
        };

        if single_pixel_rect.left < tex_w && single_pixel_rect.bottom < tex_h {
            self.texture
                .main_level()
                .first_layer()
                .into_image(None)
                .unwrap()
                .raw_read::<Vec<Vec<u32>>, u32>(&single_pixel_rect)[0][0]
        } else {
            0
        }
    }
}
