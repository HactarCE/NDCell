//! OpenGL pixel buffer objects to detect what the mouse is hovering over.

use glium::framebuffer::{DepthRenderBuffer, SimpleFrameBuffer};
use glium::texture::pixel_buffer::PixelBuffer;
use glium::texture::{DepthFormat, MipmapsOption, UncompressedUintFormat, UnsignedTexture2d};
use glium::Surface;

use super::resizing::Resizing;
use crate::DISPLAY;

/// Pixel buffer object that tells which target the cursor is hovering over.
pub struct MousePicker {
    pbo: PixelBuffer<u32>,
    attachments: Resizing<(UnsignedTexture2d, DepthRenderBuffer)>,
}
impl Default for MousePicker {
    fn default() -> Self {
        Self {
            pbo: PixelBuffer::new_empty(&**DISPLAY, 1),
            attachments: Resizing::with_generator(|w, h| {
                let texture = UnsignedTexture2d::empty_with_format(
                    &**DISPLAY,
                    UncompressedUintFormat::U32,
                    MipmapsOption::NoMipmap,
                    w,
                    h,
                )
                .expect("Failed to create pixel buffer texture");

                let depth = DepthRenderBuffer::new(&**DISPLAY, DepthFormat::F32, w, h)
                    .expect("Failed to create pixel buffer depth buffer");

                (texture, depth)
            }),
        }
    }
}
impl MousePicker {
    pub fn init(&mut self, (target_w, target_h): (u32, u32)) {
        self.attachments.set_min_size(target_w, target_h);
        let (mut fbo, _viewport) = self.fbo();
        fbo.clear_color_and_depth((0.0, 0.0, 0.0, 0.0), 0.0);
    }

    pub fn fbo<'a>(&'a mut self) -> (SimpleFrameBuffer<'a>, glium::Rect) {
        let (target_w, target_h) = self.attachments.desired_size().unwrap();
        let (texture, depth) = self.attachments.unwrap();

        let fbo = SimpleFrameBuffer::with_depth_buffer(&**DISPLAY, texture, depth)
            .expect("Failed to create frame buffer");
        let viewport = glium::Rect {
            left: 0,
            bottom: 0,
            width: target_w,
            height: target_h,
        };

        (fbo, viewport)
    }

    pub fn get_pixel(&self, (cursor_x, cursor_y): (u32, u32)) -> u32 {
        let (target_w, target_h) = self.attachments.desired_size().unwrap();
        let (texture, _depth) = self.attachments.unwrap();

        let single_pixel_rect = glium::Rect {
            left: cursor_x,
            bottom: target_h.saturating_sub(cursor_y + 1),
            width: 1,
            height: 1,
        };

        if single_pixel_rect.left < target_w && single_pixel_rect.bottom < target_h {
            texture
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
