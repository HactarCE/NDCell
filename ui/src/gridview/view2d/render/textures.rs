use glium::framebuffer::SimpleFrameBuffer;
use glium::texture::srgb_texture2d::SrgbTexture2d;
use send_wrapper::SendWrapper;
use std::cell::RefCell;

use ndcell_core::ndvec::{FVec2D, NdVec};
use ndcell_core::num::r64;

use crate::DISPLAY;

#[derive(Default)]
pub struct CachedSrgbTexture2d {
    cached: Option<SrgbTexture2d>,
    current_size: Option<(u32, u32)>,
}
impl CachedSrgbTexture2d {
    pub fn at_min_size(&mut self, w: u32, h: u32) -> (&SrgbTexture2d, SimpleFrameBuffer, FVec2D) {
        let mut real_w = w.next_power_of_two();
        let mut real_h = h.next_power_of_two();
        if let Some((current_w, current_h)) = self.current_size {
            if current_w >= w && current_h >= h {
                real_w = current_w;
                real_h = current_h;
            }
        }
        self.set_size(real_w, real_h);
        let fract = NdVec([r64(w as f64 / real_w as f64), r64(h as f64 / real_h as f64)]);
        (self.unwrap(), self.make_fbo(), fract)
    }
    fn set_size(&mut self, w: u32, h: u32) {
        if self.current_size != Some((w, h)) {
            self.cached =
                Some(SrgbTexture2d::empty(&**DISPLAY, w, h).expect("Failed to create texture"));
            self.current_size = Some((w, h));
        }
    }
    pub fn reset(&mut self) {
        *self = Self::default();
    }
    pub fn unwrap(&self) -> &SrgbTexture2d {
        self.cached.as_ref().unwrap()
    }
    pub fn make_fbo(&self) -> SimpleFrameBuffer {
        SimpleFrameBuffer::new(&**DISPLAY, self.unwrap()).expect("Failed to create frame buffer")
    }
}

#[derive(Default)]
pub struct TextureCache {
    pub unscaled_cells: CachedSrgbTexture2d,
    pub scaled_cells: CachedSrgbTexture2d,
}

lazy_static! {
    pub static ref CACHE: SendWrapper<RefCell<TextureCache>> =
        SendWrapper::new(RefCell::new(TextureCache::default()));
}
