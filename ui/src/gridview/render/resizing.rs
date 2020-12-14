//! OpenGL resources with a 2D size that are expensive to acquire, so we double
//! their size when they are too small.

pub struct Resizing<T> {
    cached: Option<T>,
    current_size: Option<(u32, u32)>,
    generator: fn(u32, u32) -> T,
}
impl<T> Resizing<T> {
    pub fn with_generator(generator: fn(u32, u32) -> T) -> Self {
        Self {
            cached: None,
            current_size: None,
            generator,
        }
    }
    pub fn reset(&mut self) {
        self.cached = None;
        self.current_size = None;
    }

    pub fn unwrap(&self) -> &T {
        self.cached.as_ref().unwrap()
    }
    pub fn unwrap_size(&self) -> (u32, u32) {
        self.current_size.unwrap()
    }

    pub fn set_exact_size(&mut self, w: u32, h: u32) {
        if self.current_size != Some((w, h)) {
            self.cached = Some((self.generator)(w, h));
            self.current_size = Some((w, h));
        }
    }
    pub fn set_min_size(&mut self, w: u32, h: u32) {
        // Round up to next power of two.
        let mut w = w.next_power_of_two();
        let mut h = h.next_power_of_two();
        if let Some((current_w, current_h)) = self.current_size {
            if current_w >= w && current_h >= h {
                // Use the current size.
                w = current_w;
                h = current_h;
            }
        }
        self.set_exact_size(w, h);
    }
}
