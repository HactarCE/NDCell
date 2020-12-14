//! OpenGL resources with a 2D size that are expensive to acquire, so we double
//! their size when they are too small.

pub struct Resizing<T> {
    generator: fn(u32, u32) -> T,
    cached: Option<ResizingInner<T>>,
}
impl<T> Resizing<T> {
    pub fn with_generator(generator: fn(u32, u32) -> T) -> Self {
        Self {
            generator,
            cached: None,
        }
    }
    pub fn reset(&mut self) {
        self.cached = None;
    }

    pub fn unwrap(&self) -> &T {
        &self.cached.as_ref().unwrap().value
    }
    pub fn actual_size(&self) -> Option<(u32, u32)> {
        self.cached.as_ref().map(|cached| cached.actual_size)
    }
    pub fn desired_size(&self) -> Option<(u32, u32)> {
        self.cached.as_ref().map(|cached| cached.desired_size)
    }

    pub fn set_exact_size(&mut self, w: u32, h: u32) {
        if self.actual_size() == Some((w, h)) {
            self.cached.as_mut().unwrap().desired_size = (w, h);
        } else {
            self.cached = Some(ResizingInner {
                value: (self.generator)(w, h),
                actual_size: (w, h),
                desired_size: (w, h),
            });
        }
    }
    pub fn set_min_size(&mut self, w: u32, h: u32) {
        // Round up ("inflate") to next power of two.
        let mut inflated_w = w.next_power_of_two();
        let mut inflated_h = h.next_power_of_two();
        if let Some((current_w, current_h)) = self.actual_size() {
            if current_w >= inflated_w && current_h >= inflated_h {
                // Use the current size, which is larger than the desired size.
                inflated_w = current_w;
                inflated_h = current_h;
            }
        }
        self.set_exact_size(inflated_w, inflated_h);
        self.cached.as_mut().unwrap().desired_size = (w, h);
    }
}

struct ResizingInner<T> {
    value: T,
    actual_size: (u32, u32),
    desired_size: (u32, u32),
}
