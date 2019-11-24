use super::*;

/// An iterator over a hyperrectangle of cell positions.
#[derive(Debug, Copy, Clone)]
pub struct NdRectIter<D: Dim> {
    rect: NdRect<D>,
    next: NdVec<D>,
    done: bool,
}

impl<D: Dim> From<NdRect<D>> for NdRectIter<D> {
    fn from(rect: NdRect<D>) -> Self {
        Self {
            rect,
            next: rect.min(),
            done: false,
        }
    }
}

impl<D: Dim> Iterator for NdRectIter<D> {
    type Item = NdVec<D>;
    fn next(&mut self) -> Option<NdVec<D>> {
        if self.done {
            None
        } else {
            let ret = self.next;
            for &ax in D::axes() {
                self.next[ax] += 1;
                if self.next[ax] > self.rect.max[ax] {
                    self.next[ax] = self.rect.min[ax];
                } else {
                    return Some(ret);
                }
            }
            self.done = true;
            Some(ret)
        }
    }
}
