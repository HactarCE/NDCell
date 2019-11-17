use std::cmp;

use super::*;

/// An N-dimensional hyperrectangle.
#[derive(Debug, Copy, Clone)]
pub struct NdRect<D: Dim> {
    /// One corner of the hyperrectangle.
    pub a: NdVec<D>,
    /// The other corner of the hyperrectangle.
    pub b: NdVec<D>,
}

/// A 1D hyperrectangle (a linear range).
pub type Rect1D = NdRect<Dim1D>;
/// A 2D hyperrectangle (a rectangle).
pub type Rect2D = NdRect<Dim2D>;
/// A 3D hyperrectangle (a rectangular prism).
pub type Rect3D = NdRect<Dim3D>;
/// A 4D hyperrectangle (a rectangular duoprism).
pub type Rect4D = NdRect<Dim4D>;
/// A 5D hyperrectangle.
pub type Rect5D = NdRect<Dim5D>;
/// A 6D hyperrectangle.
pub type Rect6D = NdRect<Dim6D>;

impl<D: Dim> NdRect<D> {
    pub fn span(a: NdVec<D>, b: NdVec<D>) -> Self {
        Self { a, b }
    }
    pub fn single_cell(pos: NdVec<D>) -> Self {
        Self::span(pos, pos)
    }
    pub fn centered(center: NdVec<D>, radius: usize) -> Self {
        Self {
            a: center - radius as isize,
            b: center + radius as isize,
        }
    }
    /// Returns the NdRect that describes a Moore neighborhood centered at the
    /// origin.
    pub fn moore(radius: usize) -> Self {
        Self::centered(NdVec::origin(), radius)
    }

    /// Returns the first corner of this NdRect.
    pub fn a(&self) -> NdVec<D> {
        self.a
    }
    /// Returns the second corner of this NdRect.
    pub fn b(&self) -> NdVec<D> {
        self.b
    }
    /// Returns the minimum (most negative) corner of this NdRect.
    pub fn min(&self) -> NdVec<D> {
        let mut ret = NdVec::origin();
        for ax in D::axes() {
            ret[ax] = cmp::min(self.a[ax], self.b[ax]);
        }
        ret
    }
    /// Returns the maximum (most positive) corner of this NdRect.
    pub fn max(&self) -> NdVec<D> {
        let mut ret = NdVec::origin();
        for ax in D::axes() {
            ret[ax] = cmp::max(self.a[ax], self.b[ax]);
        }
        ret
    }

    /// Returns the length of this NdRect along the given axis.
    pub fn len(&self, axis: Axis) -> usize {
        (self.a[axis] - self.b[axis]).abs() as usize + 1
    }
    /// Returns the number of cells in this NdRect.
    pub fn count(&self) -> usize {
        let mut product = 1;
        for ax in D::axes() {
            product *= self.len(ax);
        }
        product
    }

    /// Returns true if the cell position is contained within this
    /// hyperrectangle.
    pub fn contains(&self, pos: NdVec<D>) -> bool {
        let min = self.min();
        let max = self.max();
        for ax in D::axes() {
            if pos[ax] < min[ax] || pos[ax] > max[ax] {
                return false;
            }
        }
        true
    }

    /// Constructs and equivalent NdRect with its corners swapped.
    pub fn swap(&self) -> Self {
        Self {
            a: self.b,
            b: self.a,
        }
    }
    /// Constructs an equivalent NdRect using the minimum and maximum corners.
    pub fn order(&self) -> Self {
        Self {
            a: self.min(),
            b: self.max(),
        }
    }

    /// Returns an iterator over all the positions in this hyperrectangle.
    pub fn iter(self) -> NdRectIter<D> {
        NdRectIter {
            rect: self.order(),
            next: self.min(),
            done: false,
        }
    }
}

// TODO implement addition/subtraction of NdVecs

/// An iterator over a hyperrectangle of cell positions.
#[derive(Debug, Copy, Clone)]
pub struct NdRectIter<D: Dim> {
    rect: NdRect<D>,
    next: NdVec<D>,
    done: bool,
}

impl<D: Dim> Iterator for NdRectIter<D> {
    type Item = NdVec<D>;
    fn next(&mut self) -> Option<NdVec<D>> {
        if self.done {
            None
        } else {
            let ret = self.next;
            for ax in D::axes() {
                self.next[ax] += 1;
                if self.next[ax] > self.rect.b[ax] {
                    self.next[ax] = self.rect.a[ax];
                } else {
                    return Some(ret);
                }
            }
            self.done = true;
            Some(ret)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use std::collections::HashSet;

    fn test_iter_validity<D: Dim>(rect: NdRect<D>) {
        // Test that the iterator agrees with count().
        let cells: Vec<NdVec<D>> = rect.iter().collect();
        assert_eq!(rect.count(), cells.len());
        // Test that there are no duplicates.
        {
            let cell_set: HashSet<NdVec<D>> = rect.iter().collect();
            assert_eq!(rect.count(), cell_set.len());
        }
        for cell in cells {
            assert!(rect.contains(cell));
        }
    }

    proptest! {
        /// Test NdRect::single_cell() and various methods.
        #[test]
        fn test_rect_region_single_cell(
            pos: Vec3D,
            offset in Vec3D::arbitrary_with(Some(4)),
        ) {
            let rect = NdRect::single_cell(pos);
            // Test contains().
            assert!(rect.contains(pos));
            assert_eq!(offset.is_zero(), rect.contains(pos + offset));
            // Test is_empty().
            // assert!(!rect.is_empty()); // TODO remove
            // Test count().
            assert_eq!(1, rect.count());
            // Test iteration.
            test_iter_validity(rect);
        }

        /// Test NdRect::span() and various methods.
        #[test]
        fn test_rect_region_span(
            corner1 in Vec3D::arbitrary_with(Some(5)),
            corner2 in Vec3D::arbitrary_with(Some(5)),
        ) {
            let rect = NdRect::span(corner1, corner2);
            // There's no nice way to test contains() here; we'll leave that to
            // the other methods.
            // Test is_empty().
            // assert!(!rect.is_empty()); // TODO remove
            // Test count() and iteration.
            test_iter_validity(rect);
        }

        /// Test NdRect::centered() and various methods.
        #[test]
        fn test_rect_region_centered(
            center: Vec3D,
            radius in 0..2usize,
            test_offset in Vec3D::arbitrary_with(Some(3)),
        ) {
            let rect = NdRect::centered(center, radius);
            // Test is_empty().
            // assert!(!rect.is_empty()); // TODO remove
            // Test contains()
            {
                let mut contains = true;
                for ax in Dim3D::axes() {
                    if test_offset[ax].abs() > radius as isize {
                        contains = false;
                    }
                }
                assert_eq!(contains, rect.contains(center + test_offset));
            }
            // Test count().
            assert_eq!((radius * 2 + 1).pow(3), rect.count());
            // Test count() and iteration.
            test_iter_validity(rect);
        }
    }
}
