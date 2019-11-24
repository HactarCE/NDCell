mod iter;
mod ops;

use super::*;
use iter::*;

/// An N-dimensional hyperrectangle.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NdRect<D: Dim> {
    /// The most negative corner of the hyperrectangle.
    min: NdVec<D>,
    /// The most positive corner of the hyperrectangle.
    max: NdVec<D>,
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
    /// Constructs an NdRect spanning the given positions (inclusive).
    pub fn span(mut a: NdVec<D>, mut b: NdVec<D>) -> Self {
        for &ax in D::axes() {
            if a[ax] > b[ax] {
                std::mem::swap(&mut a[ax], &mut b[ax]);
            }
        }
        Self { min: a, max: b }
    }
    /// Constructs an NdRect consisting of a single cell.
    pub fn single_cell(pos: NdVec<D>) -> Self {
        Self { min: pos, max: pos }
    }
    /// Constructs an NdRect with size 2r+1, given a center point and a radius r.
    pub fn centered(center: NdVec<D>, radius: usize) -> Self {
        Self {
            min: center - radius as isize,
            max: center + radius as isize,
        }
    }
    /// Constructs an NdRect describing a Moore neighborhood of a given radius
    /// centered at the origin.
    pub fn moore(radius: usize) -> Self {
        Self::centered(NdVec::origin(), radius)
    }

    /// Returns the minimum (most negative) corner of this NdRect.
    pub fn min(&self) -> NdVec<D> {
        self.min
    }
    /// Returns the maximum (most positive) corner of this NdRect.
    pub fn max(&self) -> NdVec<D> {
        self.max
    }

    /// Returns the length of this NdRect along the given axis.
    pub fn len(&self, axis: Axis) -> usize {
        (self.max[axis] - self.min[axis]) as usize + 1
    }
    /// Returns the number of cells in this NdRect.
    pub fn count(&self) -> usize {
        let mut product = 1;
        for &ax in D::axes() {
            product *= self.len(ax);
        }
        product
    }

    /// Returns an iterator over all the positions in this hyperrectangle.
    pub fn iter(self) -> NdRectIter<D> {
        self.into()
    }

    /// Returns a range over all the values of the given axis in this
    /// hyperrectangle.
    pub fn axis_range(self, axis: Axis) -> std::ops::RangeInclusive<isize> {
        self.min[axis]..=self.max[axis]
    }
}

impl<D: Dim> CanContain<NdVec<D>> for NdRect<D> {
    /// Returns true if the cell position is contained within this
    /// hyperrectangle.
    fn contains(&self, pos: NdVec<D>) -> bool {
        for &ax in D::axes() {
            if pos[ax] < self.min[ax] || pos[ax] > self.max[ax] {
                return false;
            }
        }
        true
    }
}
impl<D: Dim> CanContain<NdRect<D>> for NdRect<D> {
    /// Returns true if the cells of the given NdRect are a subset of this
    /// one's; i.e. that hyperrectangle is contained within this one.
    fn contains(&self, rect: NdRect<D>) -> bool {
        self.contains(rect.min()) && self.contains(rect.max())
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
            // Test contains()
            {
                let mut contains = true;
                for &ax in Dim3D::axes() {
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

        /// Test addition and subtract of NdRect and NdVec.
        #[test]
        fn test_rect_ops(
            corner1: Vec3D,
            corner2: Vec3D,
            offset: Vec3D,
        ) {
            assert_eq!(
                NdRect::span(corner1, corner2) + offset,
                NdRect::span(corner1 + offset, corner2 + offset),
            );
            assert_eq!(
                NdRect::span(corner1, corner2) - offset,
                NdRect::span(corner1 - offset, corner2 - offset),
            );
        }
    }
}
