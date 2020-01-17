use noisy_float::types::R64;
use num::{Float, Integer, One, Zero};
use std::ops::{Add, AddAssign, MulAssign, Sub, SubAssign};

mod aliases;
mod iter;
mod ops;

use super::*;
use aliases::*;
use iter::*;

/// A "trait alias" for types that can be used as vectors in an NdRect.
pub trait NdRectVec:
    Sized + Add<Self, Output = Self> + Sub<Self, Output = Self> + AddAssign + SubAssign + Zero + One
{
}
impl<T> NdRectVec for T where
    T: Add<Self, Output = Self> + Sub<Self, Output = Self> + AddAssign + SubAssign + Zero + One
{
}

/// An N-dimensional hyperrectangle.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NdRect<D: DimFor<N>, N: NdVecNum>
where
    NdVec<D, N>: NdRectVec,
{
    /// The most negative corner of the hyperrectangle.
    min: NdVec<D, N>,
    /// The size of the hyperrectangle.
    size: NdVec<D, N>,
}

impl<D: DimFor<N>, N: NdVecNum> Copy for NdRect<D, N> where NdVec<D, N>: NdRectVec + Copy {}

impl<D: DimFor<N>, N: NdVecNum> NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    /// Constructs an NdRect spanning the given positions (inclusive).
    pub fn span(mut a: NdVec<D, N>, mut b: NdVec<D, N>) -> Self {
        for &ax in D::Dim::axes() {
            if a[ax] > b[ax] {
                std::mem::swap(&mut a[ax], &mut b[ax]);
            }
        }
        let mut size = b;
        size -= a.clone();
        size += NdVec::from(N::get_min_rect_size());
        Self { min: a, size }
    }
    /// Constructs an NdRect consisting of a single cell.
    pub fn single_cell(pos: NdVec<D, N>) -> Self {
        Self {
            min: pos,
            size: NdVec::one(),
        }
    }
    /// Constructs an NdRect with size 2r+1, given a center point and a radius r.
    pub fn centered<X: Copy>(center: NdVec<D, N>, radius: X) -> Self
    where
        NdVec<D, N>: Sub<X, Output = NdVec<D, N>>,
        N: Add<X, Output = N>,
    {
        let min = center - radius;
        let size = NdVec::from(N::get_min_rect_size() + radius + radius);
        Self { min, size }
    }
    /// Constructs an NdRect describing a Moore neighborhood of a given radius
    /// centered at the origin.
    pub fn moore<X: Copy>(radius: X) -> Self
    where
        NdVec<D, N>: Sub<X, Output = NdVec<D, N>>,
        N: Add<X, Output = N>,
    {
        Self::centered(NdVec::origin(), radius)
    }

    /// Returns the minimum (most negative) corner of this NdRect.
    pub fn min(&self) -> NdVec<D, N> {
        self.min.clone()
    }
    /// Returns the maximum (most positive) corner of this NdRect.
    pub fn max(&self) -> NdVec<D, N> {
        self.min.clone() + self.size.clone() - NdVec::from(N::get_min_rect_size())
    }

    /// Returns an NdVec representing the length of this NdRect along each axis.
    pub fn size(&self) -> NdVec<D, N> {
        self.size.clone()
    }
    /// Returns the length of this NdRect along the given axis.
    pub fn len(&self, axis: Axis) -> N {
        self.size[axis].clone()
    }
    /// Returns the number of cells in this NdRect.
    pub fn count(&self) -> N
    where
        N: Integer + MulAssign,
    {
        let mut product = N::one();
        for &ax in D::Dim::axes() {
            product *= self.len(ax);
        }
        product
    }

    /// Returns an iterator over all the positions in this hyperrectangle.
    pub fn into_iter(self) -> NdRectIter<D, N>
    where
        N: Integer,
    {
        self.into()
    }

    /// Returns a range over all the values of the given axis in this
    /// hyperrectangle.
    pub fn axis_range(self, axis: Axis) -> std::ops::RangeInclusive<N>
    where
        N: Integer,
    {
        self.min[axis].clone()..=self.max()[axis].clone()
    }

    /// Returns true if the two hyperrectangles intersect at all.
    pub fn intersects(self, other: Self) -> bool {
        for &ax in D::Dim::axes() {
            // If the rectangles do not intersect along this axis, return false.
            if other.max()[ax] < self.min()[ax] || self.max()[ax] < other.min()[ax] {
                return false;
            }
        }
        // If the rectangles intersect along each axis, then they must really
        // intersect in N-dimensional space.
        true
    }

    /// Returns the intersection of the two hyperrectangles, or None if they do
    /// not intersect.
    pub fn intersection(self, other: Self) -> Option<Self> {
        let new_min = NdVec::max(&self.min(), &other.min());
        let new_max = NdVec::min(&self.max(), &other.max());
        for &ax in D::Dim::axes() {
            if &new_max[ax] < &new_min[ax] {
                return None;
            }
        }
        Some(Self::span(new_min, new_max))
    }

    /// Return a new NdRect with the minimum and maximum coordinates offset by the given amount.
    pub fn offset_min_max<T1, T2>(self, min_offset: T1, max_offset: T2) -> Self
    where
        NdVec<D, N>: Add<T1, Output = NdVec<D, N>> + Add<T2, Output = NdVec<D, N>>,
    {
        Self::span(self.min() + min_offset, self.max() + max_offset)
    }
}

impl<D: DimFor<N>, N: NdVecNum> CanContain<NdVec<D, N>> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    /// Returns true if the cell position is contained within this
    /// hyperrectangle.
    fn contains(&self, pos: NdVec<D, N>) -> bool {
        for &ax in D::Dim::axes() {
            if pos[ax] < self.min()[ax] || pos[ax] > self.max()[ax] {
                return false;
            }
        }
        true
    }
}
impl<D: DimFor<N>, N: NdVecNum> CanContain<Self> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    /// Returns true if the cells of the given NdRect are a subset of this
    /// one's; i.e. that hyperrectangle is contained within this one.
    fn contains(&self, rect: Self) -> bool {
        self.contains(rect.min()) && self.contains(rect.max())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use std::collections::HashSet;

    fn test_iter_validity<D: Dim>(rect: IRect<D>) {
        // Test that the iterator agrees with count().
        let cells: Vec<IVec<D>> = rect.clone().into_iter().collect();
        assert_eq!(rect.count() as usize, cells.len());
        // Test that there are no duplicates.
        {
            let cell_set: HashSet<IVec<D>> = rect.clone().into_iter().collect();
            assert_eq!(rect.count() as usize, cell_set.len());
        }
        for cell in cells {
            assert!(rect.contains(cell));
        }
    }

    proptest! {
        /// Test NdRect::single_cell() and various methods.
        #[test]
        fn test_rect_region_single_cell(
            pos: IVec3D,
            offset in IVec3D::arbitrary_with(Some(4)),
        ) {
            let rect = NdRect::single_cell(pos);
            // Test contains().
            assert!(rect.contains(pos));
            assert_eq!(offset.is_zero(), rect.contains(pos + offset));
            // Test min() and max().
            assert_eq!(pos, rect.min());
            assert_eq!(pos, rect.max());
            // Test size().
            assert_eq!(NdVec::one(), rect.size());
            // Test count().
            assert_eq!(1, rect.count());
            // Test iteration.
            test_iter_validity(rect);
        }

        /// Test NdRect::span() and various methods.
        #[test]
        fn test_rect_region_span(
            corner1 in IVec3D::arbitrary_with(Some(5)),
            corner2 in IVec3D::arbitrary_with(Some(5)),
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
            center: IVec3D,
            radius in 0..2isize,
            test_offset in IVec3D::arbitrary_with(Some(3)),
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
            corner1: IVec3D,
            corner2: IVec3D,
            offset: IVec3D,
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
