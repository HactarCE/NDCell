//! N-dimensional rectangles.

use noisy_float::prelude::R64;
use num::{BigInt, Float, Integer, ToPrimitive};
use std::ops::{Add, AddAssign, MulAssign, Sub, SubAssign};

mod aliases;
mod convert;
mod iter;
mod ops;

use super::*;
pub use aliases::*;
use iter::*;

/// "Trait alias" for types that can be used as vectors in an NdRect.
pub trait NdRectVec:
    Sized + Add<Self, Output = Self> + Sub<Self, Output = Self> + AddAssign + SubAssign
{
}
impl<T> NdRectVec for T where
    T: Add<Self, Output = Self> + Sub<Self, Output = Self> + AddAssign + SubAssign
{
}

/// N-dimensional rectangle.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NdRect<D: DimFor<N>, N: NdVecNum>
where
    NdVec<D, N>: NdRectVec,
{
    /// Most negative corner.
    start: NdVec<D, N>,
    /// Size along each axis.
    size: NdVec<D, N>,
}

impl<D: DimFor<N>, N: NdVecNum> Copy for NdRect<D, N> where NdVec<D, N>: NdRectVec + Copy {}

impl<D: DimFor<N>, N: NdVecNum> NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    /// Creates an `NdRect` with the given starting position and size.
    ///
    /// # Panics
    ///
    /// This function panics if `size` is zero or negative along any axis.
    #[inline]
    pub fn with_size(start: NdVec<D, N>, size: NdVec<D, N>) -> Self {
        assert!(size > NdVec::origin(), "NdRect must have positive volume");
        NdRect { start, size }
    }
    /// Creates an `NdRect` spanning between the given positions (inclusive).
    #[inline]
    pub fn span(mut a: NdVec<D, N>, mut b: NdVec<D, N>) -> Self {
        for &ax in D::Dim::axes() {
            if a[ax] > b[ax] {
                std::mem::swap(&mut a[ax], &mut b[ax]);
            }
        }
        let mut size = b;
        size -= a.clone();
        size += NdVec::repeat(N::min_rect_size());
        Self { start: a, size }
    }
    /// Creates a unit square with its minimum corner at the given position.
    #[inline]
    pub fn single_cell(pos: NdVec<D, N>) -> Self {
        Self {
            start: pos,
            size: NdVec::repeat(N::one()),
        }
    }
    /// Creates an `NdRect` with size `2r + 1`, given a center point and a
    /// radius `r`.
    #[inline]
    pub fn centered<X: Copy>(center: NdVec<D, N>, radius: X) -> Self
    where
        NdVec<D, N>: Add<X, Output = NdVec<D, N>> + Sub<X, Output = NdVec<D, N>>,
    {
        let start = center - radius;
        let size = NdVec::repeat(N::min_rect_size()) + radius + radius;
        Self { start, size }
    }
    /// Creates an `NdRect` describing a Moore neighborhood of a given radius
    /// centered at the origin.
    #[inline]
    pub fn moore<X: Copy + Into<N>>(radius: X) -> Self
    where
        NdVec<D, N>: Add<X, Output = NdVec<D, N>> + Sub<X, Output = NdVec<D, N>>,
    {
        Self::centered(NdVec::origin(), radius)
    }
    /// Creates a square `NdRect` with the given size and its minimum corner at
    /// the origin.
    pub fn square_from_origin<X: Copy + Into<N>>(size: X) -> Self {
        Self::with_size(NdVec::origin(), NdVec::repeat(size.into()))
    }

    /// Returns the minimum (most negative) corner of the rectangle.
    #[inline]
    pub fn min(&self) -> NdVec<D, N> {
        self.start.clone()
    }
    /// Returns the maximum (most positive) corner of the rectangle.
    #[inline]
    pub fn max(&self) -> NdVec<D, N> {
        self.start.clone() + self.size.clone() - NdVec::repeat(N::min_rect_size())
    }

    /// Returns a vector of the lengths of the rectangle along each axis.
    #[inline]
    pub fn size(&self) -> NdVec<D, N> {
        self.size.clone()
    }
    /// Returns the length of the rectangle along the given axis.
    #[inline]
    pub fn len(&self, axis: Axis) -> N {
        self.size[axis].clone()
    }
    /// Returns the number of integer positions in the rectangle.
    #[inline]
    pub fn count(&self) -> N
    where
        N: Integer + MulAssign,
    {
        self.size.product()
    }

    /// Returns an iterator over all the integer positions in the rectangle.
    #[inline]
    pub fn iter(&self) -> NdRectIter<D, N>
    where
        N: Integer + ToPrimitive,
    {
        self.into()
    }

    /// Returns a range over all the values of the given axis in the rectangle.
    #[inline]
    pub fn axis_range(&self, axis: Axis) -> num::iter::RangeInclusive<N>
    where
        N: Integer,
    {
        num::range_inclusive(self.start[axis].clone(), self.max()[axis].clone())
    }

    /// Returns `true` if the two rectangles intersect.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn intersects(self, other: Self) -> bool {
        // Iff `self` and `other` intersect along all axes, then they truly
        // intersect in N-dimensional space.
        D::Dim::axes().iter().all(|&ax| {
            // Do `self` and `other` intersect along this axis?

            //   `self` is "below" `other`.          `other` is "below" `self`.
            !(self.max()[ax] < other.min()[ax] || other.max()[ax] < self.min()[ax])
        })
    }

    /// Returns the rectangular intersection of two rectangles, or `None` if
    /// they do not intersect.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        let new_min = NdVec::max(&self.min(), &other.min());
        let new_max = NdVec::min(&self.max(), &other.max());
        for &ax in D::Dim::axes() {
            if &new_max[ax] < &new_min[ax] {
                return None;
            }
        }
        Some(Self::span(new_min, new_max))
    }

    /// Offsets the minimum and maximum corners of the rectangle by the given
    /// amount.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn offset_min_max<T1, T2>(self, min_offset: T1, max_offset: T2) -> Self
    where
        NdVec<D, N>: Add<T1, Output = NdVec<D, N>> + Add<T2, Output = NdVec<D, N>>,
    {
        Self::span(self.min() + min_offset, self.max() + max_offset)
    }
}

/// Trait to allow overloading of the `contains()` method.
pub trait CanContain<I> {
    /// Returns `true` if `inner` is "contained" within `self`.
    fn contains(&self, inner: I) -> bool;
}

impl<D: DimFor<N>, N: NdVecNum> CanContain<&NdVec<D, N>> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    /// Returns `true` if `pos` is contained within the rectangle.
    #[inline]
    fn contains(&self, pos: &NdVec<D, N>) -> bool {
        for &ax in D::Dim::axes() {
            if pos[ax] < self.min()[ax] || pos[ax] > self.max()[ax] {
                return false;
            }
        }
        true
    }
}
impl<D: DimFor<N>, N: NdVecNum> CanContain<&Self> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    /// Returns `true` if `rect` is completely contained within the rectangle;
    /// i.e., the intersection of the two equals `rect`.
    #[inline]
    fn contains(&self, rect: &Self) -> bool {
        self.contains(&rect.min()) && self.contains(&rect.max())
    }
}
