use num::{Float, Integer};
use std::ops::{Add, AddAssign, MulAssign, Sub, SubAssign};

mod aliases;
mod iter;
mod ops;

use super::*;
pub use aliases::*;
use iter::*;

/// A "trait alias" for types that can be used as vectors in an NdRect.
pub trait NdRectVec:
    Sized + Add<Self, Output = Self> + Sub<Self, Output = Self> + AddAssign + SubAssign
{
}
impl<T> NdRectVec for T where
    T: Add<Self, Output = Self> + Sub<Self, Output = Self> + AddAssign + SubAssign
{
}

/// An N-dimensional hyperrectangle.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NdRect<D: DimFor<N>, N: NdVecNum>
where
    NdVec<D, N>: NdRectVec,
{
    /// The most negative corner of the hyperrectangle.
    start: NdVec<D, N>,
    /// The size of the hyperrectangle.
    size: NdVec<D, N>,
}

impl<D: DimFor<N>, N: NdVecNum> Copy for NdRect<D, N> where NdVec<D, N>: NdRectVec + Copy {}

impl<D: DimFor<N>, N: NdVecNum> NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    /// Constructs a new NdRect with the given starting position and the given
    /// size.
    pub fn new(start: NdVec<D, N>, size: NdVec<D, N>) -> Self {
        NdRect { start, size }
    }
    /// Constructs an NdRect spaning the given positions (inclusive).
    pub fn span(mut a: NdVec<D, N>, mut b: NdVec<D, N>) -> Self {
        for &ax in D::Dim::axes() {
            if a[ax] > b[ax] {
                std::mem::swap(&mut a[ax], &mut b[ax]);
            }
        }
        let mut size = b;
        size -= a.clone();
        size += NdVec::repeat(N::get_min_rect_size());
        Self { start: a, size }
    }
    /// Constructs an NdRect consisting of a single cell.
    pub fn single_cell(pos: NdVec<D, N>) -> Self {
        Self {
            start: pos,
            size: NdVec::repeat(N::one()),
        }
    }
    /// Constructs an NdRect with size 2r+1, given a center point and a radius r.
    pub fn centered<X: Copy>(center: NdVec<D, N>, radius: X) -> Self
    where
        NdVec<D, N>: Add<X, Output = NdVec<D, N>> + Sub<X, Output = NdVec<D, N>>,
    {
        let start = center - radius;
        let size = NdVec::repeat(N::get_min_rect_size()) + radius + radius;
        Self { start, size }
    }
    /// Constructs an NdRect describing a Moore neighborhood of a given radius
    /// centered at the origin.
    pub fn moore<X: Into<N>>(radius: X) -> Self
    where
        NdVec<D, N>: Copy + Add<X, Output = NdVec<D, N>> + Sub<X, Output = NdVec<D, N>>,
    {
        Self::centered(NdVec::origin(), NdVec::repeat(radius))
    }

    /// Returns the minimum (most negative) corner of this NdRect.
    pub fn min(&self) -> NdVec<D, N> {
        self.start.clone()
    }
    /// Returns the maximum (most positive) corner of this NdRect.
    pub fn max(&self) -> NdVec<D, N> {
        self.start.clone() + self.size.clone() - NdVec::repeat(N::get_min_rect_size())
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
        self.size.product()
    }

    /// Returns an iterator over all the positions in this hyperrectangle.
    pub fn iter(&self) -> NdRectIter<D, N>
    where
        N: Integer,
    {
        self.into()
    }

    /// Returns a range over all the values of the given axis in this
    /// hyperrectangle.
    pub fn axis_range(&self, axis: Axis) -> num::iter::RangeInclusive<N>
    where
        N: Integer,
    {
        num::range_inclusive(self.start[axis].clone(), self.max()[axis].clone())
    }

    /// Returns true if the two hyperrectangles intersect at all.
    #[must_use = "This method returns a new value instead of mutating its input"]
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
    #[must_use = "This method returns a new value instead of mutating its input"]
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

    /// Constructs an NdRect with the minimum and maximum coordinates offset by
    /// the given amount.
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn offset_min_max<T1, T2>(self, min_offset: T1, max_offset: T2) -> Self
    where
        NdVec<D, N>: Add<T1, Output = NdVec<D, N>> + Add<T2, Output = NdVec<D, N>>,
    {
        Self::span(self.min() + min_offset, self.max() + max_offset)
    }

    /// Converts this NdRect from one numeric type to another using
    /// NdVec::convert().
    pub fn convert<N2: NdVecNum>(&self) -> NdRect<D, N2>
    where
        D: DimFor<N2>,
        NdVec<D, N2>: NdRectVec,
        N2: std::convert::From<N>,
    {
        let start = self.start.convert();
        let size = self.size.convert();
        NdRect { start, size }
    }
}

impl<D: Dim + DimFor<N>, N: NdVecNum> NdRect<D, N>
where
    NdVec<D, N>: NdRectVec + AsUVec<D>,
{
    /// Converts this NdRect into one using usize.
    pub fn as_urect(&self) -> URect<D> {
        URect::new(self.start.as_uvec(), self.size.as_uvec())
    }
}
impl<D: Dim + DimFor<N>, N: NdVecNum> NdRect<D, N>
where
    NdVec<D, N>: NdRectVec + AsIVec<D>,
{
    /// Converts this NdRect into one using isize.
    pub fn as_irect(&self) -> IRect<D> {
        IRect::new(self.start.as_ivec(), self.size.as_ivec())
    }
}

/// A trait to allow overloading of the contains() method.
pub trait CanContain<I> {
    /// Returns true if `inner` is within `self`; usually implemented for
    /// structs that range over a set of cells on a grid with respect to cell
    /// position vectors.
    fn contains(&self, inner: I) -> bool;
}

impl<D: DimFor<N>, N: NdVecNum> CanContain<&NdVec<D, N>> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    /// Returns true if the cell position is contained within this
    /// hyperrectangle.
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
    /// Returns true if the cells of the given NdRect are a subset of this
    /// one's; i.e. that hyperrectangle is contained within this one.
    fn contains(&self, rect: &Self) -> bool {
        self.contains(&rect.min()) && self.contains(&rect.max())
    }
}
