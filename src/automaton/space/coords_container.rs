use delegate::delegate;
use std::cmp::Eq;
use std::hash::Hash;
use std::ops::*;

use super::Coords;

/// The coordinates for a chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ChunkCoords<C: Coords>(C);

/// The coordinates for a cell.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CellCoords<C: Coords>(C);

/// The coordinates for a cell within a chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalCoords<C: Coords>(C);

impl<C: Coords> LocalCoords<C> {
    /// Returns the shape of a chunk of this many dimensions.
    pub fn get_chunk_shape() -> Self {
        Self::origin() + Self::CHUNK_SIZE as isize
    }
}

impl<C: Coords> From<CellCoords<C>> for ChunkCoords<C> {
    /// Return the coordinates of the chunk containing the given cell
    /// coordinates.
    ///
    /// Because Rust does not have a proper floored division operator (`/`
    /// rounds toward zero rather than negative infinity), we instead use
    /// bitwise operators. `n // 2**k` is the same as `n >> k`. This may even be
    /// faster.
    fn from(cell_coords: CellCoords<C>) -> Self {
        let mut ret = C::origin();
        for i in 0..C::NDIM {
            ret.set(i, cell_coords.0.get(i) >> C::CHUNK_BITS);
        }
        Self(ret)
    }
}

impl<C: Coords> From<CellCoords<C>> for LocalCoords<C> {
    /// Returns the local (within a chunk) coordinates for this position, given
    /// the base-2 log of the chunk size.
    ///
    /// Because Rust does not have a proper modulo operator (`%` may return
    /// negative values), we instead use bitwise operators. `n mod 2**k` is the
    /// same as `n & (2**k - 1)`. This may even be faster.
    fn from(cell_coords: CellCoords<C>) -> Self {
        let mut ret = C::origin();
        for i in 0..C::NDIM {
            ret.set(i, cell_coords.0.get(i) & ((1 << C::CHUNK_BITS) - 1))
        }
        Self(ret)
    }
}

/// Given the name of a tuple struct whose only type parameter is a Coords type
/// and whose first parameter is an instance of that Coords type, implement
/// Coords by delegating to the methods of the contained Coords instance.
macro_rules! delegate_coords_impl {
    ($coords_container:ident) => {
        // Implement conversion from Coords trait.
        impl<C: Coords> From<C> for $coords_container<C> {
            fn from(coords: C) -> Self {
                Self(coords)
            }
        }
        // Implement generic vector operations.
        impl<C: Coords> Coords for $coords_container<C> {
            type D = C::D;
            const NDIM: usize = C::NDIM;
            const CHUNK_BITS: usize = C::CHUNK_BITS;
            const CHUNK_SIZE: usize = C::CHUNK_SIZE;
            delegate! {
                target self.0 {
                    fn get(&self, axis: usize) -> isize;
                    fn set(&mut self, axis: usize, value: isize);
                }
            }
            fn origin() -> Self {
                Self(C::origin())
            }
        }
        // Implement elementwise addition between two sets of coordinates.
        impl<C: Coords> Add<Self> for $coords_container<C> {
            type Output = Self;
            fn add(self, other: Self) -> Self {
                let mut ret = self;
                ret += other;
                ret
            }
        }
        impl<C: Coords> AddAssign<Self> for $coords_container<C> {
            fn add_assign(&mut self, other: Self) {
                for i in 0..Self::NDIM {
                    self.set(i, self.get(i) + other.get(i));
                }
            }
        }
        // Imlement addition between a set of coordinates and a scalar (i.e. add
        // the scalar to each coordinate).
        impl<C: Coords> Add<isize> for $coords_container<C> {
            type Output = Self;
            fn add(self, other: isize) -> Self {
                let mut ret = self;
                ret += other;
                ret
            }
        }
        impl<C: Coords> AddAssign<isize> for $coords_container<C> {
            fn add_assign(&mut self, other: isize) {
                for i in 0..Self::NDIM {
                    self.set(i, self.get(i) + other);
                }
            }
        }
        // Implement elementwise subtraction between two sets of coordinates.
        impl<C: Coords> Sub<Self> for $coords_container<C> {
            type Output = Self;
            fn sub(self, other: Self) -> Self {
                let mut ret = self;
                ret -= other;
                ret
            }
        }
        impl<C: Coords> SubAssign<Self> for $coords_container<C> {
            fn sub_assign(&mut self, other: Self) {
                for i in 0..Self::NDIM {
                    self.set(i, self.get(i) - other.get(i));
                }
            }
        }
        // Implement subtraction between a set of coordinates and a scalar (i.e.
        // subtract the scalar from each coordinate).
        impl<C: Coords> Sub<isize> for $coords_container<C> {
            type Output = Self;
            fn sub(self, other: isize) -> Self {
                let mut ret = self;
                ret -= other;
                ret
            }
        }
        impl<C: Coords> SubAssign<isize> for $coords_container<C> {
            fn sub_assign(&mut self, other: isize) {
                for i in 0..Self::NDIM {
                    self.set(i, self.get(i) - other);
                }
            }
        }
        // Implement multiplication between a set of coordinates and a scalar
        // (i.e. multiply each coordinate by the scalar).
        impl<C: Coords> Mul<isize> for $coords_container<C> {
            type Output = Self;
            fn mul(self, other: isize) -> Self {
                let mut ret = self;
                ret *= other;
                ret
            }
        }
        impl<C: Coords> MulAssign<isize> for $coords_container<C> {
            fn mul_assign(&mut self, other: isize) {
                for i in 0..Self::NDIM {
                    self.set(i, self.get(i) * other);
                }
            }
        }
        // Implement integer division between a set of coordinates and a scalar
        // (i.e. divide each coordinate by the scalar).
        impl<C: Coords> Div<isize> for $coords_container<C> {
            type Output = Self;
            fn div(self, other: isize) -> Self {
                let mut ret = self;
                ret /= other;
                ret
            }
        }
        impl<C: Coords> DivAssign<isize> for $coords_container<C> {
            fn div_assign(&mut self, other: isize) {
                for i in 0..Self::NDIM {
                    self.set(i, self.get(i) / other);
                }
            }
        }
        // Implement negation of a set of coordinates (i.e. negate each
        // coordinate).
        impl<C: Coords> Neg for $coords_container<C> {
            type Output = Self;
            fn neg(mut self) -> Self {
                for i in 0..Self::NDIM {
                    self.set(i, -self.get(i));
                }
                self
            }
        }
    };
}

// Implement Coords for each of these structs.
delegate_coords_impl!(CellCoords);
delegate_coords_impl!(ChunkCoords);
delegate_coords_impl!(LocalCoords);

impl<C: Coords> LocalCoords<C> {
    /// Converts the coordinates to an NdIndex.
    ///
    /// Panics if any values are negative.
    pub fn ndindex(&self) -> C::D {
        use ndarray::prelude::*;

        let mut ret = C::D::zeros(C::NDIM);
        for i in 0..Self::NDIM {
            let value = self.0.get(i);
            if value < 0 {
                panic!("Cannot convert negative Coords to NdIndex");
            }
            ret[i] = value as usize;
        }
        Dim(ret)
    }
}

#[cfg(test)]
use proptest::prelude::*;

#[cfg(test)]
pub fn cell_coords_strategy<R: Strategy<Value = isize>>(
    value_strategy: R,
) -> impl Strategy<Value = CellCoords<[isize; 3]>> {
    prop::collection::vec(value_strategy, 3)
        .prop_flat_map(|vec| Just(CellCoords::from([vec[0], vec[1], vec[2]])))
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::proptest;

    proptest! {
        /// Test vector arithmetic against ndarray.
        #[test]
        fn test_ops(
            v1 in cell_coords_strategy(50..=100isize),
            v2 in cell_coords_strategy(0..=50isize),
            scalar in 0..=50usize
        ) {
            let v1: LocalCoords<[isize; 3]> = v1.into();
            let v2: LocalCoords<[isize; 3]> = v2.into();
            let iscalar: isize = scalar as isize;
            let uscalar: usize = scalar;
            let d1 = v1.ndindex();
            let d2 = v2.ndindex();
            assert_eq!(d1 + d2, (v1 + v2).ndindex());
            assert_eq!(d1 - d2, (v1 - v2).ndindex());
            assert_eq!(d1 * uscalar, (v1 * iscalar).ndindex());
            // Check negation properties.
            assert_eq!(v1, --v1);
            assert_eq!(v2, --v2);
            assert_ne!(v1, -v1); // Each element of v1 is >=50, so this should hold.
            assert_eq!(-v1, v1 * -1);
            assert_eq!(-v2, v2 * -1);
            // Check some subtraction properties.
            assert_eq!(LocalCoords::from([0, 0, 0]), v1 - v1);
            assert_eq!(LocalCoords::from([0, 0, 0]), v2 - v2);
            assert_eq!(-v1, v1 - v1 * 2);
            assert_eq!(-v2, v2 - v2 * 2);
        }
    }
}
