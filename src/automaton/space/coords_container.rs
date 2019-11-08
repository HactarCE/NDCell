use delegate::delegate;
use std::cmp::Eq;
use std::convert::TryInto;
use std::hash::Hash;
use std::ops::*;

use super::Coords;

/// The coordinates for a cell.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CellCoords<C: Coords>(C);

/// The coordinates for a chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ChunkCoords<C: Coords>(C);

/// The coordinates for a cell within a chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalCoords<C: Coords>(C);

/// A 1D CellCoords vector.
pub type CellCoords1D = CellCoords<[isize; 1]>;
/// A 2D CellCoords vector.
pub type CellCoords2D = CellCoords<[isize; 2]>;
/// A 3D CellCoords vector.
pub type CellCoords3D = CellCoords<[isize; 3]>;
/// A 4D CellCoords vector.
pub type CellCoords4D = CellCoords<[isize; 4]>;
/// A 5D CellCoords vector.
pub type CellCoords5D = CellCoords<[isize; 5]>;
/// A 6D CellCoords vector.
pub type CellCoords6D = CellCoords<[isize; 6]>;

/// A 1D ChunkCoords vector.
pub type ChunkCoords1D = ChunkCoords<[isize; 1]>;
/// A 2D ChunkCoords vector.
pub type ChunkCoords2D = ChunkCoords<[isize; 2]>;
/// A 3D ChunkCoords vector.
pub type ChunkCoords3D = ChunkCoords<[isize; 3]>;
/// A 4D ChunkCoords vector.
pub type ChunkCoords4D = ChunkCoords<[isize; 4]>;
/// A 5D ChunkCoords vector.
pub type ChunkCoords5D = ChunkCoords<[isize; 5]>;
/// A 6D ChunkCoords vector.
pub type ChunkCoords6D = ChunkCoords<[isize; 6]>;

/// A 1D LocalCoords vector.
pub type LocalCoords1D = LocalCoords<[isize; 1]>;
/// A 2D LocalCoords vector.
pub type LocalCoords2D = LocalCoords<[isize; 2]>;
/// A 3D LocalCoords vector.
pub type LocalCoords3D = LocalCoords<[isize; 3]>;
/// A 4D LocalCoords vector.
pub type LocalCoords4D = LocalCoords<[isize; 4]>;
/// A 5D LocalCoords vector.
pub type LocalCoords5D = LocalCoords<[isize; 5]>;
/// A 6D LocalCoords vector.
pub type LocalCoords6D = LocalCoords<[isize; 6]>;

impl<C: Coords> LocalCoords<C> {
    /// Returns the shape of a chunk of this many dimensions.
    pub fn get_chunk_shape() -> Self {
        Self::origin() + Self::CHUNK_SIZE as isize
    }
}

impl<C: Coords> CellCoords<C> {
    /// Return the coordinates of the chunk containing the given cell
    /// coordinates.
    ///
    /// Because Rust does not have a proper floored division operator (`/`
    /// rounds toward zero rather than negative infinity), we instead use
    /// bitwise operators. `n // 2**k` is the same as `n >> k`. This may even be
    /// faster.
    pub fn chunk(self) -> ChunkCoords<C> {
        let mut ret = C::origin();
        for i in 0..C::NDIM {
            ret.set(i, self.0.get(i) & ((1 << C::CHUNK_BITS) - 1))
        }
        ChunkCoords(ret)
    }
    /// Returns the local (within a chunk) coordinates for this position, given
    /// the base-2 log of the chunk size.
    ///
    /// Because Rust does not have a proper modulo operator (`%` may return
    /// negative values), we instead use bitwise operators. `n mod 2**k` is the
    /// same as `n & (2**k - 1)`. This may even be faster.
    pub fn local(self) -> LocalCoords<C> {
        let mut ret = C::origin();
        for i in 0..C::NDIM {
            ret.set(i, self.0.get(i) & ((1 << C::CHUNK_BITS) - 1))
        }
        LocalCoords(ret)
    }
}

impl<C: Coords> From<CellCoords<C>> for ChunkCoords<C> {
    fn from(cell_coords: CellCoords<C>) -> Self {
        cell_coords.chunk()
    }
}

impl<C: Coords> From<CellCoords<C>> for LocalCoords<C> {
    fn from(cell_coords: CellCoords<C>) -> Self {
        cell_coords.local()
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
                    fn get(&self, axis: usize) -> &isize;
                    fn get_mut(&mut self, axis: usize) -> &mut isize;
                    fn set(&mut self, axis: usize, value: isize);
                }
            }
            fn origin() -> Self {
                Self(C::origin())
            }
        }
        // Implement indexing by usize.
        impl<C: Coords> Index<usize> for $coords_container<C> {
            type Output = isize;
            fn index(&self, axis: usize) -> &isize {
                self.get(axis)
            }
        }
        impl<C: Coords> IndexMut<usize> for $coords_container<C> {
            fn index_mut(&mut self, axis: usize) -> &mut isize {
                self.get_mut(axis)
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
            ret[i] = self[i]
                .try_into()
                .expect("Cannot convert negative Coords to NdIndex");
        }
        Dim(ret)
    }
}

#[cfg(test)]
use proptest::prelude::*;

#[cfg(test)]
pub fn cell_coords_strategy<R: Strategy<Value = isize>>(
    value_strategy: R,
) -> impl Strategy<Value = CellCoords3D> {
    prop::collection::vec(value_strategy, 3)
        .prop_flat_map(|vec| Just(CellCoords::from([vec[0], vec[1], vec[2]])))
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::proptest;

    proptest! {
        #[test]
        fn test_ops(
            pos1 in cell_coords_strategy(50..=100isize),
            pos2 in cell_coords_strategy(0..=50isize),
            scalar in 0..=50usize
        ) {
            // Do not use .into() because that would modulo the input, which
            // could cause pos2 to be greater than pos1 along some axis (and
            // thus pos1-pos2 could not be represented by an unsigned NdIndex).
            let pos1: LocalCoords3D = LocalCoords(pos1.0);
            let pos2: LocalCoords3D = LocalCoords(pos2.0);
            let iscalar: isize = scalar as isize;
            let uscalar: usize = scalar;
            let d1 = pos1.ndindex();
            let d2 = pos2.ndindex();
            // Check addition, subtraction, and multiplication (scaling) against
            // ndarray.
            assert_eq!(d1 + d2, (pos1 + pos2).ndindex());
            assert_eq!(d1 - d2, (pos1 - pos2).ndindex());
            assert_eq!(d1 * uscalar, (pos1 * iscalar).ndindex());
            // Check negation properties.
            assert_eq!(pos1, --pos1);
            assert_eq!(pos2, --pos2);
            assert_ne!(pos1, -pos1); // Each element of pos1 is >=50, so this should hold.
            assert_eq!(-pos1, pos1 * -1);
            assert_eq!(-pos2, pos2 * -1);
            // Check some subtraction properties.
            assert_eq!(LocalCoords::from([0, 0, 0]), pos1 - pos1);
            assert_eq!(LocalCoords::from([0, 0, 0]), pos2 - pos2);
            assert_eq!(-pos1, pos1 - pos1 * 2);
            assert_eq!(-pos2, pos2 - pos2 * 2);
        }

        /// Tests CellCoords decomposition into LocalCoords and ChunkCoords.
        #[test]
        fn test_coords_split(
            cell_coords in cell_coords_strategy(-100..=100isize),
        ) {
            let chunk_coords: ChunkCoords3D = cell_coords.into();
            let local_coords: LocalCoords3D = cell_coords.into();
            let chunk_size = CellCoords3D::CHUNK_SIZE as isize;
            assert!(local_coords.get(0) < chunk_size);
            assert!(local_coords.get(1) < chunk_size);
            assert!(local_coords.get(2) < chunk_size);
            unsafe {
                assert_eq!(
                    cell_coords,
                    std::mem::transmute::<ChunkCoords3D, CellCoords3D>(chunk_coords * chunk_size)
                        + std::mem::transmute::<LocalCoords3D, CellCoords3D>(local_coords)
                );
            }
        }
    }
}
