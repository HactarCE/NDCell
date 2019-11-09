use delegate::delegate;
use std::cmp::Eq;
use std::convert::TryInto;
use std::hash::Hash;
use std::iter;
use std::marker::PhantomData;
use std::ops::*;

use super::*;

/// The coordinates for a cell.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CellCoords<D: Dim>(pub D);

/// The coordinates for a chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ChunkCoords<D: Dim>(pub D);

/// The coordinates for a cell within a chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalCoords<D: Dim>(pub D);

/// A 1D CellCoords vector.
pub type CellCoords1D = CellCoords<Coords1D>;
/// A 2D CellCoords vector.
pub type CellCoords2D = CellCoords<Coords2D>;
/// A 3D CellCoords vector.
pub type CellCoords3D = CellCoords<Coords3D>;
/// A 4D CellCoords vector.
pub type CellCoords4D = CellCoords<Coords4D>;
/// A 5D CellCoords vector.
pub type CellCoords5D = CellCoords<Coords5D>;
/// A 6D CellCoords vector.
pub type CellCoords6D = CellCoords<Coords6D>;

/// A 1D ChunkCoords vector.
pub type ChunkCoords1D = ChunkCoords<Coords1D>;
/// A 2D ChunkCoords vector.
pub type ChunkCoords2D = ChunkCoords<Coords2D>;
/// A 3D ChunkCoords vector.
pub type ChunkCoords3D = ChunkCoords<Coords3D>;
/// A 4D ChunkCoords vector.
pub type ChunkCoords4D = ChunkCoords<Coords4D>;
/// A 5D ChunkCoords vector.
pub type ChunkCoords5D = ChunkCoords<Coords5D>;
/// A 6D ChunkCoords vector.
pub type ChunkCoords6D = ChunkCoords<Coords6D>;

/// A 1D LocalCoords vector.
pub type LocalCoords1D = LocalCoords<Coords1D>;
/// A 2D LocalCoords vector.
pub type LocalCoords2D = LocalCoords<Coords2D>;
/// A 3D LocalCoords vector.
pub type LocalCoords3D = LocalCoords<Coords3D>;
/// A 4D LocalCoords vector.
pub type LocalCoords4D = LocalCoords<Coords4D>;
/// A 5D LocalCoords vector.
pub type LocalCoords5D = LocalCoords<Coords5D>;
/// A 6D LocalCoords vector.
pub type LocalCoords6D = LocalCoords<Coords6D>;

impl<D: Dim> LocalCoords<D> {
    /// Returns the shape of a chunk of this many dimensions.
    pub fn get_chunk_shape() -> Self {
        Self::origin() + Self::CHUNK_SIZE as isize
    }
}

impl<D: Dim> CellCoords<D> {
    /// Return the coordinates of the chunk containing the given cell
    /// coordinates.
    ///
    /// Because Rust does not have a proper floored division operator (`/`
    /// rounds toward zero rather than negative infinity), we instead use
    /// bitwise operators. `n // 2**k` is the same as `n >> k`. This may even be
    /// faster.
    pub fn chunk(self) -> ChunkCoords<D> {
        let mut ret = D::origin();
        for ax in Self::axes() {
            ret.set(ax, self[ax] >> D::CHUNK_BITS);
        }
        ChunkCoords(ret)
    }
    /// Returns the local (within a chunk) coordinates for this position, given
    /// the base-2 log of the chunk size.
    ///
    /// Because Rust does not have a proper modulo operator (`%` may return
    /// negative values), we instead use bitwise operators. `n mod 2**k` is the
    /// same as `n & (2**k - 1)`. This may even be faster.
    pub fn local(self) -> LocalCoords<D> {
        let mut ret = D::origin();
        for ax in Self::axes() {
            ret.set(ax, self[ax] & Self::CHUNK_BITMASK);
        }
        LocalCoords(ret)
    }
}

/// An interator over all the local coordinates within a chunk.
pub struct LocalCoordsIter<D> {
    phantom: PhantomData<D>,
    current_idx: usize,
}
impl<D: Dim> iter::Iterator for LocalCoordsIter<D> {
    type Item = LocalCoords<D>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.current_idx >= D::CHUNK_SIZE.pow(D::NDIM as u32) {
            return None;
        }
        let mut ret = Self::Item::origin();
        {
            let mut current_idx = self.current_idx;
            for ax in D::axes() {
                ret[ax] = current_idx as isize & D::CHUNK_BITMASK;
                current_idx >>= D::CHUNK_BITS;
            }
        }
        self.current_idx += 1;
        Some(ret)
    }
}

impl<D: Dim> LocalCoords<D> {
    /// Return an iterator over all the local coordinates within a chunk.
    pub fn all() -> LocalCoordsIter<D> {
        LocalCoordsIter {
            phantom: PhantomData,
            current_idx: 0,
        }
    }
}

impl<D: Dim> From<CellCoords<D>> for ChunkCoords<D> {
    fn from(cell_coords: CellCoords<D>) -> Self {
        cell_coords.chunk()
    }
}

impl<D: Dim> From<CellCoords<D>> for LocalCoords<D> {
    fn from(cell_coords: CellCoords<D>) -> Self {
        cell_coords.local()
    }
}

impl<D: Dim> Add<LocalCoords<D>> for ChunkCoords<D> {
    type Output = CellCoords<D>;
    fn add(self, offset: LocalCoords<D>) -> CellCoords<D> {
        CellCoords((self * Self::CHUNK_SIZE as isize).0) + CellCoords(offset.0)
    }
}

impl<D: Dim> Add<ChunkCoords<D>> for LocalCoords<D> {
    type Output = CellCoords<D>;
    fn add(self, chunk: ChunkCoords<D>) -> CellCoords<D> {
        chunk + self
    }
}

/// Given the name of a tuple struct whose only type parameter is a Coords type
/// and whose first parameter is an instance of that Coords type, implement
/// Coords by delegating to the methods of the contained Coords instance.
macro_rules! delegate_coords_impl {
    ($coords_container:ident) => {
        // Implement conversion from Coords trait.
        impl<D: Dim> From<D> for $coords_container<D> {
            fn from(coords: D) -> Self {
                Self(coords)
            }
        }
        // Implement generic vector operations.
        impl<D: Dim> Coords for $coords_container<D> {
            type NdarrayDim = D::NdarrayDim;
            const NDIM: usize = D::NDIM;
            const CHUNK_BITS: usize = D::CHUNK_BITS;
            const CHUNK_SIZE: usize = D::CHUNK_SIZE;
            delegate! {
                target self.0 {
                    fn get(&self, axis: Axis) -> &isize;
                    fn get_mut(&mut self, axis: Axis) -> &mut isize;
                    fn set(&mut self, axis: Axis, value: isize);
                }
            }
            fn origin() -> Self {
                Self(D::origin())
            }
        }
        // Implement indexing by usize.
        impl<D: Dim> Index<Axis> for $coords_container<D> {
            type Output = isize;
            fn index(&self, axis: Axis) -> &isize {
                self.get(axis)
            }
        }
        impl<D: Dim> IndexMut<Axis> for $coords_container<D> {
            fn index_mut(&mut self, axis: Axis) -> &mut isize {
                self.get_mut(axis)
            }
        }
        // Implement elementwise addition between two sets of coordinates.
        impl<D: Dim> Add<Self> for $coords_container<D> {
            type Output = Self;
            fn add(self, other: Self) -> Self {
                let mut ret = self;
                ret += other;
                ret
            }
        }
        impl<D: Dim> AddAssign<Self> for $coords_container<D> {
            fn add_assign(&mut self, other: Self) {
                for ax in Self::axes() {
                    self.set(ax, self.get(ax) + other.get(ax));
                }
            }
        }
        // Imlement addition between a set of coordinates and a scalar (i.e. add
        // the scalar to each coordinate).
        impl<D: Dim> Add<isize> for $coords_container<D> {
            type Output = Self;
            fn add(self, other: isize) -> Self {
                let mut ret = self;
                ret += other;
                ret
            }
        }
        impl<D: Dim> AddAssign<isize> for $coords_container<D> {
            fn add_assign(&mut self, other: isize) {
                for ax in Self::axes() {
                    self.set(ax, self.get(ax) + other);
                }
            }
        }
        // Implement elementwise subtraction between two sets of coordinates.
        impl<D: Dim> Sub<Self> for $coords_container<D> {
            type Output = Self;
            fn sub(self, other: Self) -> Self {
                let mut ret = self;
                ret -= other;
                ret
            }
        }
        impl<D: Dim> SubAssign<Self> for $coords_container<D> {
            fn sub_assign(&mut self, other: Self) {
                for ax in Self::axes() {
                    self.set(ax, self.get(ax) - other.get(ax));
                }
            }
        }
        // Implement subtraction between a set of coordinates and a scalar (i.e.
        // subtract the scalar from each coordinate).
        impl<D: Dim> Sub<isize> for $coords_container<D> {
            type Output = Self;
            fn sub(self, other: isize) -> Self {
                let mut ret = self;
                ret -= other;
                ret
            }
        }
        impl<D: Dim> SubAssign<isize> for $coords_container<D> {
            fn sub_assign(&mut self, other: isize) {
                for ax in Self::axes() {
                    self.set(ax, self.get(ax) - other);
                }
            }
        }
        // Implement multiplication between a set of coordinates and a scalar
        // (i.e. multiply each coordinate by the scalar).
        impl<D: Dim> Mul<isize> for $coords_container<D> {
            type Output = Self;
            fn mul(self, other: isize) -> Self {
                let mut ret = self;
                ret *= other;
                ret
            }
        }
        impl<D: Dim> MulAssign<isize> for $coords_container<D> {
            fn mul_assign(&mut self, other: isize) {
                for ax in Self::axes() {
                    self.set(ax, self.get(ax) * other);
                }
            }
        }
        // Implement integer division between a set of coordinates and a scalar
        // (i.e. divide each coordinate by the scalar).
        impl<D: Dim> Div<isize> for $coords_container<D> {
            type Output = Self;
            fn div(self, other: isize) -> Self {
                let mut ret = self;
                ret /= other;
                ret
            }
        }
        impl<D: Dim> DivAssign<isize> for $coords_container<D> {
            fn div_assign(&mut self, other: isize) {
                for ax in Self::axes() {
                    self.set(ax, self.get(ax) / other);
                }
            }
        }
        // Implement negation of a set of coordinates (i.e. negate each
        // coordinate).
        impl<D: Dim> Neg for $coords_container<D> {
            type Output = Self;
            fn neg(mut self) -> Self {
                for ax in Self::axes() {
                    self.set(ax, -self.get(ax));
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

impl<D: Dim> LocalCoords<D> {
    /// Converts the coordinates to an NdIndex.
    ///
    /// Panics if any values are negative.
    pub fn ndindex(&self) -> D::NdarrayDim {
        use ndarray::prelude::*;

        let mut ret = D::NdarrayDim::zeros(D::NDIM);
        for ax in Self::axes() {
            ret[ax as usize] = self[ax]
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

    /// Tests iteration over all local coordinates in a chunk.
    #[test]
    fn test_local_coords_iter() {
        let mut chunk: Chunk<u8, Coords3D> = Chunk::default();
        for local_coords in LocalCoords::all() {
            chunk[local_coords] += 1;
        }
        for &value in chunk.array.iter() {
            assert_eq!(1, value);
        }
    }

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
            // Check that the local coordinates are within the range of a chunk.
            assert!(0 <= local_coords[Axis::X] && local_coords[Axis::X] < chunk_size);
            assert!(0 <= local_coords[Axis::Y] && local_coords[Axis::Y] < chunk_size);
            assert!(0 <= local_coords[Axis::Z] && local_coords[Axis::Z] < chunk_size);
            // Check that chunk + local = global.
            assert_eq!(cell_coords, chunk_coords + local_coords);
            assert_eq!(cell_coords, local_coords + chunk_coords);
        }
    }
}
