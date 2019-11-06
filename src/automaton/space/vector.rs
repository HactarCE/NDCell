use ndarray::prelude::*;
use std::cmp::Eq;
use std::hash::Hash;

/// Computes the "recommended" number of bits in each axis of a chunk index for
/// a given dimension count. The chunk size (along each axis) is 1 << (chunk
/// bits).
///
/// This is based on trying to keep the chunk size big, but still reasonable
/// (such that a full chunk is at most 4k) and always a power of 2.
///
/// Using a flat chunk size would either result in stupidly small chunks at
/// lower dimensions (16 is silly for 1D CA that often densely span thousands of
/// cells) or stupidly huge chunks at higher dimensions (even a 32^4 chunk in 4D
/// would be 1 MiB, which is rather large to be copying around constantly).
///
/// Here are the values that this function outputs:
///
/// - 1D => 12 -> 4096 = 4k
/// - 2D => 12 -> 64^2 = 4k
/// - 3D => 12 -> 16^3 = 4k
/// - 4D => 12 ->  8^4 = 4k
/// - 5D => 12 ->  4^5 = 1k (8^5 would be 32k)
/// - 6D => 12 ->  4^6 = 4k
const fn get_chunk_bits_for_ndim(ndim: usize) -> usize {
    let max_bits = 12; // 2^12 = 4096
    max_bits / ndim
}

// impl<V: Vector> std::ops::Add for ChunkVector<V> {
//     type Output = CellVector<V>;
//     // fn add(Self, other: LocalVector<V>)
// }

/// A vector (in the gemoetrical sense) for a given dimensionality which allows
/// negative values, unlike NdIndex.
pub trait Vector: Clone + Eq + Hash {
    /// The number of dimensions as an ndarray type.
    type D: Dimension;

    /// The number of dimensions (number of axes).
    const NDIM: usize;

    /// The number of bits to use to index a chunk of this many dimensions.
    const CHUNK_BITS: usize = get_chunk_bits_for_ndim(Self::NDIM);

    /// The size (length along one axis) of a chunk of this many dimensions.
    const CHUNK_SIZE: usize = 1 << Self::CHUNK_BITS;

    /// Returns the coordinate along the given axis.
    fn get(&self, axis: usize) -> isize;

    /// Sets the coordinate along the given axis.
    fn set(&mut self, axis: usize, value: isize);

    /// Returns a vector pointing to the origin.
    fn origin() -> Self;
}

/// A vector index for a chunk.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ChunkVector<V: Vector>(V);
/// A vector index for a cell.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CellVector<V: Vector>(V);
/// A vector index for a cell within a chunk.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LocalVector<V: Vector>(V);

impl<V: Vector> From<&CellVector<V>> for ChunkVector<V> {
    /// Return the chunk vector for the given cell vector.
    ///
    /// Because Rust does not have a proper floored division operator (`/`
    /// rounds toward zero rather than negative infinity), we instead use
    /// bitwise operators. `n // 2**k` is the same as `n >> k`. This may even be
    /// faster.
    fn from(cell_vector: &CellVector<V>) -> Self {
        let mut ret = V::origin();
        for i in 0..V::NDIM {
            ret.set(i, cell_vector.0.get(i) >> V::CHUNK_BITS);
        }
        Self(ret)
    }
}

impl<V: Vector> From<&CellVector<V>> for LocalVector<V> {
    /// Returns the local (within a chunk) vector for this position, given the
    /// base-2 log of the chunk size.
    ///
    /// Because Rust does not have a proper modulo operator (`%` may return
    /// negative values), we instead use bitwise operators. `n mod 2**k` is the
    /// same as `n & (2**k - 1)`. This may even be faster.
    fn from(cell_vector: &CellVector<V>) -> Self {
        let mut ret = V::origin();
        for i in 0..V::NDIM {
            ret.set(i, cell_vector.0.get(i) & ((1 << V::CHUNK_BITS) - 1))
        }
        Self(ret)
    }
}

impl<V: Vector> From<V> for CellVector<V> {
    fn from(v: V) -> Self {
        Self(v)
    }
}
impl<V: Vector> From<V> for ChunkVector<V> {
    fn from(v: V) -> Self {
        Self(v)
    }
}
impl<V: Vector> From<V> for LocalVector<V> {
    fn from(v: V) -> Self {
        Self(v)
    }
}

impl<V: Vector> LocalVector<V> {
    /// Convert the vector to an ndindex.
    ///
    /// Panics if any values are negative.
    pub fn ndindex(&self) -> V::D {
        let mut ret = V::D::zeros(V::NDIM);
        for i in 0..V::NDIM {
            let value = self.0.get(i);
            if value < 0 {
                panic!("Cannot convert negative Vector to NdIndex");
            }
            ret[i] = value as usize;
        }
        Dim(ret)
    }
}

// impl<V: Vector> Vector for LocalVector<V> {
//     type D = V::D;
//     const NDIM: usize = V::NDIM;
//     fn get(&self, index: usize) -> isize {
//         self[index]
//     }
// }

impl Vector for [isize; 1] {
    type D = Ix1;
    const NDIM: usize = 1;
    fn get(&self, index: usize) -> isize {
        self[index]
    }
    fn set(&mut self, index: usize, value: isize) {
        self[index] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}

impl Vector for [isize; 2] {
    type D = Ix2;
    const NDIM: usize = 2;
    fn get(&self, index: usize) -> isize {
        self[index]
    }
    fn set(&mut self, index: usize, value: isize) {
        self[index] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Vector for [isize; 3] {
    type D = Ix3;
    const NDIM: usize = 3;
    fn get(&self, index: usize) -> isize {
        self[index]
    }
    fn set(&mut self, index: usize, value: isize) {
        self[index] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Vector for [isize; 4] {
    type D = Ix4;
    const NDIM: usize = 4;
    fn get(&self, index: usize) -> isize {
        self[index]
    }
    fn set(&mut self, index: usize, value: isize) {
        self[index] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Vector for [isize; 5] {
    type D = Ix5;
    const NDIM: usize = 5;
    fn get(&self, index: usize) -> isize {
        self[index]
    }
    fn set(&mut self, index: usize, value: isize) {
        self[index] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Vector for [isize; 6] {
    type D = Ix6;
    const NDIM: usize = 6;
    fn get(&self, index: usize) -> isize {
        self[index]
    }
    fn set(&mut self, index: usize, value: isize) {
        self[index] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
