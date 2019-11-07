use delegate::delegate;
use std::cmp::Eq;
use std::hash::Hash;
use std::ops::*;

use super::Vector;

/// A vector index for a chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ChunkVector<V: Vector>(V);

/// A vector index for a cell.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CellVector<V: Vector>(V);

/// A vector index for a cell within a chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

/// Given the name of a tuple struct whose only type parameter is a Vector type
/// and whose first parameter is an instance of that Vector type, implement
/// Vector by delegating to the methods of the contained Vector instance.
macro_rules! delegate_vector_impl {
    ($vector_container:ident) => {
        // Implement generic vector operations.
        impl<V: Vector> Vector for $vector_container<V> {
            type D = V::D;
            const NDIM: usize = V::NDIM;
            const CHUNK_BITS: usize = V::CHUNK_BITS;
            const CHUNK_SIZE: usize = V::CHUNK_SIZE;
            delegate! {
                target self.0 {
                    fn get(&self, axis: usize) -> isize;
                    fn set(&mut self, axis: usize, value: isize);
                }
            }
            fn origin() -> Self {
                Self(V::origin())
            }
        }
        // Implement elementwise addition between two vectors.
        impl<V: Vector> Add<Self> for $vector_container<V> {
            type Output = Self;
            fn add(self, other: Self) -> Self {
                let mut ret = self;
                ret += other;
                ret
            }
        }
        impl<V: Vector> AddAssign<Self> for $vector_container<V> {
            fn add_assign(&mut self, other: Self) {
                for i in 0..Self::NDIM {
                    self.set(i, self.get(i) + other.get(i));
                }
            }
        }
        // Imlement addition between a vector and a scalar (i.e. add the scalar
        // to each axis of the vector).
        impl<V: Vector> Add<isize> for $vector_container<V> {
            type Output = Self;
            fn add(self, other: isize) -> Self {
                let mut ret = self;
                ret += other;
                ret
            }
        }
        impl<V: Vector> AddAssign<isize> for $vector_container<V> {
            fn add_assign(&mut self, other: isize) {
                for i in 0..Self::NDIM {
                    self.set(i, self.get(i) + other);
                }
            }
        }
        // Implement elementwise subtraction between two vectors.
        impl<V: Vector> Sub<Self> for $vector_container<V> {
            type Output = Self;
            fn sub(self, other: Self) -> Self {
                let mut ret = self;
                ret -= other;
                ret
            }
        }
        impl<V: Vector> SubAssign<Self> for $vector_container<V> {
            fn sub_assign(&mut self, other: Self) {
                for i in 0..Self::NDIM {
                    self.set(i, self.get(i) - other.get(i));
                }
            }
        }
        // Implement subtraction between a vector and a scalar (i.e. subtract
        // the scalar from each axis of the vector).
        impl<V: Vector> Sub<isize> for $vector_container<V> {
            type Output = Self;
            fn sub(self, other: isize) -> Self {
                let mut ret = self;
                ret -= other;
                ret
            }
        }
        impl<V: Vector> SubAssign<isize> for $vector_container<V> {
            fn sub_assign(&mut self, other: isize) {
                for i in 0..Self::NDIM {
                    self.set(i, self.get(i) - other);
                }
            }
        }
        // Implement multiplication between a vector and a scalar (i.e. multiply
        // each element of the vector by the scalar).
        impl<V: Vector> Mul<isize> for $vector_container<V> {
            type Output = Self;
            fn mul(self, other: isize) -> Self {
                let mut ret = self;
                ret *= other;
                ret
            }
        }
        impl<V: Vector> MulAssign<isize> for $vector_container<V> {
            fn mul_assign(&mut self, other: isize) {
                for i in 0..Self::NDIM {
                    self.set(i, self.get(i) * other);
                }
            }
        }
        // Implement integer division between a vector and a scalar (i.e. divide
        // each element of the vector by the scalar).
        impl<V: Vector> Div<isize> for $vector_container<V> {
            type Output = Self;
            fn div(self, other: isize) -> Self {
                let mut ret = self;
                ret /= other;
                ret
            }
        }
        impl<V: Vector> DivAssign<isize> for $vector_container<V> {
            fn div_assign(&mut self, other: isize) {
                for i in 0..Self::NDIM {
                    self.set(i, self.get(i) / other);
                }
            }
        }
        // Implement negation of a vector (i.e. negate each axis).
        impl<V: Vector> Neg for $vector_container<V> {
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

// Implement Vector for each of these structs.
delegate_vector_impl!(CellVector);
delegate_vector_impl!(ChunkVector);
delegate_vector_impl!(LocalVector);

impl<V: Vector> LocalVector<V> {
    /// Convert the vector to an ndindex.
    ///
    /// Panics if any values are negative.
    pub fn ndindex(&self) -> V::D {
        use ndarray::prelude::*;

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

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::proptest;

    proptest! {
        /// Test vector arithmetic against ndarray.
        #[test]
        fn test_ops(v1 in [50..=100isize, 50..=100isize, 50..=100isize], v2 in [0..=50isize, 0..=50isize, 0..=50isize], scalar in 0..=50usize) {
            let v1: LocalVector<[isize; 3]> = v1.into();
            let v2: LocalVector<[isize; 3]> = v2.into();
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
            assert_eq!(LocalVector::from([0, 0, 0]), v1 - v1);
            assert_eq!(LocalVector::from([0, 0, 0]), v2 - v2);
            assert_eq!(-v1, v1 - v1 * 2);
            assert_eq!(-v2, v2 - v2 * 2);
        }
    }
}
