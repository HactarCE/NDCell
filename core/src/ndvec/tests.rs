use noisy_float::prelude::r64;
use num::integer::Integer;
use proptest::prelude::*;

use super::*;

use crate::dim::Dim3D;
use crate::Axis::{X, Y, Z};

#[test]
fn test_ndvec_macro() {
    let v1: IVec3D = NdVec([2, 10, -3]);
    assert_eq!(2, v1[X]);
    assert_eq!(10, v1[Y]);
    assert_eq!(-3, v1[Z]);

    let v2: IVec3D = NdVec([6; 3]);
    assert_eq!(6, v2[X]);
    assert_eq!(6, v2[Y]);
    assert_eq!(6, v2[Z]);
}

#[test]
fn test_bigvec() {
    let x = BigInt::from(1_i64 << 62);
    let y = BigInt::from(-(1_i64 << 62));
    let z = BigInt::from(10);
    let scalar = BigInt::from(1) << 20;
    let mut v1: BigVec3D = NdVec([x.clone(), y.clone(), z.clone()]);
    v1 *= &scalar;
    v1 = v1.div_floor(&scalar);
    assert_eq!(x, v1[X]);
    assert_eq!(y, v1[Y]);
    assert_eq!(z, v1[Z]);
}

#[test]
fn test_fvec() {
    let x = r64(0.25);
    let y = r64(2.0);
    let z = r64(-4.5);
    let scalar = 1.6;
    let mut v1: FVec3D = NdVec([x, y, z]);
    v1 /= scalar;
    assert_eq!(x / scalar, v1[X]);
    assert_eq!(y / scalar, v1[Y]);
    assert_eq!(z / scalar, v1[Z]);
}

impl proptest::arbitrary::Arbitrary for IVec2D {
    type Parameters = Option<isize>;
    type Strategy = BoxedStrategy<Self>;
    fn arbitrary_with(max: Option<isize>) -> Self::Strategy {
        let max = max.unwrap_or(100);
        prop::collection::vec(-max..=max, 2)
            .prop_flat_map(|v| Just(NdVec([v[0], v[1]])))
            .boxed()
    }
}

impl proptest::arbitrary::Arbitrary for IVec3D {
    type Parameters = Option<isize>;
    type Strategy = BoxedStrategy<Self>;
    fn arbitrary_with(max: Option<isize>) -> Self::Strategy {
        let max = max.unwrap_or(100);
        prop::collection::vec(-max..=max, 3)
            .prop_flat_map(|v| Just(NdVec([v[0], v[1], v[2]])))
            .boxed()
    }
}

proptest! {
    /// Tests various vector operations.
    #[test]
    fn test_ndvec_ops(
        pos1: IVec3D,
        pos2: IVec3D,
        scalar in -100..=100_isize,
        shift in 0..10_isize,
    ) {
        // Test operations.
        for &ax in Dim3D::axes() {
            assert_eq!(-(pos1[ax]), (-pos1)[ax]);
            assert_eq!(pos1[ax] + pos2[ax],   (pos1 + pos2  )[ax]);
            assert_eq!(pos1[ax] - pos2[ax],   (pos1 - pos2  )[ax]);
            assert_eq!(pos1[ax] * pos2[ax],   (pos1 * pos2  )[ax]);
            assert_eq!(pos1[ax] + scalar,     (pos1 + scalar)[ax]);
            assert_eq!(pos1[ax] - scalar,     (pos1 - scalar)[ax]);
            assert_eq!(pos1[ax] * scalar,     (pos1 * scalar)[ax]);
            if scalar != 0 {
                assert_eq!(pos1[ax].div_floor(&scalar), (pos1.div_floor(&scalar))[ax]);
                assert_eq!(pos1[ax].mod_floor(&scalar), (pos1.mod_floor(&scalar))[ax]);
            }
            assert_eq!(pos1[ax] & scalar,     (pos1 & scalar)[ax]);
            assert_eq!(pos1[ax] | scalar,     (pos1 | scalar)[ax]);
            assert_eq!(pos1[ax] ^ scalar,     (pos1 ^ scalar)[ax]);
            assert_eq!(pos1[ax] << shift,     (pos1 << shift)[ax]);
            assert_eq!(pos1[ax] >> shift,     (pos1 >> shift)[ax]);
        }
        let mut result;
        result = pos1; result += pos2;   assert_eq!(result, pos1 + pos2);
        result = pos1; result -= pos2;   assert_eq!(result, pos1 - pos2);
        result = pos1; result *= pos2;   assert_eq!(result, pos1 * pos2);
        result = pos1; result += scalar; assert_eq!(result, pos1 + scalar);
        result = pos1; result -= scalar; assert_eq!(result, pos1 - scalar);
        result = pos1; result *= scalar; assert_eq!(result, pos1 * scalar);
        result = pos1; result &= scalar; assert_eq!(result, pos1 & scalar);
        result = pos1; result |= scalar; assert_eq!(result, pos1 | scalar);
        result = pos1; result ^= scalar; assert_eq!(result, pos1 ^ scalar);
        result = pos1; result <<= shift; assert_eq!(result, pos1 << shift);
        result = pos1; result >>= shift; assert_eq!(result, pos1 >> shift);

        // Test comparison.
        let cmp_x = pos1[X].partial_cmp(&pos2[X]);
        let cmp_y = pos1[Y].partial_cmp(&pos2[Y]);
        let cmp_z = pos1[Z].partial_cmp(&pos2[Z]);
        if cmp_x == cmp_y && cmp_y == cmp_z {
            assert_eq!(cmp_x, pos1.partial_cmp(&pos2));
        } else {
            assert_eq!(None, pos1.partial_cmp(&pos2));
        }
    }
}
