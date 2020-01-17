use num::integer::Integer;
use proptest::prelude::*;

use super::*;

impl proptest::arbitrary::Arbitrary for IVec2D {
    type Parameters = Option<isize>;
    type Strategy = BoxedStrategy<Self>;
    fn arbitrary_with(max: Option<isize>) -> Self::Strategy {
        let max = max.unwrap_or(100);
        prop::collection::vec(-max..=max, 2)
            .prop_flat_map(|v| Just(ndvec![v[0], v[1]]))
            .boxed()
    }
}

impl proptest::arbitrary::Arbitrary for IVec3D {
    type Parameters = Option<isize>;
    type Strategy = BoxedStrategy<Self>;
    fn arbitrary_with(max: Option<isize>) -> Self::Strategy {
        let max = max.unwrap_or(100);
        prop::collection::vec(-max..=max, 3)
            .prop_flat_map(|v| Just(ndvec![v[0], v[1], v[2]]))
            .boxed()
    }
}

proptest! {
    /// Tests various vector operations.
    #[test]
    fn test_ops(
        pos1: IVec3D,
        pos2: IVec3D,
        scalar in -100..=100isize,
        shift in 0..10isize,
    ) {
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
    }
}
