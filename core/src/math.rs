//! Math utilities.

use num::{BigInt, Signed};

use crate::{BigVec, Dim};

/// Returns an iterator of points along a line between `start` and `end`
/// generated using an N-dimensional generalized version of Bresenham's
/// line-drawing algorithm.
///
/// Note that the points may be in reverse order (i.e. from `end` to `start`).
pub fn bresenham<D: Dim>(
    mut start: BigVec<D>,
    mut end: BigVec<D>,
) -> impl Iterator<Item = BigVec<D>> {
    let mut delta = &end - &start;

    let longest_axis = delta.max_axis(|_, val| val.abs());
    // Ensure that delta[longest_axis] is positive.
    let negate = delta[longest_axis].is_negative();
    if negate {
        start = -start;
        end = -end;
        delta = -delta;
    }

    let longest_axis_range = num::range_inclusive(BigInt::from(0), delta[longest_axis].clone());

    let double_delta = &delta * &BigInt::from(2);
    let double_longest_axis_len = double_delta[longest_axis].clone();

    let mut current = start;
    let mut error = delta.signum() * BigVec::repeat(delta[longest_axis].clone());
    longest_axis_range.map(move |_| {
        let ret = current.clone();
        if ret != end {
            error += &double_delta;
            current += error.div_to_zero(&double_longest_axis_len);
            error = error.rem_to_zero(&double_longest_axis_len);
        }
        if negate {
            -ret
        } else {
            ret
        }
    })
}

#[cfg(test)]
mod tests {
    use noisy_float::prelude::r64;
    use proptest::prelude::*;

    use crate::space::*;

    use super::*;

    fn test_bresenham_single_line<D: Dim>(start: IVec<D>, end: IVec<D>) {
        let big_start: BigVec<D> = start.to_bigvec();
        let big_end: BigVec<D> = end.to_bigvec();

        if big_start == big_end {
            let points: Vec<BigVec<D>> = bresenham(big_start.clone(), big_end).collect();
            assert_eq!(vec![big_start], points);
            return;
        }

        let rect = NdRect::span(big_start.clone(), big_end.clone());

        let f_start: FVec<D> = start.to_fvec();
        let f_end: FVec<D> = end.to_fvec();

        let mut delta = f_end - f_start.clone();
        let longest_axis = delta.max_axis(|_, val| val.abs());
        delta /= delta[longest_axis].abs();
        let mut f_current = f_start.clone();
        let mut max_positive_error = r64(0.0);
        let mut max_negative_error = r64(0.0);
        let mut last_point = None;
        for p in bresenham(big_start, big_end.clone()) {
            println!("integer {}", p);
            println!("float   {}", &f_current);
            let diff = p.to_fvec() - f_current.clone();
            println!("diff    {}", diff);
            println!();

            assert!(rect.contains(&p));

            max_positive_error =
                std::cmp::max(max_positive_error, diff[diff.max_axis(|_, val| *val)]);
            max_negative_error =
                std::cmp::min(max_negative_error, diff[diff.min_axis(|_, val| *val)]);

            f_current += delta.clone();

            last_point = Some(p);
        }
        println!("max positive error = {}", max_positive_error);
        println!("max negative error = {}", max_negative_error);
        assert!(max_positive_error < 0.51);
        assert!(max_negative_error > -0.51);

        assert_eq!(Some(big_end), last_point, "Line did not complete");
    }

    proptest! {
        /// Tests the line-drawing algorithm by comparing it against a floating
        /// point implementation.
        #[test]
        fn test_bresenham(start: IVec3D, end: IVec3D) {
            test_bresenham_single_line(start, end);
        }
    }
}
