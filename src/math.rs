//! Miscellaneous math functions

use noisy_float::prelude::r64;
use num::{BigInt, Signed};

use crate::automaton::{BigVec, Dim};

/// Computes the base-2 logarithm of a number, rounded up.
///
/// Put another way: returns the smallest number `p` (minimum `0`) such that `n <= 2
/// ** p`.
pub fn ceil_log_base_2(mut n: usize) -> usize {
    if n <= 1 {
        return 0;
    }
    n -= 1;
    let total_bytes = std::mem::size_of::<usize>();
    let total_bits = total_bytes * 8;
    let real_bits = total_bits - n.leading_zeros() as usize;
    real_bits
}

/// Rounds a number to the nearest multiple of another number.
pub fn round_to(n: f32, m: f32) -> f32 {
    (n / m).round() * m
}

/// Returns an iterator that uses a generalized version of Bresenham's
/// line-drawing algorithm to return the integer points on a line between the
/// given points.
pub fn bresenham<D: Dim>(start: BigVec<D>, end: BigVec<D>) -> impl Iterator<Item = BigVec<D>> {
    let delta = &end - &start;
    let longest_axis = delta.max_axis(|_, val| val.abs());
    let longest_axis_range =
        num::range_inclusive(start[longest_axis].clone(), end[longest_axis].clone());
    let double_delta = &delta * &BigInt::from(2);
    let double_longest_axis_len = double_delta[longest_axis].clone();
    let mut current = start;
    let mut error = BigVec::repeat(delta[longest_axis].clone()) * delta.signum();
    // error[longest_axis] = BigInt::from(0); // TODO: explain why this is necessary
    longest_axis_range.map(move |_| {
        let ret = current.clone();
        println!("error was {}", error);
        error += &double_delta;
        println!(
            "next one will gain {}",
            error.div_to_zero(&double_longest_axis_len)
        );
        current += error.div_to_zero(&double_longest_axis_len);
        error = error.rem_to_zero(&double_longest_axis_len);
        ret
    })
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;
    use crate::automaton::space::*;

    fn test_bresenham_single_line<D: Dim>(start: IVec<D>, end: IVec<D>) {
        let big_start: BigVec<_> = start.convert();
        let big_end: BigVec<_> = end.convert();
        let rect = NdRect::span(big_start.clone(), big_end.clone());

        let f_start: FVec<_> = start.as_fvec();
        let f_end: FVec<_> = end.as_fvec();

        let mut delta = f_end - f_start.clone();
        let longest_axis = delta.max_axis(|_, val| val.abs());
        delta /= delta[longest_axis];
        let mut f_current = f_start.clone();
        let mut max_positive_error = r64(0.0);
        let mut max_negative_error = r64(0.0);
        for p in bresenham(big_start, big_end) {
            println!("integer {}", p);
            println!("float   {}", &f_current);
            let diff = p.as_fvec() - f_current.clone();
            println!("diff    {}", diff);
            println!();

            // assert!(rect.contains(&p));

            max_positive_error =
                std::cmp::max(max_positive_error, diff[diff.max_axis(|_, val| *val)]);
            max_negative_error =
                std::cmp::min(max_negative_error, diff[diff.min_axis(|_, val| *val)]);

            f_current += delta.clone();
        }
        println!("max positive error = {}", max_positive_error);
        println!("max negative error = {}", max_negative_error);
        assert!(max_positive_error < 0.51);
        assert!(max_negative_error > -0.51);
    }

    proptest! {
        #[test]
        fn test_ceil_log_base_2(n in 0..10000usize) {
            let p = ceil_log_base_2(n);
            if n <= 1 {
                assert_eq!(0, p)
            } else {
                assert!(n <= 1 << p);
                assert!(n > 1 << (p - 1));
            }
        }

        /// Test the line-drawing algorithm by comparing it against a
        /// floating point implementation.
        #[test]
        fn test_bresenham(start: IVec3D, end: IVec3D) {
            prop_assume!(start != end);
            test_bresenham_single_line(start, end);
        }
    }
}
