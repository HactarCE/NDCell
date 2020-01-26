//! Miscellaneous math functions

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

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;

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
    }
}
