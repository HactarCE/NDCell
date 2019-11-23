//! Miscellaneous math functions

/// Computes the base-2 logarithm of a number, rounded up.
///
/// Put another way: returns the smallest number `p` (minimum `0`) such that `n <= 2
/// ** p`.
pub fn ceil_log_base_2(mut n: usize) -> usize {
    n -= 1;
    let total_bytes = std::mem::size_of::<usize>();
    let total_bits = total_bytes * 8;
    let real_bits = total_bits - n.leading_zeros() as usize;
    real_bits
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

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
