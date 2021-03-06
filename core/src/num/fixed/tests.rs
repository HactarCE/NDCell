use proptest::prelude::*;
use std::convert::TryInto;

use super::*;

const TINY: f64 = 0.001;

macro_rules! assert_approx_eq {
    ($leeway_factor:expr, $expected:expr, $actual:expr) => {
        let expected: f64 = $expected;
        let actual: FixedPoint = $actual;
        // Skip if `f64` can't handle it.
        let allowed_error = TINY * ($leeway_factor + expected.abs());
        if expected.is_finite() && allowed_error.is_finite() {
            assert!(
                (expected - actual.to_f64().unwrap()).abs() < allowed_error,
                "{:.024} != {:.024} from {:.024}; diff is {:.024}; tolerance is {:.024}",
                expected,
                actual.to_f64().unwrap(),
                actual,
                (expected - actual.to_f64().unwrap()).abs(),
                allowed_error,
            );
        }
    };
}

proptest! {
    #[test]
    fn test_fixed_point_rounding(a: f64) {
        let a = (a * (2.0_f64.powi(16))).round() / (2.0_f64.powi(16));
        prop_assume!(a.is_finite());
        let fa: FixedPoint = a.try_into().unwrap();
        assert_eq!(a.trunc(), fa.trunc().to_f64().unwrap());
        assert_eq!(a.fract(), fa.fract());
        assert_eq!(a.floor(), fa.floor().to_f64().unwrap());
        assert_eq!(a.ceil(), fa.ceil().to_f64().unwrap());
        assert_eq!(a.round(), fa.round().to_f64().unwrap());
    }

    #[test]
    fn test_fixed_point_self_ops(a: f64, b: f64) {
        let a = (a * (2.0_f64.powi(16))).round() / (2.0_f64.powi(16));
        let b = (b * (2.0_f64.powi(16))).round() / (2.0_f64.powi(16));
        prop_assume!(a.is_finite());
        prop_assume!(b.is_finite());

        let leeway_factor = 1.0 + a.abs() + b.abs();
        prop_assume!(leeway_factor.is_finite());

        println!("a = {:.024}; b = {:.024}", a, b);

        let fa: FixedPoint = a.try_into().unwrap();
        let fb: FixedPoint = b.try_into().unwrap();
        println!("fa = {:.024}; fb = {:.024}", fa, fb);
        println!("fa = {:?}; fb = {:?}", fa, fb);

        assert_approx_eq!(leeway_factor, a, fa.clone());
        assert_approx_eq!(leeway_factor, b, fb.clone());

        assert_approx_eq!(leeway_factor, a + b, &fa + &fb);
        assert_approx_eq!(leeway_factor, a - b, &fa - &fb);
        assert_approx_eq!(leeway_factor, a * b, &fa * &fb);

        if b.abs() > TINY {
            assert_approx_eq!(leeway_factor, a / b, &fa / &fb);
            assert_approx_eq!(leeway_factor, a % b, &fa % &fb);
        }

        assert_approx_eq!(leeway_factor, -a, -&fa);
        assert_approx_eq!(leeway_factor, -b, -&fb);

        if a.abs() < 30.0 {
            assert_approx_eq!(leeway_factor, 2.0_f64.powf(a), fa.exp2());
        }
        if let Some(result) = FixedPoint::from_f64(fa.log2()) {
            assert_approx_eq!(leeway_factor, a.log2(), result);
        } else {
            assert!(!a.log2().is_finite())
        }
    }
}
