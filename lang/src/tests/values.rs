use super::LangInt;

pub fn test_values<T: TestValues>() -> &'static [T] {
    T::test_values()
}

pub trait TestValues: Sized {
    /// Returns corner case values to be used as inputs for testing.
    fn test_values() -> &'static [Self];
}

impl TestValues for LangInt {
    fn test_values() -> &'static [Self] {
        &[
            LangInt::MIN,
            LangInt::MIN + 1,
            -2,
            -1,
            0,
            1,
            2,
            LangInt::MAX - 1,
            LangInt::MAX,
        ]
    }
}

impl TestValues for Option<LangInt> {
    fn test_values() -> &'static [Self] {
        &[
            None,
            Some(LangInt::MIN),
            Some(LangInt::MIN + 1),
            Some(-2),
            Some(-1),
            Some(0),
            Some(1),
            Some(2),
            Some(LangInt::MAX - 1),
            Some(LangInt::MAX),
        ]
    }
}
