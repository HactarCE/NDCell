/// Matches based on a dimensionality type parameter.
#[macro_export]
macro_rules! match_ndim {
    (match $dim:ty {
        1 => $case1:expr,
        2 => $case2:expr,
        3 => $case3:expr,
        4 => $case4:expr,
        5 => $case5:expr,
        6 => $case6:expr $(,)?
    }) => {
        match <$dim as $crate::Dim>::NDIM {
            1 => $case1,
            2 => $case2,
            3 => $case3,
            4 => $case4,
            5 => $case5,
            6 => $case6,
            _ => {
                #[cfg(debug_assertions)]
                unreachable!("NDIM greater than 6");

                #[cfg(not(debug_assertions))]
                unsafe {
                    std::hint::unreachable_unchecked!();
                }
            }
        }
    };
}
