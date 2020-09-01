/// Matches based on a dimensionality type parameter.
///
/// # Examples
///
/// ```
/// # use ndcell_core::{Dim, match_ndim};
/// fn f<D: Dim>() {
///     match_ndim!(match D {
///         1 => (/* 1 dimension  */),
///         2 => (/* 2 dimensions */),
///         3 => (/* 3 dimensions */),
///         4 => (/* 4 dimensions */),
///         5 => (/* 5 dimensions */),
///         6 => (/* 6 dimensions */),
///
///         // No "default" case needed!
///     })
/// }
/// ```
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
        match <$dim as $crate::dim::Dim>::NDIM {
            1 => $case1,
            2 => $case2,
            3 => $case3,
            4 => $case4,
            5 => $case5,
            6 => $case6,
            _ => unreachable!("NDIM greater than 6"),
        }
    };
}
