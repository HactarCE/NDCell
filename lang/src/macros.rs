//! Macros for this crate.

/// Automatically implements std::fmt::Display and std::str::FromStr on an enum
/// using custom-specified string representations.
#[macro_export]
macro_rules! enum_with_str_repr {
    (
        // For each enum ...
        $(
            // Get enum attributes (including #[derive] and docs).
            $(#[$enum_attr:meta])*
            // Get enum definition line.
            pub enum $enum_name:ident {
                // For each variant ...
                $(
                    // Get variant attributes (including docs).
                    $(#[$variant_attr:meta])*
                    // Get the variant name and its string representation.
                    $variant_name:ident = $variant_str:expr
                ),*
                $(,)?
            }
        )*
    ) => {
        // For each enum ...
        $(
            $(#[$enum_attr])*
            pub enum $enum_name {
                $(
                    $(#[$variant_attr])*
                    $variant_name,
                )*
            }
            impl std::fmt::Display for $enum_name {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
                    match self {
                        $(
                            <$enum_name>::$variant_name => write!(f, "{}", $variant_str),
                        )*
                    }
                }
            }
            impl std::str::FromStr for $enum_name {
                type Err = ();
                fn from_str(s: &str) -> Result<Self, ()> {
                    match s {
                        $(
                            $variant_str => Ok(<$enum_name>::$variant_name),
                        )*
                        _ => Err(()),
                    }
                }
            }
        )*
    }
}
