//! Macros for this crate.

/// Automatically implements std::fmt::Display and std::str::FromStr on an enum
/// using custom-specified string representations.
macro_rules! enum_with_str_repr {
    (
        // For each enum ...
        $(
            // Get enum attributes (including #[derive] and docs).
            $(#[$enum_attr:meta])*
            // Get enum definition line.
            $vis:vis enum $enum_name:ident {
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
            $vis enum $enum_name {
                $(
                    $(#[$variant_attr])*
                    $variant_name,
                )*
            }
            impl std::fmt::Display for $enum_name {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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

/// Check that the given value matches any one of the given types, returning
/// Ok(()) if it does match and a type error if it does not match.
macro_rules! typecheck {
    ($got:expr, [ $( $expected:tt ),+ $(,)? ]) => {
        $got.typecheck([ $( get_type_desc!($expected) ),+ ].as_ref())
    };
    ($got:expr, $expected:tt) => {
        $got.typecheck(get_type_desc!($expected))
    };
}

/// Convert type names (such as "Int") or type description name (such as
/// "Vector" or "Pattern") into a TypeDesc.
macro_rules! get_type_desc {
    (Vector) => {
        crate::types::TypeDesc::Vector
    };
    (Pattern) => {
        crate::types::TypeDesc::Pattern
    };
    (Rectangle) => {
        crate::types::TypeDesc::Rectangle
    };
    ($other:tt) => {
        crate::types::TypeDesc::Specific(crate::types::Type::$other)
    };
}
