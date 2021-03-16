//! Error reporting functionality for compilation and runtime.

use codemap::{Span, Spanned};
use std::borrow::Cow;
use std::fmt;

use crate::data::{Type, TypeClass, MAX_VECTOR_LEN};
use crate::{MAX_NDIM, MAX_STATE_COUNT};

pub const NO_RUNTIME_REPRESENTATION: &str = "Type has no runtime representation!";

/// `Result` type alias for NDCA compile-time and runtime errors.
pub type Result<T> = std::result::Result<T, Error>;

/// Extension trait for converting a value to an `Option<Span>`.
pub trait SpanConvertExt {
    /// Converts the value to an `Option<Span>`.
    fn to_span(&self) -> Option<Span>;
}
impl<T: SpanConvertExt> SpanConvertExt for &T {
    fn to_span(&self) -> Option<Span> {
        (*self).to_span()
    }
}
impl SpanConvertExt for Option<Span> {
    fn to_span(&self) -> Option<Span> {
        *self
    }
}
impl SpanConvertExt for Span {
    fn to_span(&self) -> Option<Span> {
        Some(*self)
    }
}

/// Whether a problem is an error or a warning.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Severity {
    /// Problem that prevents running the code.
    Error,
    /// Problem that does not prevent running the code.
    Warning,
}

macro_rules! error_fn {
    ($severity:ident; fn $fn_name:ident(
        $fmt_str:tt $(,
            $arg_name:ident: $arg_type:ty
            $(=> $lambda:expr)?
        )* $(,
            _ => $fmt_expr:expr
        )* $(,)?
    )) => {
        paste! {
            #[doc = "Returns an error with the template `" $fmt_str "`."]
            pub(crate) fn $fn_name(span: impl SpanConvertExt $(, $arg_name: $arg_type)*) -> Error {
                Error {
                    msg: format!(
                        $fmt_str $(,
                            error_fn!(@@ $arg_name $(=> $lambda)?)
                        )* $(,
                            $fmt_expr
                        )*
                    ),
                    span: span.to_span(),
                    severity: Severity::$severity,
                }
            }
        }
    };

    (@@ $arg_name:ident) => {
        $arg_name
    };
    (@@ $arg_name:ident => $lambda:expr) => {
        ($lambda)($arg_name)
    };
}

/// Error or warning in user code.
#[derive(Debug, Clone)]
pub struct Error {
    /// Error message.
    pub msg: String,
    /// Location of error in source code (if any).
    pub span: Option<Span>,
    /// Severity of error.
    pub severity: Severity,
}
impl Error {
    /// Attaches a span to the error, if it does not already have one.
    pub fn with_span(mut self, span: impl SpanConvertExt) -> Self {
        if self.span.is_none() {
            self.span = span.to_span();
        }
        self
    }

    // Miscellaneous errors

    error_fn!(Error; fn unimplemented(
        "This feature is unimplemented",
    ));

    error_fn!(Error; fn internal(
        "{}", msg: impl fmt::Display,
    ));

    // Compile errors

    error_fn!(Error; fn unterminated(
        "This {} never ends",
        thing: impl fmt::Display,
    ));

    error_fn!(Error; fn invalid_symbol(
        "Invalid symbol",
    ));

    error_fn!(Error; fn expected(
        "Expected {}",
        expected: impl fmt::Display => crate::utils::a,
    ));

    error_fn!(Error; fn name_in_use(
        "This name is already in use",
    ));

    error_fn!(Warning; fn name_in_use_by_builtin(
        "This hides a built-in with the same name",
    ));

    error_fn!(Error; fn reserved_word(
        "This is a reserved word",
    ));

    error_fn!(Error; fn cannot_resolve_name(
        "Can't find anything with this name that is accessible from here",
    ));

    error_fn!(Error; fn else_without_if(
        "This 'else' has no matching 'if'",
    ));

    error_fn!(Error; fn invalid_integer_literal(
        "Can't parse this; it looks like an integer literal, but {}",
        integer_parse_error: impl fmt::Display,
    ));

    error_fn!(Error; fn ambiguous_octothorpe(
        "This is ambiguous; if it is a tag name, remove the space after '#'; if it is a variable name, wrap it in parentheses",
    ));

    error_fn!(Error; fn missing_directive(
        "Directive '{}' is required, but not present",
        directive: impl fmt::Display,
    ));

    error_fn!(Error; fn invalid_directive_name(
        "This isn't a valid directive name",
    ));

    error_fn!(Error; fn invalid_directive_name_with_suggestion(
        "This isn't a valid directive name; did you mean '{}'",
        suggested: impl fmt::Display,
    ));

    error_fn!(Error; fn duplicate_directive(
        "There can only be one '{}' directive; this is a duplicate",
        directive: impl fmt::Display,
    ));

    error_fn!(Error; fn dependency_cycle(
        "Cyclic dependency in '{}' directive",
        directive: impl fmt::Display,
    ));

    error_fn!(Error; fn invalid_dimension_count(
        "The number of dimensions must be an integer from 1 to {}",
        _ => MAX_NDIM,
    ));

    error_fn!(Error; fn invalid_state_count(
        "The number of states must be an integer from 1 to {}",
        _ => MAX_STATE_COUNT,
    ));

    error_fn!(Error; fn cannot_const_eval(
        "Cannot evaluate this at compile-time",
    ));

    error_fn!(Error; fn type_error(
        "Mismatched types: expected {} but got {}",
        expected: impl fmt::Display,
        got: &Type,
    ));

    error_fn!(Error; fn cmp_type_error(
        "Mismatched types: cannot compare {0} to {2} using '{1}'{3}",
        lhs: &Type,
        cmp: &str,
        rhs: &Type,
        _ => if *lhs == Type::Cell && *rhs == Type::Cell {
            "; convert them to integers first using the '#id' tag"
        } else {
            ""
        },
    ));

    error_fn!(Error; fn invalid_arguments(
        "These arguments have types {1}, which are invalid for {0}",
        name: String,
        arg_types: &[Type] => crate::utils::display_bracketed_list,
    ));

    error_fn!(Error; fn cannot_index_type(
        "Cannot index into value of type {}",
        ty: Type,
    ));

    error_fn!(Error; fn cannot_assign_to_expr(
        "This expression cannot have a value assigned to it",
    ));

    error_fn!(Error; fn return_in_transition_fn(
        "'return' is not allowed in the transition function; use 'become' or 'remain' instead",
    ));

    error_fn!(Error; fn become_or_remain_in_helper_fn(
        "'become' and 'remain' are not allowed outside the transition function; use 'return' instead",
    ));
}

/// Handles internal errors in the NDCA compiler. Panics in debug mode, but
/// returns a nice error message in release mode (so that the program doesn't
/// immediately crash on the user).
///
/// Prefer internal_error!(); be careful not to call this and then throw away
/// the error it returns, because in debug mode it will still panic.
macro_rules! internal_error_value {
    // Automatically format arguments.
    ( $( $args:expr ),+ $(,)? ) => {{
        #[allow(unused)]
        let ret: crate::errors::Error;

        if cfg!(debug_assertions) {
            // Panic in a debug build (for stack trace).
            panic!($( $args ),+)

        } else {
            // Give a nice error message for the user in a release build.
            let msg: String = format!($( $args ),+).into();
            crate::errors::Error::internal(None, msg)
        }
    }};
}

/// Handles internal errors in the NDCA compiler. Panics in debug mode, but
/// returns a nice error message in release mode (so that the program doesn't
/// immediately crash on the user).
///
/// Note that this macro actually returns the error from the caller; it does not just provide the value.
macro_rules! internal_error {
    ( $( $args:expr ),+ $(,)? ) => {
        return Err(internal_error_value!($( $args ),+))
    };
}

// Emits an error for when an argument index is out of range (which should never
// happen).
macro_rules! arg_out_of_range {
    () => {
        internal_error!("Argument index out of range")
    };
}

// Emits an error for when a TypeError occurs in a place where it should have
// already been caught.
macro_rules! uncaught_type_error {
    () => {
        internal_error!("Uncaught type error")
    };
    (in $loc:ident) => {
        internal_error!(concat!("Uncaught type error in ", stringify!($loc)))
    };
}
