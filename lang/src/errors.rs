//! Error reporting functionality for compilation and runtime.

// TODO: reword error messages to state _what is wrong_ in addition to what is
// required.

use codemap::Span;
pub use codemap_diagnostic::Diagnostic;
use codemap_diagnostic::{Level, SpanLabel, SpanStyle};
use itertools::Itertools;
use std::fmt;

use crate::data::{Type, MAX_VECTOR_LEN, MAX_VECTOR_SET_EXTENT, MAX_VECTOR_SET_SIZE};
use crate::{MAX_NDIM, MAX_STATE_COUNT};

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

macro_rules! error_fn {
    ($level:ident; fn $fn_name:ident(
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
                Error::New(Diagnostic {
                    level: Level::$level,
                    message: format!(
                        $fmt_str $(,
                            error_fn!(@@ $arg_name $(=> $lambda)?)
                        )* $(,
                            $fmt_expr
                        )*
                    ),
                    code: None,
                    spans: span
                        .to_span()
                        .into_iter()
                        .map(|span| SpanLabel {
                            span,
                            label: None,
                            style: SpanStyle::Primary,
                        })
                        .collect_vec(),
                })
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

/// Handles internal errors in the NDCA compiler. Panics in debug mode, but
/// returns a nice error message in release mode so that the program doesn't
/// immediately crash.
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
/// Note that this macro actually returns the error from the caller; it does not
/// just provide the value.
macro_rules! internal_error {
    ( $( $args:expr ),+ $(,)? ) => {
        return Err(internal_error_value!($( $args ),+))
    };
}

/// Error or warning in user code.
#[derive(Debug, Clone)]
pub enum Error {
    /// Error that has not yet been added to the error list.
    New(Diagnostic),
    /// Error that has already been added to the error list.
    AlreadyReported,
}
impl Error {
    /// Adds a spanned note to the error message.
    pub fn with_note(mut self, span: Span, msg: impl fmt::Display) -> Self {
        if let Self::New(diag) = &mut self {
            diag.spans.push(SpanLabel {
                span,
                label: Some(msg.to_string()),
                style: SpanStyle::Secondary,
            });
        }
        self
    }
    /// Extracts the diagnostic from the error message or returns an internal
    /// error if there isn't one.
    pub fn unwrap_diagnostic(self) -> Diagnostic {
        match self {
            Error::New(e) => e,
            Error::AlreadyReported => {
                match internal_error_value!("expected diagnostic, but error already reported") {
                    Error::New(e) => e,
                    Error::AlreadyReported => unreachable!(),
                }
            }
        }
    }

    // Syntax/parsing errors
    error_fn!(Error; fn else_without_if("this 'else' has no matching 'if'"));
    error_fn!(Error; fn expected("expected {}", expected: impl fmt::Display => crate::utils::a));
    error_fn!(Error; fn invalid_integer_literal("can't parse this; it looks like an integer literal, but {}", integer_parse_error: impl fmt::Display));
    error_fn!(Error; fn invalid_symbol("invalid symbol"));
    error_fn!(Error; fn reserved_word("this is a reserved word"));
    error_fn!(Error; fn unterminated("this {} never ends", thing: impl fmt::Display));

    // Miscellaneous errors

    error_fn!(Error; fn unimplemented("this feature is unimplemented"));

    error_fn!(Error; fn internal(
        "internal error occurred! This is a bug in NDCell. ({})",
        msg: impl fmt::Display,
    ));

    error_fn!(Error; fn custom("{}", msg: impl fmt::Display)); // TODO: find and remove uses of `Error::custom()`

    // Compile errors

    error_fn!(Error; fn name_in_use("this name is already in use"));

    error_fn!(Warning; fn name_in_use_by_builtin("this hides a built-in with the same name"));

    error_fn!(Error; fn cannot_resolve_name("can't find anything with this name that is accessible from here"));

    error_fn!(Error; fn ambiguous_octothorpe("this is ambiguous; if it is a tag name, remove the space after '#'; if it is a variable name, wrap it in parentheses"));

    error_fn!(Error; fn missing_directive(
        "directive '{}' is required, but not present",
        directive: impl fmt::Display,
    ));

    error_fn!(Error; fn invalid_directive_name("this isn't a valid directive name"));

    error_fn!(Error; fn invalid_directive_name_with_suggestion(
        "this isn't a valid directive name; did you mean '{}'",
        suggested: impl fmt::Display,
    ));

    error_fn!(Error; fn duplicate_directive(
        "there can only be one '{}' directive; this is a duplicate",
        directive: impl fmt::Display,
    ));

    error_fn!(Error; fn dependency_cycle(
        "cyclic dependency in '{}' directive",
        directive: impl fmt::Display,
    ));

    error_fn!(Error; fn invalid_dimension_count(
        "number of dimensions must be an integer from 1 to {}",
        _ => MAX_NDIM,
    ));

    error_fn!(Error; fn invalid_state_count(
        "number of states must be an integer from 1 to {}",
        _ => MAX_STATE_COUNT,
    ));

    error_fn!(Error; fn invalid_vector_length(
        "length of a vector must be an integer from 1 to {}",
        _ => MAX_VECTOR_LEN,
    ));

    error_fn!(Error; fn invalid_set_type(
        "set can only be constructed from values of types Integer, Cell, or Vector with length at most 6; not {}",
        ty: &Type,
    ));

    error_fn!(Error; fn invalid_vector_component_for_set(
        "magnitude of components of a vector in a vector set must not exceed {}",
        _ => MAX_VECTOR_SET_EXTENT,
        // TODO: include value in this error message and others (possible
        // because set must be compile-time constant)
    ));
    error_fn!(Error; fn invalid_vector_set_size(
        "volume of vector set's bounding rectangle must not exceed {}",
        _ => MAX_VECTOR_SET_SIZE,
    ));

    error_fn!(Error; fn must_be_constant("must be a constant"));
    error_fn!(Error; fn cannot_compile("not allowed in compiled code"));
    error_fn!(Error; fn cannot_const_eval("cannot evaluate because evaluation depends on simulation contents"));

    error_fn!(Error; fn type_error(
        "type error: expected {} but got {}",
        expected: impl fmt::Display,
        got: &Type,
    ));
    error_fn!(Error; fn cmp_type_error(
        "type error: cannot compare {1} to {2} using '{0}'{3}",
        cmp: impl fmt::Display,
        lhs: &Type,
        rhs: &Type,
        _ => if *lhs == Type::Cell && *rhs == Type::Cell {
            "; convert them to integers first using the '#id' tag"
        } else {
            ""
        },
    ));

    error_fn!(Error; fn invalid_arguments(
        "'{}' cannot take arguments of types {}",
        name: impl fmt::Display,
        arg_types: &[Type] => crate::utils::display_bracketed_list,
    ));
    error_fn!(Error; fn duplicate_keyword_argument("duplicate keyword argument"));
    error_fn!(Error; fn invalid_keyword_argument("invalid keyword argument for '{}'", func: impl fmt::Display));

    error_fn!(Error; fn not_reached_directive("not yet reached '{}' directive", directive_name: &str));

    error_fn!(Error; fn cannot_index_type(
        "cannot index into value of type {}", // TODO: this is a terrible error message
        ty: &Type,
    ));
    error_fn!(Error; fn cannot_call_arbitrary_expression("cannot call arbitrary expression"));
    error_fn!(Error; fn no_such_function("no such function"));
    error_fn!(Error; fn no_such_method("type {} has no such method or attribute", ty: &Type));

    error_fn!(Error; fn uninitialized_variable("this variable doesn't exist or hasn't been assigned a value"));
    error_fn!(Error; fn maybe_uninitialized_variable("this variable might not have been assigned a value"));
    error_fn!(Error; fn ambiguous_variable_type("this variable's type is ambiguous"));
    error_fn!(Error; fn unknown_variable_value("value of type {} must be compile-time constant", ty: &Type));

    error_fn!(Error; fn cannot_assign_to("cannot assign to this expression"));
    error_fn!(Error; fn cannot_compile_assign_to("cannot assign to this expression in compiled code"));

    error_fn!(Error; fn break_not_in_loop("cannot 'break' when not in a loop"));
    error_fn!(Error; fn continue_not_in_loop("cannot 'continue' when not in a loop"));

    error_fn!(Error; fn return_not_in_fn("cannot 'return' when not in a function"));
    error_fn!(Error; fn become_not_in_fn("cannot 'become' when not in the transition function"));
    error_fn!(Error; fn remain_not_in_fn("cannot 'return' when not in a function"));

    error_fn!(Error; fn return_in_transition_fn("'return' is not allowed in the transition function; use 'become' or 'remain' instead"));
    error_fn!(Error; fn become_in_helper_fn("'become' is only allowed in the transition function; use 'return' instead"));
    error_fn!(Error; fn remain_in_helper_fn("'remain' is only allowed in the transition function; use 'return' instead"));

    error_fn!(Error; fn integer_overflow("integer overflow"));
    error_fn!(Error; fn division_by_zero("division by zero"));
    error_fn!(Error; fn negative_exponent("negative exponent"));
    error_fn!(Error; fn bitshift_out_of_range("bitshift amount out of range"));
    error_fn!(Error; fn index_out_of_bounds("index out of bounds"));
    error_fn!(Error; fn position_out_of_bounds("position out of bounds"));
    error_fn!(Error; fn position_excluded_by_array_mask("position excluded by array mask"));
    error_fn!(Error; fn cell_state_out_of_range("invalid cell state ID"));

    error_fn!(Error; fn invalid_cell_symbol("invalid cell symbol: {:?}", s: &str));
    error_fn!(Error; fn wrong_cell_count("wrong cell cound; expected {} cells but got {}", expected: usize, got: usize));

    error_fn!(Error; fn user_error("error"));
    error_fn!(Error; fn assertion_failed("assertion failed"));
    error_fn!(Error; fn user_error_with_msg("{}", msg: &str));
    error_fn!(Error; fn assertion_failed_with_msg("{}", msg: &str));
}
