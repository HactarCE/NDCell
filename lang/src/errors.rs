//! Error reporting functionality for compilation and runtime.

use std::borrow::Cow;
use std::error::Error;
use std::fmt;

use crate::lexer::ComparisonToken;
use crate::types::{TypeDesc, MAX_VECTOR_LEN};
use crate::{Span, Type, MAX_NDIM, MAX_STATE_COUNT};

/// Result of a LangError with an accompanying line of source code.
pub type CompleteLangResult<T> = Result<T, LangErrorWithSource>;
/// Result of a LangError.
pub type LangResult<T> = Result<T, LangError>;

pub const NO_RUNTIME_REPRESENTATION: &str = "Type has no runtime representation!";

/// An error type and an accompanying line and span of source code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LangErrorWithSource {
    /// The string of source code of the error location (if any).
    pub source_line: Option<String>,
    /// The 1-indexed line number of the error location (if any).
    pub line_num: Option<usize>,
    /// The 1-indexed character span of the error location within the given line
    /// (if any).
    pub span: Option<(usize, usize)>,
    /// The type of error.
    pub msg: LangErrorMsg,
}
impl fmt::Display for LangErrorWithSource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let (Some(line), Some(line_num), Some((start, end))) =
            (&self.source_line, self.line_num, self.span)
        {
            // Write line and column numbers.
            writeln!(f, "Error at line {}; column {}", line_num, start)?;
            // Remove initial whitespace.
            let trimmed_len = line.len() - line.trim_start().len();
            let trimmed_start = start - trimmed_len;
            // Write line of source code.
            writeln!(f, "{}", line.trim())?;
            for _ in 0..(trimmed_start - 1) {
                write!(f, " ")?;
            }
            // Write arrows pointing to the part with the error.
            for _ in start..end {
                write!(f, "^")?;
            }
            write!(f, "   ")?;
        }
        // Write the error message.
        write!(f, "{}", self.msg)?;
        Ok(())
    }
}
impl Error for LangErrorWithSource {}
impl LangErrorWithSource {
    /// Returns a pair (source_code, error_msg) that succinctly gives the error
    /// location; especially useful in tests, since the exact formatting of
    /// errors may change in the future.
    pub fn pair(&self) -> (String, String) {
        let source: String;
        if let (Some(line), Some((start, end))) = (&self.source_line, self.span) {
            source = line[(start - 1)..(end - 1)].to_owned();
        } else {
            source = "".to_owned();
        }
        (source, self.msg.to_string())
    }
}

/// An error type and an accompanying span.
#[derive(Debug, Clone)]
pub struct LangError {
    /// The span of the error location (if any).
    pub span: Option<Span>,
    /// The type of error.
    pub msg: LangErrorMsg,
}
impl LangError {
    /// Attaches a span to this LangError, if it does not already have one.
    pub fn with_span(mut self, span: impl Into<Span>) -> Self {
        if self.span.is_none() {
            self.span = Some(span.into());
        }
        self
    }
    /// Provides a line of source code as context to this error, returning a
    /// LangErrorWithSource.
    pub fn with_source(self, src: &str) -> LangErrorWithSource {
        if let Some(span) = self.span {
            let (start_tp, end_tp) = span.textpoints(src);
            let start = start_tp.column();
            // If the error spans multiple lines, use a zero-length span on the
            // first line.
            let mut end = start;
            if start_tp.line() == end_tp.line() && end_tp.column() > start_tp.column() {
                end = end_tp.column();
            }
            LangErrorWithSource {
                source_line: src
                    .lines()
                    .skip(start_tp.line() - 1)
                    .next()
                    .map(str::to_owned),
                line_num: Some(start_tp.line()),
                span: Some((start, end)),
                msg: self.msg,
            }
        } else {
            LangErrorWithSource {
                source_line: None,
                line_num: None,
                span: None,
                msg: self.msg,
            }
        }
    }
}

/// Information about the type of error that occurred.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LangErrorMsg {
    // Miscellaneous errors
    Unimplemented,
    UnknownError,
    InternalError(Cow<'static, str>),

    // Compile errors
    UnknownSymbol,
    Unterminated(&'static str),
    Expected(String),
    ReservedWord,
    ElseWithoutIf,
    InvalidDirectiveName,
    RepeatDirective(&'static str),
    FunctionNameConflict,
    InvalidDimensionCount,
    InvalidStateCount,
    TypeError {
        expected: TypeDesc,
        got: Type,
    },
    CustomTypeError {
        expected: String,
        got: Type,
    },
    CmpError {
        lhs: Type,
        cmp: ComparisonToken,
        rhs: Type,
    },
    InvalidArguments {
        name: String,
        arg_types: Vec<Type>,
    },
    CannotIndexType(Type),
    CannotAssignToExpr,
    CannotAssignTypeToVariable(Type),
    UseOfUninitializedVariable,
    BecomeInHelperFunction,
    ReturnInTransitionFunction,
    CannotEvalAsConst,
    VectorTooBig,
    FunctionLookupError,
    NotInLoop,

    // Runtime errors
    IntegerOverflow,
    DivideByZero,
    NegativeExponent,
    CellStateOutOfRange,
    IndexOutOfBounds,
    AssertionFailed(Option<String>),
    UserError(Option<String>),
}
impl fmt::Display for LangErrorMsg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Unimplemented => {
                write!(f, "This feature is unimplemented")?;
            }
            Self::UnknownError => {
                write!(f, "(unknown error)")?;
            }
            Self::InternalError(s) => {
                write!(f, "Internal error: {}\nThis is a bug in NDCell, not your cellular automaton. Please report this to the developer!", s)?;
            }

            Self::UnknownSymbol => {
                write!(f, "Unknown symbol")?;
            }
            Self::Unterminated(s) => {
                write!(f, "This {} never ends", s)?;
            }
            Self::Expected(s) => {
                write!(f, "Expected {}", s)?;
            }
            Self::ReservedWord => {
                write!(f, "This is a reserved word")?;
            }
            Self::ElseWithoutIf => {
                write!(f, "This 'else' has no matching 'if'")?;
            }
            Self::InvalidDirectiveName => {
                write!(f, "Invalid directive name")?;
            }
            Self::RepeatDirective(name) => {
                write!(f, "Multiple {:?} directives; only one is allowed", name)?;
            }
            Self::FunctionNameConflict => {
                write!(f, "There is already a function with this name")?;
            }
            Self::InvalidDimensionCount => {
                write!(f, "Number of dimensions must range from 1 to {}", MAX_NDIM)?;
            }
            Self::InvalidStateCount => {
                write!(
                    f,
                    "Number of states must range from 1 to {}",
                    MAX_STATE_COUNT
                )?;
            }

            Self::TypeError { expected, got } => {
                write!(f, "Type error: expected {} but got {}", expected, got)?;
            }
            Self::CustomTypeError { expected, got } => {
                write!(f, "Type error: expected {} but got {}", expected, got)?;
            }
            Self::CmpError { lhs, cmp, rhs } => {
                write!(
                    f,
                    "Type error: cannot compare {} to {} using '{}'",
                    lhs, rhs, cmp,
                )?;
                if *lhs == Type::CellState && *rhs == Type::CellState {
                    write!(f, "; convert them to integers first using the '#id' tag")?;
                }
            }
            Self::InvalidArguments { name, arg_types } => {
                write!(f, "Invalid arguments {:?} for {}", arg_types, name,)?;
            }
            Self::CannotIndexType(ty) => {
                write!(f, "Cannot index {}", ty)?;
            }
            Self::CannotAssignToExpr => {
                write!(f, "Cannot assign to this expression")?;
            }
            Self::CannotAssignTypeToVariable(ty) => {
                write!(f, "Cannot assign {} to variable", ty)?;
            }
            Self::UseOfUninitializedVariable => {
                write!(f, "This variable must be initialized before it is used")?;
            }
            Self::BecomeInHelperFunction => {
                write!(
                    f,
                    "Use 'return' instead of 'become' outside of transition functions",
                )?;
            }
            Self::ReturnInTransitionFunction => {
                write!(
                    f,
                    "Use 'become' instead of 'return' in transition functions",
                )?;
            }
            Self::CannotEvalAsConst => {
                write!(f, "Cannot evaluate this expression as a constant")?;
            }
            Self::VectorTooBig => {
                write!(
                    f,
                    "Too many elements in {}; maximum is {}",
                    TypeDesc::Vector,
                    MAX_VECTOR_LEN,
                )?;
            }
            Self::FunctionLookupError => {
                write!(f, "There is no function with this name")?;
            }
            Self::NotInLoop => {
                write!(f, "This is only allowed a loop")?;
            }

            Self::IntegerOverflow => {
                write!(f, "Integer overflow")?;
            }
            Self::DivideByZero => {
                write!(f, "Divide by zero")?;
            }
            Self::NegativeExponent => {
                write!(f, "Negative exponent")?;
            }
            Self::CellStateOutOfRange => {
                write!(f, "Cell state out of range")?;
            }
            Self::IndexOutOfBounds => {
                write!(f, "Index out of bounds")?;
            }
            Self::AssertionFailed(msg) => {
                write!(f, "Assertion failed")?;
                if let Some(msg) = msg {
                    write!(f, ": {:?}", msg)?;
                }
            }
            Self::UserError(msg) => {
                write!(f, "Error")?;
                if let Some(msg) = msg {
                    write!(f, ": {:?}", msg)?;
                }
            }
        }
        Ok(())
    }
}
impl LangErrorMsg {
    /// Attaches a span to this error message, returning a LangError.
    pub fn with_span(self, span: impl Into<Span>) -> LangError {
        LangError {
            span: Some(span.into()),
            msg: self,
        }
    }
    /// Returns a LangError from this error message, without a span.
    pub const fn without_span(self) -> LangError {
        LangError {
            span: None,
            msg: self,
        }
    }
}

impl<T: Into<LangErrorMsg>> From<T> for LangError {
    fn from(msg: T) -> Self {
        msg.into().without_span()
    }
}

/// Handles internal errors in the NDCA compiler. Panics in debug mode, but
/// returns a nice error message in release mode (so that the program doesn't
/// immediately crash on the user).
///
/// Prefer internal_error!(); be careful not to call this and then throw away
/// the error it returns, because in debug mode it will still panic.
macro_rules! internal_error_value {
    // Don't allocate a new String for &'static str literals.
    ( $msg:expr ) => {{
        // Panic in a debug build (for stack trace).
        #[cfg(debug_assertions)]
        #[allow(unused)]
        let ret: crate::errors::LangError = panic!("{}", $msg);
        // Give nice error message for user in release build.
        #[cfg(not(debug_assertions))]
        #[allow(unused)]
        let ret: crate::errors::LangError = crate::errors::LangErrorMsg::InternalError(
            std::borrow::Cow::Borrowed($msg),
        )
        .without_span();
        #[allow(unreachable_code)]
        ret
    }};
    // Automatically format!() arguments.
    ( $( $args:expr ),+ $(,)? ) => {{

        // Panic in a debug build (for stack trace).
        #[cfg(debug_assertions)]
        #[allow(unused)]
        let ret: crate::errors::LangError = panic!($( $args ),+);
        // Give nice error message for user in release build.
        #[cfg(not(debug_assertions))]
        #[allow(unused)]
        let ret: crate::errors::LangError =
            crate::errors::LangErrorMsg::InternalError(format!($( $args ),+).into()).without_span();
        #[allow(unreachable_code)]
        ret
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
}
