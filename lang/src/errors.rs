//! Error reporting functionality for compilation and runtime.

use std::borrow::Cow;
use std::error::Error;
use std::fmt;

use crate::lexer::ComparisonToken;
use crate::types::MAX_VECTOR_LEN;
use crate::{Span, Type, MAX_NDIM, MAX_STATES};

/// InternalError returned when an argument index is out of range (which should
/// never happen).
pub const ARG_OUT_OF_RANGE: LangError =
    LangErrorMsg::InternalError(Cow::Borrowed("Argument index out of range")).without_span();

/// InternalError returned when a variable is used improperly and it was not
/// caught by the type checker.
const INVALID_VARIABLE_USE: LangError = LangErrorMsg::InternalError(std::borrow::Cow::Borrowed(
    "Invalid variable use not caught by type checker",
))
.without_span();

/// InternalError returned when a TypeError occurs in a place where it should
/// have already been caught.
pub const UNCAUGHT_TYPE_ERROR: LangError =
    LangErrorMsg::InternalError(Cow::Borrowed("Uncaught type error")).without_span();

/// Result of a LangError with an accompanying line of source code.
pub type CompleteLangResult<T> = Result<T, LangErrorWithSource>;
/// Result of a LangError.
pub type LangResult<T> = Result<T, LangError>;

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
    Unmatched(char, char),
    Expected(&'static str),
    ExpectedGot {
        expected: &'static str,
        got: &'static str,
    },
    ReservedWord(Cow<'static, str>),
    ElseWithoutIf,
    MissingSetKeyword,
    TopLevelNonDirective,
    InvalidDirectiveName,
    RepeatDirective(&'static str),
    FunctionNameConflict,
    InvalidDimensionCount,
    InvalidStateCount,
    TypeError {
        expected: Type,
        got: Type,
    },
    CustomTypeError {
        expected: &'static str,
        got: Type,
    },
    CmpError {
        lhs: Type,
        cmp: ComparisonToken,
        rhs: Type,
    },
    InvalidArguments {
        name: String,
        is_method: bool,
        arg_types: Vec<Type>,
    },
    CannotIndexType(Type),
    CannotAssignTypeToVariable(Type),
    UseOfUninitializedVariable,
    BecomeInHelperFunction,
    ReturnInTransitionFunction,
    CannotEvalAsConst,
    VectorTooBig,
    FunctionLookupError,

    // Runtime errors
    IntegerOverflow,
    DivideByZero,
    NegativeExponent,
    CellStateOutOfRange,
    IndexOutOfBounds,
    AssertionFailed(Option<String>),
    UserError(Option<String>),
}
impl<T: 'static + std::error::Error> From<T> for LangErrorMsg {
    fn from(error: T) -> Self {
        Self::InternalError(error.to_string().into())
    }
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
                write!(f, "Internal error: {}\nThis is a bug in NDCell, not your code. Please report this to the developer!", s)?;
            }

            Self::UnknownSymbol => {
                write!(f, "Unknown symbol")?;
            }
            Self::Unterminated(s) => {
                write!(f, "This {} never ends", s)?;
            }
            Self::Unmatched(char1, char2) => {
                write!(f, "This '{}' has no matching '{}'", char1, char2)?;
            }
            Self::Expected(s) => {
                write!(f, "Expected {}", s)?;
            }
            Self::ExpectedGot { expected, got } => {
                write!(f, "Expected {}; got {}", expected, got)?;
            }
            Self::ReservedWord(s) => {
                write!(f, "'{}' is a reserved word", s)?;
            }
            Self::ElseWithoutIf => {
                write!(f, "This 'else' has no matching 'if'")?;
            }
            Self::MissingSetKeyword => {
                write!(f, "Variable assignment requires the 'set' keyword")?;
            }
            Self::TopLevelNonDirective => {
                write!(f, "Only directives may appear at the top level of a file")?;
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
                write!(f, "Number of states must range from 1 to {}", MAX_STATES)?;
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
                    lhs, rhs, cmp
                )?;
                if *lhs == Type::CellState && *rhs == Type::CellState {
                    write!(f, "; convert them to integers first using the '#id' tag")?;
                }
            }
            Self::InvalidArguments {
                name,
                is_method,
                arg_types,
            } => {
                // Omit first argument if used as a method.
                let arg_slice = if *is_method && !arg_types.is_empty() {
                    &arg_types[1..]
                } else {
                    &arg_types
                };
                write!(f, "Invalid arguments {:?} for {}", arg_slice, name,)?;
            }
            Self::CannotIndexType(ty) => {
                write!(f, "Cannot index {}", ty)?;
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
                    "Use 'return' instead of 'become' outside of transition functions"
                )?;
            }
            Self::ReturnInTransitionFunction => {
                write!(
                    f,
                    "Use 'become' instead of 'return' in transition functions"
                )?;
            }
            Self::CannotEvalAsConst => {
                write!(f, "Cannot evaluate this expression as a constant")?;
            }
            Self::VectorTooBig => {
                write!(
                    f,
                    "Too many elements in vector; maximum is {}",
                    MAX_VECTOR_LEN
                )?;
            }
            Self::FunctionLookupError => {
                write!(f, "There is no function with this name")?;
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
