use std::borrow::Cow;
use std::error::Error;
use std::fmt;

use super::ast::common::{Cmp, Op};
use super::span::Span;
use super::types::Type;

/// A Result of a LangError and an accompanying line of source code.
pub type CompleteLangResult<T> = Result<T, LangErrorWithSource>;
/// A Result of a LangError.
pub type LangResult<T> = Result<T, LangError>;

/// An error type and an accompanying line and span of source code.
#[derive(Debug, Clone)]
pub struct LangErrorWithSource {
    /// The string of source code of the error location (if any).
    pub source_line: Option<String>,
    /// The 1-indexed line number of the error location (if any).
    pub line_num: Option<usize>,
    /// The span of the error location (if any).
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
#[derive(Debug, Clone)]
pub enum LangErrorMsg {
    // Miscellaneous errors
    Unimplemented,
    InternalError(Cow<'static, str>),

    // Compile errors
    UnknownSymbol,
    Unterminated(&'static str),
    Unmatched(char, char),
    Expected(&'static str),
    ReservedWord(Cow<'static, str>),
    ElseWithoutIf,
    MissingSetKeyword,
    TopLevelNonDirective,
    InvalidDirectiveName,
    MissingTransitionFunction,
    MultipleTransitionFunctions,
    TypeError { expected: Type, got: Type },
    OpError { op: Op, lhs: Type, rhs: Type },
    CmpError { cmp: Cmp, lhs: Type, rhs: Type },
    CannotAssignTypeToVariable(Type),
    UseOfUninitializedVariable,
    BecomeInHelperFunction,
    ReturnInTransitionFunction,

    // Runtime errors
    IntegerOverflow,
    DivideByZero,
    CellStateOutOfRange,
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
            Self::MissingTransitionFunction => {
                write!(f, "Missing transition function")?;
            }
            Self::MultipleTransitionFunctions => {
                write!(f, "Multiple transition functions")?;
            }

            Self::TypeError { expected, got } => {
                write!(f, "Type error: expected {} but got {}", expected, got)?;
            }
            Self::OpError { op, lhs, rhs } => {
                write!(f, "Cannot apply operation '{}' to {} and {}", op, lhs, rhs)?;
            }
            Self::CmpError { cmp, lhs, rhs } => {
                write!(
                    f,
                    "Type error: cannot compare {} to {} using '{}'",
                    lhs, rhs, cmp
                )?;
                if *lhs == Type::CellState && *rhs == Type::CellState {
                    write!(f, "; convert them to integers first using the '#id' tag")?;
                }
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

            Self::IntegerOverflow => {
                write!(f, "Integer overflow")?;
            }
            Self::DivideByZero => {
                write!(f, "Divide by zero")?;
            }
            Self::CellStateOutOfRange => {
                write!(f, "Cell state out of range")?;
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
