use std::borrow::Cow;
use std::error::Error;
use std::fmt;

use super::span::Span;
use super::types::Type;

pub type CompleteLangResult<T> = Result<T, LangErrorWithSource>;
pub type LangResult<T> = Result<T, LangError>;

#[derive(Debug, Clone)]
pub struct LangErrorWithSource {
    pub source_line: Option<String>,
    pub span: Option<(usize, usize)>,
    pub msg: LangErrorMsg,
}
impl fmt::Display for LangErrorWithSource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let (Some(line), Some((start, end))) = (&self.source_line, self.span) {
            // Write line of source code.
            writeln!(f, "{}", line)?;
            for _ in 0..(start - 1) {
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

#[derive(Debug, Clone)]
pub struct LangError {
    pub span: Option<Span>,
    pub msg: LangErrorMsg,
}
impl LangError {
    pub fn with_source(self, src: &str) -> LangErrorWithSource {
        if let Some(span) = self.span {
            let (start_tp, end_tp) = span.textpoints(src);
            let start = start_tp.column();
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
                span: Some((start, end)),
                msg: self.msg,
            }
        } else {
            LangErrorWithSource {
                source_line: None,
                span: None,
                msg: self.msg,
            }
        }
    }
}

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
    MissingSetKeyword,
    TopLevelNonDirective,
    InvalidDirectiveName,
    MissingTransitionFunction,
    MultipleTransitionFunctions,

    // Compile errors for JIT; runtime errors for interpreter
    TypeError {
        expected: Type,
        got: Type,
    },
    ComparisonError {
        cmp_sym: &'static str,
        lhs: Type,
        rhs: Type,
    },
    UseOfUninitializedVariable,
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
            Self::ComparisonError { cmp_sym, lhs, rhs } => {
                write!(
                    f,
                    "Type error: cannot compare {} to {} using {}",
                    lhs, rhs, cmp_sym
                )?;
                if *lhs == Type::CellState && *rhs == Type::CellState {
                    write!(f, "; convert them to integers first using the '#id' tag")?;
                }
            }
            Self::UseOfUninitializedVariable => {
                write!(f, "This variable might not be initialized before use")?;
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
    pub fn with_span(self, span: impl Into<Span>) -> LangError {
        LangError {
            span: Some(span.into()),
            msg: self,
        }
    }
    pub fn without_span(self) -> LangError {
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
