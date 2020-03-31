use std::borrow::Cow;
use std::error::Error;
use std::fmt;

use super::span::Span;
use super::Type;

pub type LangResult<T> = Result<T, LangError>;
pub type LangErrorMsg = Cow<'static, str>;

#[derive(Debug)]
pub struct LangError {
    pub span: Span,
    pub msg: LangErrorMsg,
}
impl LangError {
    pub fn with_source(&self, src: &str) -> LangErrorWithSource {
        let (start_tp, end_tp) = self.span.textpoints(src);
        let start = start_tp.column();
        let mut end = start;
        if start_tp.line() == end_tp.line() && end_tp.column() > start_tp.column() {
            end = end_tp.column();
        }
        LangErrorWithSource {
            start,
            end,
            msg: self.msg.clone(),
            source_line: src
                .lines()
                .skip(start_tp.line() - 1)
                .next()
                .unwrap_or_default()
                .to_owned(),
        }
    }
}

#[derive(Debug)]
pub struct LangErrorWithSource {
    pub start: usize,
    pub end: usize,
    pub msg: LangErrorMsg,
    pub source_line: String,
}
impl fmt::Display for LangErrorWithSource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.start > 0 {
            // Write line of source code.
            writeln!(f, "{}", self.source_line)?;
            for _ in 0..(self.start - 1) {
                write!(f, " ")?;
            }
            // Write arrows pointing to the part with the error.
            for _ in self.start..self.end {
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

pub fn lang_error<S: Into<Span>, M: Into<LangErrorMsg>>(span: S, msg: M) -> LangError {
    LangError {
        span: span.into(),
        msg: msg.into(),
    }
}

pub fn lang_err<T, S: Into<Span>, M: Into<LangErrorMsg>>(span: S, msg: M) -> LangResult<T> {
    Err(lang_error(span, msg))
}

pub fn type_error<T>(
    spanned: impl Into<Span>,
    got_type: Type,
    expected_type: Type,
) -> LangResult<T> {
    lang_err(
        spanned,
        format!(
            "Type error: expected {} but got {}",
            expected_type, got_type
        ),
    )
}
