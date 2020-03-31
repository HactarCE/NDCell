use std::borrow::Cow;
use std::error::Error;
use std::fmt;

use super::span::Span;
use super::Type;

pub type LangResult<T> = Result<T, LangError>;
pub type LangErrorMsg = Cow<'static, str>;

#[derive(Debug)]
pub struct LangError {
    pub span: Option<Span>,
    pub msg: LangErrorMsg,
}
impl LangError {
    pub fn with_source(&self, src: &str) -> LangErrorWithSource {
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
                msg: self.msg.clone(),
            }
        } else {
            LangErrorWithSource {
                source_line: None,
                span: None,
                msg: self.msg.clone(),
            }
        }
    }
}

#[derive(Debug)]
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

pub fn spanned_lang_error(span: impl Into<Span>, msg: impl Into<LangErrorMsg>) -> LangError {
    LangError {
        span: Some(span.into()),
        msg: msg.into(),
    }
}

pub fn spanned_lang_err<T>(span: impl Into<Span>, msg: impl Into<LangErrorMsg>) -> LangResult<T> {
    Err(spanned_lang_error(span, msg))
}

pub fn lang_error(msg: impl Into<LangErrorMsg>) -> LangError {
    LangError {
        span: None,
        msg: msg.into(),
    }
}

pub fn lang_err<T>(msg: impl Into<LangErrorMsg>) -> LangResult<T> {
    Err(lang_error(msg))
}

pub fn type_error<T>(
    spanned: impl Into<Span>,
    got_type: Type,
    expected_type: Type,
) -> LangResult<T> {
    spanned_lang_err(
        spanned,
        format!(
            "Type error: expected {} but got {}",
            expected_type, got_type
        ),
    )
}
