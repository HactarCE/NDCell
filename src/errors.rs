use std::borrow::Cow;

use super::span::Span;

pub type LangErrorMsg = Cow<'static, str>;
pub type LangError = (Span, LangErrorMsg);
pub type LangResult<T> = Result<T, LangError>;

pub fn lang_error<S: Into<Span>, M: Into<LangErrorMsg>>(span: S, msg: M) -> LangError {
    (span.into(), msg.into())
}

pub fn lang_err<T, S: Into<Span>, M: Into<LangErrorMsg>>(span: S, msg: M) -> LangResult<T> {
    Err((span.into(), msg.into()))
}
