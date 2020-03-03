use std::borrow::Cow;

use super::Span;

pub type LangErrorMsg = Cow<'static, str>;
pub type LangError = (Span, LangErrorMsg);
pub type LangResult<T> = Result<T, LangError>;

pub fn lang_error<S: Into<Span>, M: Into<LangErrorMsg>>(span: S, msg: M) -> LangError {
    (span.into(), msg.into())
}

pub fn lang_err<T, S: Into<Span>, M: Into<LangErrorMsg>>(span: S, msg: M) -> LangResult<T> {
    Err(lang_error(span, msg))
}

// pub fn unimplemented<'a>(token: &Token<'a>) -> String {
//     format!(
//         "Functionality for '{}' is not yet implemented",
//         token.string
//     )
// }

// pub fn invalid_statement_token() -> String {
//     format!("There should be a statement here, but every statement must begin with 'become', 'break', 'case', 'continue', 'else', 'for', 'if', 'remain', 'return', 'set', 'unless', or 'while'")
// }

// pub fn internal_error(msg: &str) -> String {
//     format!("Internal parsing error: {}", msg)
// }

// pub fn expected<'a>(expected: &str) -> String {
//     format!("Expected {}", expected)
// }

// pub fn expected_got<'a>(expected: &str, got: &str) -> String {
//     format!("Expected {} but got {}", expected, got)
// }

// pub fn reserved_word_as_variable<'a>(got: &str) -> String {
//     format!("Expected a variable name here, but '{}' is a reserved word and cannot be used as a variable name", got)
// }

// pub fn unmatched_open_delimiter<'a>(open: &str, close: &str) -> String {
//     format!("This '{}' has no matching '{}'", open, close)
// }

// pub fn incorrect_close_delimiter<'a>(open: &str, expected: &str, got: &str) -> String {
//     format!(
//         "Expected '{}' to close '{}' but got {} instead",
//         expected, open, got
//     )
// }

// pub fn incomplete_group<'a>(open: &Token<'a>, close: Option<&Token<'a>>) -> String {
//     let expected_close = match open.string {
//         "(" => ")",
//         "[" => "]",
//         "{" => "}",
//         other => panic!("No closing token for {}", other),
//     };
//     match close {
//         None => format!("Expected {} to close {}", expected_close, open.string),
//         Some(close) => format!(
//             "Expected {} to close {}, but got {} instead",
//             expected_close, open.string, close.string
//         ),
//     }
// }
