pub mod atoms;
pub mod combinators;
pub mod directives;
pub mod expressions;
pub mod statements;
pub mod stencils;

pub use atoms::*;
pub use combinators::*;
pub use directives::*;
pub use expressions::*;
pub use statements::*;
pub use stencils::*;

use super::TokenFeeder;
use crate::errors::*;

/// A syntax structure that eats tokens and produces part of a parse tree.
pub trait TokenEater: std::fmt::Display {
    /// Parse type that this eater outputs.
    type Output;
    /// Returns whether if it appears that the user is trying to form this
    /// construct (generally returns true if the first token matches). If
    /// eat_tokens() returns Ok(_), this function MUST return true.
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool;
    /// Eats the tokens that are part of this syntax structure, returning the
    /// part of the parse tree produced. Does NOT restore the TokenFeeder if
    /// eating fails.
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output>;
    /// Wraps this token eater in another one that consumes the same tokens but
    /// attaches span information to the result.
    fn spanned(self) -> TokenSpanner<Self>
    where
        Self: Sized,
    {
        TokenSpanner(self)
    }
    /// Wraps this token eater in another one that consumes the same tokens but
    /// applies some function to the result.
    fn map<B, F: Fn(Self::Output) -> B>(self, f: F) -> TokenMapper<Self, F>
    where
        Self: Sized,
    {
        TokenMapper { eater: self, f }
    }
}

impl<O> std::fmt::Debug for Box<dyn TokenEater<Output = O>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", *self)
    }
}
impl<O, T: TokenEater<Output = O> + ?Sized> TokenEater for Box<T> {
    type Output = O;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        self.as_ref().might_match(tf)
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        self.as_ref().eat(tf)
    }
}

// Any TokenClass can be used as a TokenEater that consumes only a token exactly
// like it.
impl TokenEater for crate::lexer::TokenClass<'_> {
    type Output = ();
    fn might_match(&self, mut tf: TokenFeeder<'_>) -> bool {
        self.eat(&mut tf).is_ok()
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        if tf.next_class() == Some(*self) {
            Ok(())
        } else {
            tf.expected(self)
        }
    }
}
impl<T: TokenEater> TokenEater for &T {
    type Output = T::Output;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        (*self).might_match(tf)
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        (*self).eat(tf)
    }
}
