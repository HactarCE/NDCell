//! Structs for passing arguments to functions.

use std::borrow::Cow;
use std::ops::Index;

use super::{Expr, ExprRef, UserFunction};
use crate::compiler::*;
use crate::errors::*;
use crate::{ConstValue, Spanned, Type};
use LangErrorMsg::InternalError;

/// Error returned when an argument index is out of range (which should never happen).
const ARG_OUT_OF_RANGE: LangError =
    InternalError(Cow::Borrowed("Argument index out of range")).without_span();

/// Collection of expressions passed as arguments to a function.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Args(Vec<ExprRef>);
impl From<Vec<ExprRef>> for Args {
    fn from(expressions: Vec<ExprRef>) -> Self {
        Self(expressions)
    }
}
impl Index<usize> for Args {
    type Output = ExprRef;
    fn index(&self, idx: usize) -> &ExprRef {
        &self.0[idx]
    }
}
impl Args {
    /// Constructs an Args containing no arguments.
    pub fn none() -> Self {
        Self(vec![])
    }
    /// Returns the number of arguments.
    pub fn len(&self) -> usize {
        self.0.len()
    }
    /// Returns an ArgValues for this Args.
    pub fn values<'a>(&'a self, userfunc: &'a UserFunction) -> ArgValues<'a> {
        ArgValues::new(userfunc, self)
    }
    /// Returns an iterator over the ExprRefs of this Args.
    pub fn iter_refs(&self) -> impl Iterator<Item = &ExprRef> {
        self.0.iter()
    }
    /// Returns an iterator over the Exprs of this Args.
    pub fn iter<'a>(&'a self, userfunc: &'a UserFunction) -> impl Iterator<Item = &'a Expr> {
        self.iter_refs().map(move |&e| &userfunc[e])
    }
    /// Returns the types of this Args.
    pub fn types(&self, userfunc: &UserFunction) -> ArgTypes {
        self.iter(userfunc).map(Expr::spanned_type).collect()
    }
}

/// Collection of argument types with associated spans.
pub type ArgTypes = Vec<Spanned<Type>>;

/// Wrapper over Args that allows the arguments to be compiled or evaluated.
#[derive(Debug, Copy, Clone)]
pub struct ArgValues<'a> {
    /// Reference to the user function that this is a part of.
    userfunc: &'a UserFunction,
    /// Reference to the AST nodes of these arguments.
    arg_asts: &'a Args,
}
impl<'a> ArgValues<'a> {
    /// Constructs a new ArgValues from an existing Args.
    pub fn new(userfunc: &'a UserFunction, args: &'a Args) -> Self {
        Self {
            userfunc,
            arg_asts: args,
        }
    }
    /// Returns the number of arguments.
    pub fn len(&self) -> usize {
        self.arg_asts.len()
    }
    /// Compiles the argument at the given index and returns the resulting
    /// Value.
    pub fn compile(&self, compiler: &mut Compiler, idx: usize) -> LangResult<Value> {
        match self.arg_asts.0.get(idx) {
            Some(arg) => self.userfunc.compile_expr(compiler, *arg),
            None => Err(ARG_OUT_OF_RANGE),
        }
    }
    /// Compiles all arguments and return the resulting Values.
    pub fn compile_all(&self, compiler: &mut Compiler) -> LangResult<Vec<Value>> {
        self.arg_asts
            .iter_refs()
            .map(|&expr| self.userfunc.compile_expr(compiler, expr))
            .collect()
    }
    /// Evaluates the argument at the given index as a constant and returns the
    /// resulting ConstValue.
    ///
    /// Returns Err(CannotEvalAsConst) if the expression cannot be evaluated at
    /// compile time.
    pub fn const_eval(&self, idx: usize) -> LangResult<ConstValue> {
        match self.arg_asts.0.get(idx) {
            Some(arg) => self.userfunc.const_eval_expr(*arg),
            None => Err(ARG_OUT_OF_RANGE),
        }
    }
}
