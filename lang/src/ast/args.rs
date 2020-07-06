//! Structs for passing arguments to functions.

use std::ops::Index;

use super::{Expr, ExprRef, UserFunction};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::{ConstValue, Spanned, Type};

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
    /// Returns the types of this Args, along with associated spans.
    pub fn types(&self, userfunc: &UserFunction) -> Vec<Spanned<Type>> {
        self.iter(userfunc).map(Expr::spanned_ret_type).collect()
    }
}

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
    /// Returns the expression of the argument at the given index.
    pub fn get_expr(&self, idx: usize) -> LangResult<&Expr> {
        match self.arg_asts.0.get(idx) {
            Some(arg) => Ok(&self.userfunc[*arg]),
            None => arg_out_of_range!(),
        }
    }

    /// Compiles the argument at the given index and returns the resulting
    /// Value.
    pub fn compile(&self, compiler: &mut Compiler, idx: usize) -> LangResult<Value> {
        self.get_expr(idx)?.compile(compiler, self.userfunc)
    }
    /// Compiles all arguments and return the resulting Values.
    pub fn compile_all(&self, compiler: &mut Compiler) -> LangResult<Vec<Value>> {
        (0..self.len()).map(|i| self.compile(compiler, i)).collect()
    }

    /// Evaluates the argument at the given index as a constant and returns the
    /// resulting ConstValue.
    ///
    /// Returns Err(CannotEvalAsConst) if the expression cannot be evaluated at
    /// compile time.
    pub fn const_eval(&self, idx: usize) -> LangResult<ConstValue> {
        self.get_expr(idx)?.const_eval(self.userfunc)
    }
    /// Evaluates all arguments and returns the resulting ConstValues.
    pub fn const_eval_all(&self) -> LangResult<Vec<ConstValue>> {
        (0..self.len()).map(|i| self.const_eval(i)).collect()
    }

    /// Returns whether the given argument can be assigned to.
    pub fn can_assign(&self, idx: usize) -> bool {
        self.get_expr(idx)
            .unwrap()
            .assign_type(self.userfunc)
            .is_ok()
    }
    /// Compiles an assignment to the assignable value resulting from the
    /// argument at the given index.
    pub fn compile_assign(
        &self,
        compiler: &mut Compiler,
        idx: usize,
        value: Value,
    ) -> LangResult<()> {
        match self.arg_asts.0.get(idx) {
            Some(arg) => self.userfunc[*arg].compile_assign(compiler, value, self.userfunc),
            None => arg_out_of_range!(),
        }
    }
}
