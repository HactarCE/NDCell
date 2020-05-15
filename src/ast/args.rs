use std::borrow::Cow;
use std::ops::Index;

use super::super::compiler::*;
use super::super::errors::*;
use super::super::{ConstValue, Type};
use super::{Expr, ExprRef, UserFunction};
use LangErrorMsg::InternalError;

const ARG_OUT_OF_RANGE: LangError =
    InternalError(Cow::Borrowed("Argument index out of range")).without_span();

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
    pub fn none() -> Self {
        Self(vec![])
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn values<'a>(&'a self, userfunc: &'a UserFunction) -> ArgValues<'a> {
        ArgValues::new(userfunc, self)
    }
    pub fn iter_refs(&self) -> impl Iterator<Item = &ExprRef> {
        self.0.iter()
    }
    pub fn iter<'a>(&'a self, userfunc: &'a UserFunction) -> impl Iterator<Item = &'a Expr> {
        self.iter_refs().map(move |&e| &userfunc[e])
    }
    pub fn types(&self, userfunc: &UserFunction) -> ArgTypes {
        ArgTypes(self.iter(userfunc).map(Expr::return_type).collect())
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ArgTypes(Vec<Type>);
impl From<Vec<Type>> for ArgTypes {
    fn from(types: Vec<Type>) -> Self {
        Self(types)
    }
}
impl ArgTypes {
    pub fn to_string(&self, omit_first: bool) -> String {
        if omit_first && !self.0.is_empty() {
            format!("{:?}", &self.0[1..])
        } else {
            format!("{:?}", &self.0)
        }
    }
}

pub struct ArgValues<'a> {
    userfunc: &'a UserFunction,
    arg_asts: &'a Args,
    compiled_values: Vec<Option<Value>>,
    const_values: Vec<Option<ConstValue>>,
}
impl<'a> ArgValues<'a> {
    pub fn new(userfunc: &'a UserFunction, args: &'a Args) -> Self {
        Self {
            userfunc,
            arg_asts: args,
            compiled_values: vec![None; args.len()],
            const_values: vec![None; args.len()],
        }
    }
    pub fn len(&self) -> usize {
        self.arg_asts.len()
    }
    pub fn compile<'b>(&'b mut self, compiler: &mut Compiler, idx: usize) -> LangResult<&'b Value> {
        match self.compiled_values.get_mut(idx) {
            // Return the already-compiled argument.
            Some(Some(value)) => Ok(value),
            // Compile the argument and return it.
            Some(memoized) => {
                let compiled_arg = self.userfunc.compile_expr(compiler, self.arg_asts[idx])?;
                *memoized = Some(compiled_arg);
                return Ok(memoized.as_ref().unwrap());
            }
            None => Err(ARG_OUT_OF_RANGE),
        }
    }
    pub fn const_eval(&mut self, idx: usize) -> LangResult<&ConstValue> {
        match self.const_values.get_mut(idx) {
            // Return the already-evaluated argument.
            Some(Some(value)) => Ok(value),
            // Evaluate the argument and return it.
            Some(memoized) => {
                let evaluated_arg = self.userfunc.const_eval_expr(self.arg_asts[idx])?;
                *memoized = Some(evaluated_arg);
                return Ok(memoized.as_ref().unwrap());
            }
            None => Err(ARG_OUT_OF_RANGE),
        }
    }
    pub fn compile_all<'b>(mut self, compiler: &mut Compiler) -> LangResult<Vec<Value>> {
        for idx in 0..self.len() {
            self.compile(compiler, idx)?;
        }
        Ok(self
            .compiled_values
            .into_iter()
            .map(Option::unwrap)
            .collect())
    }
    pub fn const_eval_all(mut self) -> LangResult<Vec<ConstValue>> {
        for idx in 0..self.len() {
            self.const_eval(idx)?;
        }
        Ok(self.const_values.into_iter().map(Option::unwrap).collect())
    }
    pub fn get_compiled(&self, idx: usize) -> Option<&Value> {
        self.compiled_values.get(idx).map(Option::as_ref).flatten()
    }
    pub fn take_compiled(&mut self, idx: usize) -> Option<Value> {
        self.compiled_values
            .get_mut(idx)
            .map(Option::take)
            .flatten()
    }
    pub fn get_const(&self, idx: usize) -> Option<&ConstValue> {
        self.const_values.get(idx).map(Option::as_ref).flatten()
    }
    pub fn take_const(&mut self, idx: usize) -> Option<ConstValue> {
        self.const_values.get_mut(idx).map(Option::take).flatten()
    }
}
