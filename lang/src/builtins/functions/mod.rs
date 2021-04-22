use codemap::{Span, Spanned};
use std::fmt;

pub mod math;

use crate::ast;
use crate::compiler::Compiler;
use crate::data::{FallibleTypeOf, RtVal, Type, Val};
use crate::errors::{AlreadyReported, Error, Fallible, ReportError};
use crate::runtime::Runtime;

/// Function that can be evaluated and/or compiled, taking zero or more
/// arguments and returning a value.
///
/// Most of the interpreter and compiler code is in implementations of this
/// trait.
pub trait Function: fmt::Debug + fmt::Display {
    /// Executes the expression and returns the resulting value.
    fn eval(&self, runtime: &mut Runtime, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal>;
    /// Compiles code to evaluate the expression and returns the resulting
    /// value.
    ///
    /// The default implementation unconditionally returns an error stating that
    /// this expression cannot be compiled.
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        Err(compiler.error(Error::cannot_compile(call.span)))
    }
}

impl Function for Box<dyn Function> {
    fn eval(&self, runtime: &mut Runtime, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        (**self).eval(runtime, call)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        (**self).compile(compiler, call)
    }
}

/// Data associated with a function call.
#[derive(Debug, Clone)]
pub struct CallInfo<A> {
    /// Span of the function name in the original source code.
    pub span: Span,
    /// Arguments passed to the function.
    pub args: Vec<A>,
}
impl CallInfo<ast::Expr<'_>> {
    pub fn eval_args(&self, runtime: &mut Runtime) -> Fallible<Vec<Spanned<RtVal>>> {
        self.args
            .iter()
            .map(|&arg_expr| runtime.eval_expr(arg_expr))
            .collect()
    }
    pub fn compile_args(&self, compiler: &mut Compiler) -> Fallible<Vec<Spanned<Val>>> {
        self.args
            .iter()
            .map(|&arg_expr| compiler.build_expr(arg_expr))
            .collect()
    }
}
impl<V: FallibleTypeOf> CallInfo<Spanned<V>> {
    fn arg_types(&self, reporter: &mut impl ReportError) -> Fallible<Vec<Type>> {
        self.args
            .iter()
            .map(|arg| {
                arg.fallible_ty()
                    .and_then(|ty| ty.map_err(|e| reporter.error(e)))
            })
            .collect()
    }

    fn invalid_args_error(
        &self,
        reporter: &mut impl ReportError,
        func_name: impl fmt::Display,
    ) -> AlreadyReported {
        match self.arg_types(reporter) {
            Err(AlreadyReported) => AlreadyReported,
            Ok(arg_types) => {
                reporter.error(Error::invalid_arguments(self.span, func_name, &arg_types))
            }
        }
    }

    pub fn check_args_len(
        &self,
        n: usize,
        reporter: &mut impl ReportError,
        func_name: impl fmt::Display,
    ) -> Fallible<()> {
        if self.args.len() == n {
            Ok(())
        } else {
            Err(self.invalid_args_error(reporter, func_name))
        }
    }
}
