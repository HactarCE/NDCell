use codemap::{Span, Spanned};
use std::fmt;
use std::sync::Arc;

pub(super) mod bools;
pub(super) mod cells;
pub(super) mod math;
pub(super) mod range;
pub(super) mod sets;
pub(super) mod types;
pub(super) mod vectors;

use crate::ast;
use crate::data::{FallibleTypeOf, RtVal, Type, Val};
use crate::errors::{AlreadyReported, Error, Fallible};
use crate::exec::{Compiler, Ctx, CtxTrait, ErrorReportExt};

/// Function that can be evaluated and/or compiled, taking zero or more
/// arguments and returning a value.
///
/// Most of the interpreter and compiler code is in implementations of this
/// trait.
pub trait Function: fmt::Debug + fmt::Display {
    /// Returns a list of allowed keys for keyword arguments.
    fn kwarg_keys(&self) -> &[&'static str] {
        &[]
    }

    /// Executes the expression and returns the resulting value.
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal>;
    /// Compiles code to evaluate the expression and returns the resulting
    /// value.
    ///
    /// The default implementation unconditionally returns an error stating that
    /// this expression cannot be compiled.
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        Err(compiler.error(Error::cannot_compile(call.span)))
    }

    /// Wrap the function in a `Box<dyn Function>`.
    fn boxed(self) -> Box<dyn Function>
    where
        Self: 'static + Sized,
    {
        Box::new(self)
    }
}

impl Function for Box<dyn Function> {
    fn kwarg_keys(&self) -> &[&'static str] {
        (**self).kwarg_keys()
    }

    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        (**self).eval(ctx, call)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        (**self).compile(compiler, call)
    }

    fn boxed(self) -> Box<dyn Function> {
        self
    }
}

/// Data associated with a function call.
#[derive(Debug, Clone)]
pub struct CallInfo<A> {
    /// Span of the function name in the original source code.
    pub span: Span,
    /// Arguments passed to the function.
    pub args: Vec<A>,
    /// Keyword arguments passed to the function.
    kwargs: Vec<(Arc<String>, A)>,
}
impl<A: Clone> CallInfo<A> {
    pub fn new(span: Span, args_and_kwargs: Vec<ast::FuncArg<A>>) -> Self {
        let args = args_and_kwargs
            .iter()
            .filter(|(k, _)| k.is_none())
            .map(|(_, v)| v)
            .cloned()
            .collect();
        let kwargs = args_and_kwargs
            .iter()
            .filter_map(|(k, v)| Some((k.clone()?.node, v.clone())))
            .collect();
        Self { span, args, kwargs }
    }
}
impl<V> CallInfo<Spanned<V>>
where
    Spanned<V>: FallibleTypeOf,
{
    fn arg_types(&self, ctx: &mut impl CtxTrait) -> Fallible<Vec<Type>> {
        self.args
            .iter()
            .map(|arg| arg.fallible_ty().and_then(|ty| ty.report_err(ctx)))
            .collect()
    }

    fn invalid_args_error(
        &self,
        ctx: &mut impl CtxTrait,
        func_name: impl fmt::Display,
    ) -> AlreadyReported {
        match self.arg_types(ctx) {
            Err(AlreadyReported) => AlreadyReported,
            Ok(arg_types) => ctx.error(Error::invalid_arguments(self.span, func_name, &arg_types)),
        }
    }

    pub fn check_args_len(
        &self,
        n: usize,
        ctx: &mut impl CtxTrait,
        func_name: impl fmt::Display,
    ) -> Fallible<()> {
        // TODO: replace calls to this function with more specific ones for
        // better error messages (e.g. "<function name> expects 2 arguments")
        if self.args.len() == n {
            Ok(())
        } else {
            Err(self.invalid_args_error(ctx, func_name))
        }
    }

    pub fn arg(&self, i: usize, ctx: &mut impl CtxTrait) -> Fallible<&Spanned<V>> {
        self.args
            .get(i)
            .ok_or_else(|| ctx.error(internal_error_value!("arg out of range")))
    }
    pub fn kwarg(&self, key: &str) -> Option<&Spanned<V>> {
        self.kwargs
            .iter()
            .find(|(k, _)| &**k == key)
            .map(|(_, v)| v)
    }
}
