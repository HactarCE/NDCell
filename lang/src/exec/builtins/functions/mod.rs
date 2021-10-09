use codemap::{Span, Spanned};
use std::fmt;
use std::sync::Arc;

pub(super) mod arrays;
pub(super) mod bools;
pub(super) mod cells;
pub(super) mod cmp;
pub(super) mod math;
pub(super) mod range;
pub(super) mod sets;
pub(super) mod types;
pub(super) mod vectors;

use crate::ast;
use crate::data::{RtVal, TryGetType, Type, Val};
use crate::errors::{AlreadyReported, Error, Fallible};
use crate::exec::{Compiler, Ctx, CtxTrait, ErrorReportExt, Runtime};

/// Function that can be evaluated and/or compiled, taking zero or more
/// arguments and returning a value.
///
/// Most of the interpreter and compiler code is in implementations of this
/// trait.
pub trait Function: fmt::Debug {
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

    /// Assigns to the expression.
    ///
    /// The default implementation unconditionally returns an error stating that
    /// this expression cannot be assigned to.
    fn eval_assign(
        &self,
        runtime: &mut Runtime,
        call: CallInfo<Spanned<RtVal>>,
        first_arg: ast::Expr<'_>,
        new_value: Spanned<RtVal>,
    ) -> Fallible<()> {
        Err(runtime.error(Error::cannot_assign_to(call.span)))
    }
    /// Compiles code to assign to the expression.
    ///
    /// The default implementation unconditionally returns an error stating that
    /// this expression cannot be assigned to.
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        call: CallInfo<Spanned<Val>>,
        first_arg: ast::Expr<'_>,
        new_value: Spanned<Val>,
    ) -> Fallible<()> {
        Err(compiler.error(Error::cannot_compile_assign_to(call.span)))
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

    fn eval_assign(
        &self,
        runtime: &mut Runtime,
        call: CallInfo<Spanned<RtVal>>,
        first_arg: ast::Expr<'_>,
        new_value: Spanned<RtVal>,
    ) -> Fallible<()> {
        (**self).eval_assign(runtime, call, first_arg, new_value)
    }
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        call: CallInfo<Spanned<Val>>,
        first_arg: ast::Expr<'_>,
        new_value: Spanned<Val>,
    ) -> Fallible<()> {
        (**self).compile_assign(compiler, call, first_arg, new_value)
    }

    fn boxed(self) -> Box<dyn Function> {
        self
    }
}

// TODO: use these aliases everywhere

/// Data associated with a runtime function call.
pub type RuntimeCallInfo = CallInfo<Spanned<RtVal>>;
/// Data associated with a compile-time function call.
pub type CompiledCallInfo = CallInfo<Spanned<Val>>;

/// Data associated with a function call.
#[derive(Debug, Clone)]
pub struct CallInfo<A> {
    /// Function name.
    pub name: Arc<String>,
    /// Span of the function name or symbol in the original source code.
    pub span: Span,
    /// Arguments passed to the function.
    pub args: Vec<A>,
    /// Keyword arguments passed to the function.
    kwargs: Vec<(Arc<String>, A)>,
}
impl<A: Clone> CallInfo<A> {
    pub fn new(name: Spanned<Arc<String>>, args_and_kwargs: Vec<ast::FuncArg<A>>) -> Self {
        Self {
            name: name.node,
            span: name.span,
            args: args_and_kwargs
                .iter()
                .filter(|(k, _)| k.is_none())
                .map(|(_, v)| v)
                .cloned()
                .collect(),
            kwargs: args_and_kwargs
                .iter()
                .filter_map(|(k, v)| Some((k.clone()?.node, v.clone())))
                .collect(),
        }
    }
}
impl<V> CallInfo<Spanned<V>>
where
    Spanned<V>: TryGetType,
{
    fn arg_types(&self, ctx: &mut impl CtxTrait) -> Fallible<Vec<Type>> {
        self.args
            .iter()
            .map(|arg| arg.try_get_type().and_then(|ty| ty.report_err(ctx)))
            .collect()
    }

    fn invalid_args_error(&self, ctx: &mut impl CtxTrait) -> AlreadyReported {
        match self.arg_types(ctx) {
            Err(AlreadyReported) => AlreadyReported,
            Ok(arg_types) => ctx.error(Error::invalid_arguments(self.span, &self.name, &arg_types)),
        }
    }

    pub fn check_args_len(&self, n: usize, ctx: &mut impl CtxTrait) -> Fallible<()> {
        // TODO: replace calls to this function with more specific ones for
        // better error messages (e.g. "<function name> expects 2 arguments")
        if self.args.len() == n {
            Ok(())
        } else {
            Err(self.invalid_args_error(ctx))
        }
    }
    pub fn check_kwargs(&self, func: &impl Function, ctx: &mut impl CtxTrait) -> Fallible<()> {
        let allowed_kwarg_keys = func.kwarg_keys();
        let mut kwargs_present = vec![false; allowed_kwarg_keys.len()];
        for (key, expr) in &self.kwargs {
            if let Some(i) = allowed_kwarg_keys.iter().position(|&k| k == &***key) {
                if kwargs_present[i] {
                    return Err(ctx.error(Error::duplicate_keyword_argument(expr.span)));
                }
                kwargs_present[i] = true;
            } else {
                return Err(ctx.error(Error::invalid_keyword_argument(expr.span, &self.name)));
            }
        }
        Ok(())
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
