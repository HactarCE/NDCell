use codemap::{Span, Spanned};
use std::collections::HashMap;
use std::sync::Arc;

use crate::ast;
use crate::data::{LangInt, RtVal, SpannedRuntimeValueExt};
use crate::errors::{AlreadyReported, Error, Fallible, Result};

/// Global initialization and compile-time execution context.
///
/// "Global" here means that the information is the same at every scope. Missing
/// information may be filled in as initialization proceeds, and errors may be
/// added during compilation.
///
/// Includes information about the rule (such as number of dimensions) and any
/// errors during initialization and compilation.
#[derive(Debug, Clone)]
pub struct Ctx {
    /// List of all initialization and compiler errors. If this list is
    /// non-empty, the JIT function cannot be produced.
    pub errors: Vec<Error>,

    pub global_constants: HashMap<Arc<String>, RtVal>,

    /// `@compile` directive.
    ///
    /// This is `None` during initialization if no `@compile` directive has been
    /// reached yet; if there is no `@compile` directive at all, then that will
    /// cause an error.
    pub compile_directive: Option<ast::DirectiveId>,

    /// Number of dimensions.
    ///
    /// This is `None` during initialization if an `@ndim` directive exists but
    /// has not been reached yet; if there is no `@ndim` directive at all, this
    /// will be filled with a default value before initialization.
    pub ndim: Option<usize>,

    /// Number of states.
    ///
    /// This is `None` during initialization if an `@states` directive exists
    /// but has not been reached yet; if there is no `@states` directive at all,
    /// this will be filled with a default value before initialization.
    pub states: Option<usize>,
}
impl Ctx {
    /// Constructs a new directive, with values missing where they will be
    /// initialized in the future.
    pub fn new(directives: &[ast::Directive<'_>]) -> Self {
        // Infer ndim if there is no `@ndim` directive.
        let explicit_ndim = directives
            .iter()
            .any(|d| matches!(d.data(), ast::DirectiveData::Ndim(_)));
        let ndim = if explicit_ndim {
            None
        } else {
            Some(crate::DEFAULT_NDIM)
        };

        // Infer states if there is no `@states` directive.
        let explicit_states = directives
            .iter()
            .any(|d| matches!(d.data(), ast::DirectiveData::States(_)));
        let states = if explicit_states {
            None
        } else {
            Some(crate::DEFAULT_STATE_COUNT)
        };

        Self {
            errors: vec![],
            global_constants: HashMap::new(),
            compile_directive: None,
            ndim,
            states,
        }
    }

    /// Returns an internal error if there are any missing values.
    pub fn error_if_missing_values(&self) -> Result<()> {
        // This `match` may look clunky, but it ensures that we aren't
        // forgetting any fields. If new fields are added to `Ctx` in the
        // future, this exhaustive pattern-matching expression will produce a
        // compile error until it is updated.
        match self {
            Self {
                errors: _,
                global_constants: _,
                compile_directive: _, // ok if no `@compile` directive
                ndim: Some(_),
                states: Some(_),
            } => Ok(()),
            _ => internal_error!("context contains missing values: {:#?}", self),
        }
    }

    /// Sets the number of dimensions based on the result of an `@ndim`
    /// expression.
    pub fn set_ndim(&mut self, directive_span: Span, ndim: Spanned<RtVal>) -> Result<()> {
        if self.ndim.is_some() {
            return Err(Error::duplicate_directive(directive_span, "@ndim"));
        }

        let span = ndim.span;
        let n = ndim.as_integer()?;
        if n < 1 && n > crate::MAX_NDIM as LangInt {
            return Err(Error::invalid_dimension_count(span));
        }

        self.ndim = Some(n as usize);

        Ok(())
    }
    /// Sets the number of states based on the result of an `@states`
    /// expression.
    pub fn set_states(&mut self, directive_span: Span, states: Spanned<RtVal>) -> Result<()> {
        if self.states.is_some() {
            return Err(Error::duplicate_directive(directive_span, "@states"));
        }

        let span = states.span;
        let n = states.as_integer()?;
        if n < 1 && n > crate::MAX_STATE_COUNT as LangInt {
            return Err(Error::invalid_state_count(span));
        }

        self.states = Some(n as usize);

        Ok(())
    }
}

/// Trait for `Ctx` or structures that contain a `Ctx` (paricularly `Runtime`
/// and `Compiler`).
pub trait CtxTrait {
    /// Returns the global initialization and compile-time execution context.
    fn ctx(&mut self) -> &mut Ctx;

    /// Records an initialization or compiler error.
    fn error(&mut self, e: Error) -> AlreadyReported {
        self.ctx().errors.push(e);
        AlreadyReported
    }
    /// Returns the list of errors that have been reported, or `Ok(())` if there
    /// are none.
    fn get_errors_list(&mut self) -> std::result::Result<(), Vec<Error>> {
        if self.ctx().errors.is_empty() {
            Ok(())
        } else {
            Err(self.ctx().errors.clone())
        }
    }

    /// Returns the number of dimensions, or an error at `error_span` if yet
    /// initialized.
    fn get_ndim(&mut self, error_span: Span) -> Fallible<usize> {
        self.ctx()
            .ndim
            .ok_or_else(|| self.error(Error::not_reached_directive(error_span, "@ndim")))
    }
    /// Returns the number of states, or an error at `error_span` if not yet
    /// initialized.
    fn get_states(&mut self, error_span: Span) -> Fallible<usize> {
        self.ctx()
            .states
            .ok_or_else(|| self.error(Error::not_reached_directive(error_span, "@states")))
    }
}

impl CtxTrait for Ctx {
    fn ctx(&mut self) -> &mut Ctx {
        self
    }
}

/// Extension trait for easy error reporting.
pub trait ErrorReportExt {
    type Ok;

    /// Records an initialization or compiler error if there is one.
    fn report_err(self, ctx: &mut impl CtxTrait) -> Fallible<Self::Ok>;
}
impl<T> ErrorReportExt for Result<T> {
    type Ok = T;

    fn report_err(self, ctx: &mut impl CtxTrait) -> Fallible<T> {
        self.map_err(|e| ctx.error(e))
    }
}
