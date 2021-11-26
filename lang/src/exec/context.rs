use codemap::{Span, Spanned};
use codemap_diagnostic::Level;
use std::collections::HashMap;
use std::sync::Arc;

use crate::data::{LangInt, RtVal, SpannedRuntimeValueExt};
use crate::errors::{Error, Result};
use crate::{ast, LangMode};

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
    /// List of all initialization and compiler errors and warnings. If this
    /// list contains an error, the JIT function cannot be produced.
    pub errors: Vec<Error>,

    pub global_constants: HashMap<Arc<String>, RtVal>,

    /// `@compile` directive.
    ///
    /// This is `None` during initialization if no `@compile` directive has been
    /// reached yet; if there is no `@compile` directive at all, then that will
    /// cause an error.
    pub compile_directive: Option<ast::DirectiveId>,
    /// `@transition` directive.
    pub transition_directive: Option<ast::DirectiveId>,
    /// `@function` directives.
    pub helper_functions: HashMap<Arc<String>, ast::DirectiveId>,

    /// Number of dimensions.
    ///
    /// This is `None` during initialization if an `@ndim` directive exists but
    /// has not been reached yet; if there is no `@ndim` directive at all, this
    /// will be filled with a default value before initialization.
    pub ndim: Option<usize>,
    /// Neighborhood radius. (TODO: shape instead)
    pub radius: Option<usize>,
    /// Number of states.
    ///
    /// This is `None` during initialization if an `@states` directive exists
    /// but has not been reached yet; if there is no `@states` directive at all,
    /// this will be filled with a default value before initialization.
    pub states: Option<usize>,

    /// "Language mode."
    pub mode: LangMode,
}
impl Ctx {
    /// Constructs a new directive, with values missing where they will be
    /// initialized in the future.
    pub fn new(ast: &ast::Program) -> Self {
        let mut errors = vec![];

        // Infer ndim if there is no `@ndim` directive.
        let ndim = if ast.has_directive(|d| matches!(d, ast::DirectiveData::Ndim(_))) {
            None
        } else {
            Some(crate::DEFAULT_NDIM)
        };

        // Infer radius if there is no `@radius` directive.
        let radius = if ast.has_directive(|d| matches!(d, ast::DirectiveData::Radius(_))) {
            None
        } else {
            Some(crate::DEFAULT_NDIM)
        };

        // Infer states if there is no `@states` directive.
        let states = if ast.has_directive(|d| matches!(d, ast::DirectiveData::States(_))) {
            None
        } else {
            Some(crate::DEFAULT_STATE_COUNT)
        };

        // Find `@compile` directive.
        let compile_directive = ast
            .find_single_directive("@compile", |d| {
                matches!(d, ast::DirectiveData::Compile { .. })
            })
            .unwrap_or_else(|e| {
                errors.push(e);
                None
            });

        // Find `@transition` directive.
        let transition_directive = ast
            .find_single_directive("@transition", |d| {
                matches!(d, ast::DirectiveData::Transition { .. })
            })
            .unwrap_or_else(|e| {
                errors.push(e);
                None
            });

        // Find `@function` directives.
        let helper_functions = ast
            .directives()
            .filter_map(|d| match d.data() {
                ast::DirectiveData::Function { name, .. } => Some((Arc::clone(name), d.id)),
                _ => None,
            })
            .collect();

        Self {
            errors,

            global_constants: HashMap::new(),

            compile_directive,
            transition_directive,
            helper_functions,

            ndim,
            radius,
            states,

            mode: LangMode::Internal,
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

                compile_directive: _,    // ok if no `@compile` directive
                transition_directive: _, // ok if no `@transition` directive
                helper_functions: _,

                ndim: Some(_),
                radius: Some(_),
                states: Some(_),

                mode: _,
            } => Ok(()),
            _ => internal_error!("context contains missing values: {:#?}", self),
        }
    }

    /// Sets the number of dimensions based on the result of an `@ndim`
    /// expression.
    pub fn set_ndim(&mut self, directive_span: Span, ndim: &Spanned<RtVal>) -> Result<()> {
        if self.ndim.is_some() {
            return Err(Error::duplicate_directive(directive_span, "@ndim"));
        }

        let n = ndim.as_integer()?;
        if !(1 <= n && n <= crate::MAX_NDIM as LangInt) {
            return Err(Error::invalid_dimension_count(ndim.span));
        }

        self.ndim = Some(n as usize);

        Ok(())
    }
    /// Sets the radius based on the result of an `@radius` expression.
    pub fn set_radius(&mut self, directive_span: Span, radius: &Spanned<RtVal>) -> Result<()> {
        if self.radius.is_some() {
            return Err(Error::duplicate_directive(directive_span, "@radius"));
        }

        let r = radius.as_integer()?;
        if !(0 <= r && r <= crate::MAX_RADIUS as LangInt) {
            return Err(Error::invalid_radius(radius.span));
        }

        self.radius = Some(r as usize);

        Ok(())
    }
    /// Sets the number of states based on the result of an `@states`
    /// expression.
    pub fn set_states(&mut self, directive_span: Span, states: &Spanned<RtVal>) -> Result<()> {
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
    fn report_error(&mut self, e: Error) {
        match &e {
            Error::New(_) => self.ctx().errors.push(e),
            Error::AlreadyReported => (),
        }
    }
    /// Returns whether there are any errors that would prevent compilation.
    fn has_errors(&mut self) -> bool {
        self.ctx()
            .errors
            .iter()
            .any(|e| e.as_diagnostic().unwrap().level == Level::Error)
    }
    /// Returns `Ok(())` if there are no errors or warnings, or `Err()` if there
    /// are some.
    #[cfg(test)]
    fn get_errors_and_warnings_result(&mut self) -> std::result::Result<(), Vec<Error>> {
        if self.ctx().errors.is_empty() {
            Ok(())
        } else {
            Err(self.ctx().errors.clone())
        }
    }

    /// Returns the number of dimensions, or an error at `error_span` if yet
    /// initialized.
    fn get_ndim(&mut self, error_span: Span) -> Result<usize> {
        self.ctx()
            .ndim
            .ok_or_else(|| Error::not_reached_directive(error_span, "'@ndim'"))
    }
    /// Returns the radius, or an error at `error_span` if yet initialized.
    fn get_radius(&mut self, error_span: Span) -> Result<usize> {
        self.ctx()
            .radius
            .ok_or_else(|| Error::not_reached_directive(error_span, "'@radius' or '@nbhd'"))
    }
    /// Returns the number of states, or an error at `error_span` if not yet
    /// initialized.
    fn get_states(&mut self, error_span: Span) -> Result<usize> {
        self.ctx()
            .states
            .ok_or_else(|| Error::not_reached_directive(error_span, "'@states'"))
    }

    /// Returns the current "language mode."
    fn mode(&self) -> LangMode;
}

impl CtxTrait for Ctx {
    fn ctx(&mut self) -> &mut Ctx {
        self
    }
    fn mode(&self) -> LangMode {
        self.mode
    }
}
