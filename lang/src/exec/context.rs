use std::collections::HashMap;
use std::sync::Arc;

use crate::ast;
use crate::data::RtVal;
use crate::errors::{AlreadyReported, Error};

/// Global initialization and compile-time execution context.
///
/// "Global" here means that the information is the same at every scope. Missing
/// information may be filled in as initialization proceeds, and errors may be
/// added during compilation.
///
/// Includes information about the rule (such as number of dimensions) and any
/// errors during initialization and compilation.
#[derive(Debug, Default, Clone)]
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

    /// Number of states.
    ///
    /// This is `None` during initialization if no `@ndim` directive has been
    /// reached yet; if there is no `@ndim` directive at all, this will be
    /// filled with a default value before compilation.
    pub ndim: Option<usize>,
}
impl Ctx {
    /// Fills in default values for anything missing.
    pub fn infer_missing_values(&mut self) {
        self.ndim.get_or_insert(crate::DEFAULT_NDIM);
    }
}

/// Trait for `Ctx` or structures that contain a `Ctx` (paricularly `Runtime`
/// and `Compiler`).
pub trait CtxTrait {
    /// Returns the global initialization and compile-time execution context.
    fn ctx(&mut self) -> &mut Ctx;

    /// Reports an initialization or compiler error.
    fn error(&mut self, e: Error) -> AlreadyReported {
        self.ctx().errors.push(e);
        AlreadyReported
    }
}

impl CtxTrait for Ctx {
    fn ctx(&mut self) -> &mut Ctx {
        self
    }
}
