//! High-level representations of expressions in the AST.

use super::super::compiler::*;
use super::super::errors::*;
use super::super::{ConstValue, Span, Type};
use super::{ArgTypes, ArgValues, Args, UserFunction};
use LangErrorMsg::{CannotEvalAsConst, InternalError, InvalidArguments};

/// Expression node in the AST.
#[derive(Debug)]
pub struct Expr {
    /// Span of this expression in the original source code.
    span: Span,
    /// Function used to compile or evaluate this expression.
    func: Box<dyn Function>,
    /// Arguments (other expressions) passed to the function.
    args: Args,
    /// Type that this expression evaluates to.
    return_type: Type,
}
impl Expr {
    /// Returns the span of this expression in the original source code.
    pub fn span(&self) -> Span {
        self.span
    }
    /// Returns the type that this expression evaluates to.
    pub fn return_type(&self) -> Type {
        self.return_type
    }
    /// Constructs a new expression by applying the given Args to the given
    /// Function.
    pub fn try_new(
        span: Span,
        userfunc: &mut UserFunction,
        func: Box<dyn Function>,
        args: Args,
    ) -> LangResult<Self> {
        let arg_types = args.types(userfunc);
        // Figure out the function signature, which will tells us the return
        // type. If there is no matching function signature, then return an
        // Err(InvalidArguments).
        let signature = func
            .get_signature(&arg_types)
            .ok_or_else(|| InvalidArguments {
                name: func.name(),
                omit_first: matches!(func.kind(), FunctionKind::Method | FunctionKind::Property),
                expected: func.signatures().iter().map(|s| s.args.clone()).collect(),
                got: arg_types,
            })?;
        Ok(Self {
            span,
            func,
            args,
            return_type: signature.ret,
        })
    }
    /// Compiles this expression and returns the resulting Value.
    pub fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<Value> {
        let ret_val = self.func.compile(compiler, self.args.values(userfunc))?;
        // Check return type.
        if ret_val.ty() == self.return_type() {
            Ok(ret_val)
        } else {
            Err(InternalError("Expression returned wrong type".into()).with_span(self.span()))
        }
    }
    /// Evaluates this expression as a constant and returns the resulting
    /// ConstValue.
    ///
    /// Returns Err(CannotEvalAsConst) if this expression cannot be evaluated at
    /// compile time.
    pub fn const_eval(&self, userfunc: &UserFunction) -> LangResult<ConstValue> {
        let ret_val = self
            .func
            .const_eval(self.args.values(userfunc))
            .transpose() // Convert Result<Option<_>, _> to Option<Result<_, _>>
            .unwrap_or_else(|| Err(CannotEvalAsConst.with_span(self.span())))?;
        // Check return type.
        if ret_val.ty() == self.return_type() {
            Ok(ret_val)
        } else {
            Err(InternalError("Expression returned wrong type".into()).with_span(self.span()))
        }
    }
}

/// A "function" that takes zero or more arguments and returns a value that can
/// be compiled and optionally compile-time evaluated.
///
/// This language uses a VERY broad definition of "function" so that all kinds
/// of expressions can be represented using the same system. Properties/methods,
/// operators, variable access, etc. all have corresponding "Function"
/// implementors.
pub trait Function: std::fmt::Debug {
    /// Returns the end-user-friendly name for this function.
    ///
    /// For non-functions like operators and properties, the name should be
    /// surrounded with angle brackets. For methods and properties, the name
    /// should be prefixed with the type followed by a period (e.g.
    /// "Pattern.outer").
    fn name(&self) -> String;

    /// Returns the kind of "function" this is. See FunctionKind for details.
    fn kind(&self) -> FunctionKind;

    /// Returns a list of valid signatures for this function.
    ///
    /// If a custom implementation of get_signature() is provided, then this
    /// function may return an empty vec![].
    fn signatures(&self) -> Vec<FnSignature>;

    /// Returns the signature this function uses if passed the given arguments.
    ///
    /// By default, this method returns the first matching FnSignature from
    /// signatures().
    fn get_signature(&self, args: &ArgTypes) -> Option<FnSignature> {
        self.signatures().into_iter().find(|s| s.matches(args))
    }

    /// Compiles this function using the given arguments and returns the Value
    /// returned from it.
    ///
    /// This function may panic or return an Err(InternalError) if ArgValues has
    /// invalid types.
    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value>;

    /// Evaluates this function using the given ArgValues and returns the Value
    /// returned from it, if the expression can be evaluated at compile time.
    ///
    /// Returns Ok(Some(_)) if a compile-time constant was successfully
    /// produced, Err(_) if a runtime error occured, or Ok(None) if this
    /// function cannot be evaluated at compile time. The default implementation
    /// returns Ok(None), indicating that the function cannot be evaluated as a
    /// constant.
    ///
    /// Semantically, Option<Result<_, _>> would make more sense here than
    /// Result<Option<_>, _>, but the `?`-operator makes Result<_, _> as the
    /// outermost type much more ergonomic.
    ///
    /// This function may panic or return an Err(InternalError) if ArgValues has
    /// invalid types.
    fn const_eval(&self, _args: ArgValues) -> LangResult<Option<ConstValue>> {
        Ok(None)
    }
}

/// An enumeration of the kinds of "functions," most of which would not
/// conventionally be called functions.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FunctionKind {
    /// Traditional function, such as `function(arg1, arg2)`.
    Function,
    /// Method, such as `value.method(arg1, arg2)`.
    Method,
    /// Operator, such as `arg1 + arg2`.
    Operator,
    /// Property, such as `value.property`.
    Property,
    /// Single value that takes no parameters, such as a literal or variable.
    Atom,
}

/// Function signature, consisting of types for the arguments and a return
/// type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnSignature {
    pub args: ArgTypes,
    pub ret: Type,
}
impl FnSignature {
    /// Constructs the signature for a function that takes no arguments.
    pub fn atom(ret: Type) -> Self {
        Self::new(vec![], ret)
    }
    /// Constructs the signature for a function that takes one argument and
    /// returns a property of that argument.
    pub fn property(self_type: Type, ret: Type) -> Self {
        Self::new(vec![self_type], ret)
    }
    /// Constructs any arbitrary function signature.
    pub fn new(args: impl Into<ArgTypes>, ret: Type) -> Self {
        let args = args.into();
        Self { args, ret }
    }
    /// Returns true if the given argument types match the arguments of this function signature.
    pub fn matches(&self, args: &ArgTypes) -> bool {
        &self.args == args
    }
}
