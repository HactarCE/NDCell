//! High-level representations of expressions in the AST.

use super::{ArgTypes, ArgValues, Args, UserFunction};
use crate::compiler::*;
use crate::errors::*;
use crate::functions;
use crate::parser;
use crate::{ConstValue, Span, Spanned, Type};
use LangErrorMsg::{CannotAssignToExpr, CannotEvalAsConst, InternalError, InvalidArguments};

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
        self.return_type.clone()
    }
    /// Returns the type that this expression evaluates to along with its span
    /// in the original source code.
    pub fn spanned_type(&self) -> Spanned<Type> {
        Spanned {
            span: self.span(),
            inner: self.return_type(),
        }
    }
    /// Constructs a new expression by applying the given Args to the given
    /// Function.
    pub fn try_new(
        userfunc: &mut UserFunction,
        span: Span,
        args: Args,
        func_constructor: functions::FuncConstructor,
    ) -> LangResult<Self> {
        let arg_types = args.types(userfunc);
        let func = func_constructor(userfunc, span, arg_types)?;
        let return_type = func.return_type(span)?;
        Ok(Self {
            span,
            func,
            args,
            return_type,
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

    /// Returns the type that can be assigned to this expression, or an error if
    /// this expression cannot be assigned to.
    pub fn assign_type(&self, userfunc: &UserFunction) -> LangResult<Type> {
        self.func
            .as_assignable(&self.args.values(userfunc))
            .ok_or_else(|| CannotAssignToExpr.with_span(self.span()))?
            .assign_type(self.span)
    }
    /// Returns a pointer value to the assignable value resulting from this
    /// expression.
    pub fn compile_assign(
        &self,
        compiler: &mut Compiler,
        userfunc: &UserFunction,
        value: Value,
    ) -> LangResult<()> {
        let arg_values: ArgValues = self.args.values(userfunc);
        self.func
            .as_assignable(&arg_values)
            .ok_or_else(|| CannotAssignToExpr.with_span(self.span()))?
            .compile_assign(compiler, arg_values, value)
    }
}

/// "Function" that takes zero or more arguments and returns a value that can be
/// compiled and optionally compile-time evaluated. The argument types that
/// function takes are baked into the function when it is constructed.
///
/// This language uses a very broad definition of "function" so that all kinds
/// of expressions can be represented using the same system. Properties/methods,
/// operators, variable access, etc. all have corresponding "Function"
/// implementors.
pub trait Function: std::fmt::Debug {
    /// Returns the end-user-friendly name for this function.
    ///
    /// For methods and properties, the name should be prefixed with the type
    /// followed by a period (e.g. "Pattern.outer").
    fn name(&self) -> String;

    /// Returns the kind of "function" this is. See FunctionKind for details.
    fn kind(&self) -> FunctionKind;

    /// Returns the types of arguments passed to this function.
    fn arg_types(&self) -> ArgTypes;
    /// Returns the return type of this function, or an error (e.g.
    /// InvalidArguments) if the function is passed invalid arguments.
    fn return_type(&self, span: Span) -> LangResult<Type>;

    /// Returns an InvalidArguments error if this set of arguments does not have
    /// the given length.
    fn check_args_len(&self, span: Span, len: usize) -> LangResult<()> {
        if self.arg_types().len() != len {
            Err(self.invalid_args_err(span))
        } else {
            Ok(())
        }
    }
    /// Returns an InvalidArguments error for this set of arguments.
    fn invalid_args_err(&self, span: Span) -> LangError {
        // TODO: when #[feature(never_type)] stabalizes, use that here and
        // return LangResult<!>.
        InvalidArguments {
            name: self.name(),
            is_method: matches!(self.kind(), FunctionKind::Method | FunctionKind::Property),
            arg_types: self.arg_types().into_iter().map(|sp| sp.inner).collect(),
        }
        .with_span(span)
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

    /// Returns this function's implementation for compile_get_ptr(), or None if
    /// it has none.
    fn as_assignable<'a>(&self, _args: &'a ArgValues) -> Option<&dyn AssignableFunction> {
        None
    }
}

/// Function that can be assigned to with a "set" statement.
///
/// This is used for variable assignment, and only makes sense for a handful of
/// functions (e.g. variable or vector access).
pub trait AssignableFunction: Function {
    /// Returns a pointer value to the assignable value resulting from this
    /// function.
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        _args: ArgValues,
        value: Value,
    ) -> LangResult<()>;
    /// Returns the type of value that should be assigned to this function,
    /// which by default is the same as its return type.
    fn assign_type(&self, span: Span) -> LangResult<Type> {
        self.return_type(span)
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
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FnSignature {
    pub args: Vec<Type>,
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
    pub fn new(args: Vec<Type>, ret: Type) -> Self {
        let args = args.into();
        Self { args, ret }
    }
    /// Constructs a function signature from a helper function parse tree node.
    pub fn from_helper_function_parse_tree(helper_func: &parser::HelperFunc, ndim: u8) -> Self {
        Self::new(
            helper_func
                .args
                .iter()
                .map(|arg| arg.inner.0.inner.resolve(ndim))
                .collect::<Vec<_>>(),
            helper_func.return_type.inner.resolve(ndim),
        )
    }
    /// Returns true if the given argument types match the arguments of this function signature.
    pub fn matches(&self, arg_types: &ArgTypes) -> bool {
        arg_types.iter().map(|s| &s.inner).eq(&self.args)
    }
    /// Returns the LLVM function type that is equivalent to this function signature.
    pub fn llvm_fn_type(
        &self,
        compiler: &Compiler,
    ) -> LangResult<inkwell::types::FunctionType<'static>> {
        use inkwell::types::BasicType;
        // TODO: validate user function args and return value at some point.
        let llvm_param_types = self
            .args
            .iter()
            .map(|t| compiler.get_llvm_type(t))
            .collect::<LangResult<Vec<_>>>()?;
        let llvm_ret_type = compiler.get_llvm_type(&self.ret)?;
        Ok(llvm_ret_type.fn_type(&llvm_param_types, false))
    }
}
