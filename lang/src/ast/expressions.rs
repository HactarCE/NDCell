//! High-level representations of expressions in the AST.

use super::{ArgTypes, ArgValues, Args, UserFunction};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::functions;
use crate::parser;
use crate::{ConstValue, Span, Spanned, Type};
use LangErrorMsg::{
    CannotAssignToExpr, CannotEvalAsConst, CannotIndexType, Expected, FunctionLookupError,
    InternalError, InvalidArguments, ReservedWord, Unimplemented,
};

pub fn from_parse_tree(
    userfunc: &mut UserFunction,
    parse_tree: &Spanned<parser::Expr>,
) -> LangResult<Expr> {
    let span = parse_tree.span;
    let args: Args;
    let func: functions::FuncConstructor;

    match &parse_tree.inner {
        // Integer literal
        parser::Expr::Int(i) => {
            args = Args::none();
            func = functions::literals::Int::with_value(*i);
        }
        // Type name
        parser::Expr::TypeName(_) => {
            args = Args::none();
            func = Err(ReservedWord.with_span(span))?;
        }
        // Identifier (variable)
        parser::Expr::Ident(s) => {
            args = Args::none();
            func = Box::new(functions::misc::GetVar::with_name(s.to_owned()));
        }
        // String literal
        parser::Expr::String(_) => {
            args = Args::none();
            func = Err(Unimplemented)?;
        }
        // Vector literal
        parser::Expr::Vector(exprs) => {
            args = Args::from(
                exprs
                    .iter()
                    .map(|expr| userfunc.build_expression_ast(expr))
                    .collect::<LangResult<Vec<_>>>()?,
            );
            func = Box::new(functions::literals::Vector::construct);
        }
        // Parenthetical group
        parser::Expr::ParenExpr(inner) => return from_parse_tree(userfunc, inner),
        // Unary operator
        parser::Expr::UnaryOp { op, operand } => {
            let arg = userfunc.build_expression_ast(operand)?;
            args = Args::from(vec![arg]);
            func = functions::lookup_unary_operator(*op, userfunc[arg].return_type(), span)?;
        }
        // Binary mathematical/bitwise operator
        parser::Expr::BinaryOp { lhs, op, rhs } => {
            let lhs = userfunc.build_expression_ast(lhs)?;
            let rhs = userfunc.build_expression_ast(rhs)?;
            args = Args::from(vec![lhs, rhs]);
            func = functions::lookup_binary_operator(
                &userfunc[lhs].return_type(),
                *op,
                &userfunc[rhs].return_type(),
                span,
            )?;
        }
        // Logical NOT
        parser::Expr::LogicalNot(expr) => {
            args = Args::from(vec![userfunc.build_expression_ast(expr)?]);
            func = Box::new(functions::logic::LogicalNot::construct);
        }
        // Binary logical operator
        parser::Expr::LogicalOr { lhs, rhs }
        | parser::Expr::LogicalXor { lhs, rhs }
        | parser::Expr::LogicalAnd { lhs, rhs } => {
            let lhs = userfunc.build_expression_ast(lhs)?;
            let rhs = userfunc.build_expression_ast(rhs)?;
            args = Args::from(vec![lhs, rhs]);
            func = functions::logic::LogicalBinaryOp::with_op(match &parse_tree.inner {
                parser::Expr::LogicalOr { .. } => functions::logic::LogicalBinOpType::Or,
                parser::Expr::LogicalXor { .. } => functions::logic::LogicalBinOpType::Xor,
                parser::Expr::LogicalAnd { .. } => functions::logic::LogicalBinOpType::And,
                _ => unreachable!(),
            })
        }
        // Comparison
        parser::Expr::Cmp { exprs, cmps } => {
            args = Args::from(userfunc.build_expression_list_ast(exprs)?);
            func = functions::cmp::Cmp::with_comparisons(cmps.clone());
        }
        // Attribute access
        parser::Expr::GetAttr { object, attribute } => {
            let ty: Type;
            if let parser::Expr::TypeName(type_token) = object.inner {
                ty = type_token.resolve(userfunc.rule_meta().ndim);
                args = Args::none();
            } else {
                let receiver = userfunc.build_expression_ast(object)?;
                ty = userfunc[receiver].return_type();
                args = Args::from(vec![receiver]);
            }
            func = functions::lookup_method(ty, &attribute.inner)
                .ok_or_else(|| FunctionLookupError.with_span(attribute.span))?;
        }
        // Function call
        parser::Expr::FnCall {
            func: func_expr,
            args: arg_exprs,
        } => {
            let mut args_list = userfunc.build_expression_list_ast(arg_exprs)?;
            func = match &func_expr.inner {
                parser::Expr::TypeName(type_token) => {
                    // Built-in function
                    functions::lookup_type_function(*type_token)
                        .ok_or_else(|| FunctionLookupError.with_span(func_expr.span))?
                }
                parser::Expr::Ident(func_name) => {
                    if userfunc
                        .rule_meta()
                        .helper_function_signatures
                        .contains_key(func_name)
                    {
                        // User-defined function
                        functions::misc::CallUserFn::with_name(func_name.to_owned())
                    } else {
                        // Built-in function
                        functions::lookup_function(func_name)
                            .ok_or_else(|| FunctionLookupError.with_span(func_expr.span))?
                    }
                }
                parser::Expr::GetAttr {
                    object,
                    attribute: method_name,
                } => {
                    // Built-in method
                    let ty: Type;
                    if let parser::Expr::TypeName(type_token) = object.inner {
                        ty = type_token.resolve(userfunc.rule_meta().ndim);
                    } else {
                        let receiver = userfunc.build_expression_ast(object)?;
                        ty = userfunc[receiver].return_type();
                        args_list.insert(0, receiver);
                    }
                    functions::lookup_method(ty, &method_name.inner)
                        .ok_or_else(|| FunctionLookupError.with_span(method_name.span))?
                }
                _ => Err(Expected("function name").with_span(func_expr.span))?,
            };
            args = Args::from(args_list);
        }
        parser::Expr::Index {
            object,
            args: index_args,
        } => {
            let mut args_list = vec![userfunc.build_expression_ast(object)?];
            args_list.extend(userfunc.build_expression_list_ast(index_args)?);
            args = Args::from(args_list);
            func = match userfunc[args[0]].return_type() {
                Type::Vector(_) => functions::vectors::Access::with_component_idx(None),
                ty => Err(CannotIndexType(ty))?,
            }
        }
    };

    Expr::try_new(userfunc, span, args, func)
}

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
    /// Returns the return type of this function, panicking or returning garbage
    /// if the function is passed invalid arguments. The default implementation
    /// of this method calls return_type().
    fn unwrap_return_type(&self) -> Type {
        self.return_type(Span::empty(0))
            .expect("Err given by return_type()")
    }

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
