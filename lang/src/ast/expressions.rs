//! High-level representations of expressions in the AST.

use super::{ArgValues, Args, ErrorPointRef, UserFunction};
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
    let name: String;
    let span = parse_tree.span;
    let args: Args;
    let func: functions::FuncConstructor;

    match &parse_tree.inner {
        // Integer literal
        parser::Expr::Int(i) => {
            name = "integer literal".to_string();
            args = Args::none();
            func = functions::literals::Int::with_value(*i);
        }
        // Type name
        parser::Expr::TypeName(type_token) => {
            name = type_token.to_string();
            args = Args::none();
            func = Err(ReservedWord.with_span(span))?;
        }
        // Identifier (variable)
        parser::Expr::Ident(s) => {
            name = s.clone();
            args = Args::none();
            func = Box::new(functions::misc::GetVar::with_name(s.to_owned()));
        }
        // String literal
        parser::Expr::String(_) => {
            name = "string literal".to_owned();
            args = Args::none();
            func = Err(Unimplemented)?;
        }
        // Vector literal
        parser::Expr::Vector(exprs) => {
            name = "vector literal".to_owned();
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
            name = format!("unary '{}' operator", op);
            let arg = userfunc.build_expression_ast(operand)?;
            args = Args::from(vec![arg]);
            func = functions::lookup_unary_operator(*op, userfunc[arg].ret_type(), span)?;
        }
        // Binary mathematical/bitwise operator
        parser::Expr::BinaryOp { lhs, op, rhs } => {
            name = format!("binary '{}' operator", op);
            let lhs = userfunc.build_expression_ast(lhs)?;
            let rhs = userfunc.build_expression_ast(rhs)?;
            args = Args::from(vec![lhs, rhs]);
            func = functions::lookup_binary_operator(
                &userfunc[lhs].ret_type(),
                *op,
                &userfunc[rhs].ret_type(),
                span,
            )?;
        }
        // Logical NOT
        parser::Expr::LogicalNot(expr) => {
            name = "unary 'not' operator".to_owned();
            args = Args::from(vec![userfunc.build_expression_ast(expr)?]);
            func = Box::new(functions::logic::LogicalNot::construct);
        }
        // Binary logical operator
        parser::Expr::LogicalOr(lhs, rhs)
        | parser::Expr::LogicalXor(lhs, rhs)
        | parser::Expr::LogicalAnd(lhs, rhs) => {
            let op = match &parse_tree.inner {
                parser::Expr::LogicalOr { .. } => functions::logic::LogicalBinOpType::Or,
                parser::Expr::LogicalXor { .. } => functions::logic::LogicalBinOpType::Xor,
                parser::Expr::LogicalAnd { .. } => functions::logic::LogicalBinOpType::And,
                _ => unreachable!(),
            };
            name = format!("binary '{}' operator", op);
            let lhs = userfunc.build_expression_ast(lhs)?;
            let rhs = userfunc.build_expression_ast(rhs)?;
            args = Args::from(vec![lhs, rhs]);
            func = functions::logic::LogicalBinaryOp::with_op(op)
        }
        // Comparison
        parser::Expr::Cmp { exprs, cmps } => {
            name = "comparison operator".to_owned();
            args = Args::from(userfunc.build_expression_list_ast(exprs)?);
            func = functions::cmp::Cmp::with_comparisons(cmps.clone());
        }
        // Membership/matching
        parser::Expr::Is(lhs, rhs) => {
            name = format!("binary 'is' operator");
            let lhs = userfunc.build_expression_ast(lhs)?;
            let rhs = userfunc.build_expression_ast(rhs)?;
            args = Args::from(vec![lhs, rhs]);
            func = Box::new(functions::cmp::Is::construct);
        }
        // Attribute access
        parser::Expr::GetAttr { object, attribute } => {
            let ty: Type;
            if let parser::Expr::TypeName(type_token) = object.inner {
                ty = type_token.resolve(userfunc.rule_meta().ndim);
                args = Args::none();
            } else {
                let receiver = userfunc.build_expression_ast(object)?;
                ty = userfunc[receiver].ret_type();
                args = Args::from(vec![receiver]);
            }
            name = format!("method {}.{}", ty, attribute.inner);
            func = functions::lookup_method(ty, &attribute.inner)
                .ok_or_else(|| FunctionLookupError.with_span(attribute.span))?;
        }
        // Function call
        parser::Expr::FnCall {
            func: func_expr,
            args: arg_exprs,
        } => {
            let mut args_list = userfunc.build_expression_list_ast(arg_exprs)?;
            match &func_expr.inner {
                parser::Expr::TypeName(type_token) => {
                    // Type constructor
                    name = format!("constructor {}", type_token);
                    func = functions::lookup_type_function(*type_token)
                        .ok_or_else(|| FunctionLookupError.with_span(func_expr.span))?;
                }
                parser::Expr::Ident(func_name) => {
                    if userfunc
                        .rule_meta()
                        .helper_function_signatures
                        .contains_key(func_name)
                    {
                        // User-defined function
                        name = format!("user-defined function {}", func_name);
                        func = functions::misc::CallUserFn::with_name(func_name.to_owned());
                    } else {
                        // Built-in function
                        name = format!("function {}", func_name);
                        func = functions::lookup_function(func_name)
                            .ok_or_else(|| FunctionLookupError.with_span(func_expr.span))?;
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
                        ty = userfunc[receiver].ret_type();
                        args_list.insert(0, receiver);
                    }
                    name = format!("method {}.{}", ty, method_name.inner);
                    func = functions::lookup_method(ty, &method_name.inner)
                        .ok_or_else(|| FunctionLookupError.with_span(method_name.span))?;
                }
                _ => return Err(Expected("function name".to_owned()).with_span(func_expr.span))?,
            }
            args = Args::from(args_list);
        }
        parser::Expr::Index {
            object,
            args: index_args,
        } => {
            let object_expr = userfunc.build_expression_ast(object)?;
            let object_type = userfunc[object_expr].ret_type();
            name = format!("{} indexing", object_type);
            let mut args_list = vec![object_expr];
            args_list.extend(userfunc.build_expression_list_ast(index_args)?);
            args = Args::from(args_list);
            func = match object_type {
                Type::Vector(_) => functions::vectors::Access::with_component_idx(None),
                other_type => Err(CannotIndexType(other_type))?,
            }
        }
    };

    let func_constructor = func;
    Expr::try_new(name, span, args, userfunc, func_constructor)
}

/// Expression node in the AST.
#[derive(Debug)]
pub struct Expr {
    /// Function used to compile or evaluate this expression.
    func: Box<dyn Function>,
    /// End-user-friendly name for the function being called.
    ///
    /// For methods and properties, the name should be prefixed with the type
    /// followed by a period (e.g. "Vec3.sum").
    name: String,
    /// Span of this expression in the original source code.
    span: Span,
    /// Arguments (other expressions) passed to the function.
    args: Args,
    /// Expected return type (None if not yet determined).
    ret_type: Type,
}
impl Expr {
    /// Constructs a new expression by applying the given Args to the given
    /// Function.
    pub fn try_new(
        mut name: String,
        span: Span,
        mut args: Args,
        userfunc: &mut UserFunction,
        func_constructor: functions::FuncConstructor,
    ) -> LangResult<Self> {
        let mut call_info = FuncCallInfoMut {
            name: &mut name,
            span,
            args: &mut args,
            userfunc,
        };
        let func = func_constructor(&mut call_info)?;
        let ret_type = func.return_type(&mut call_info)?;
        Ok(Self {
            func,
            name,
            span,
            args,
            ret_type,
        })
    }

    /// Returns mutable references to information about the function call in
    /// this expression.
    fn call_info_mut<'a>(&'a mut self, userfunc: &'a mut UserFunction) -> FuncCallInfoMut<'a> {
        FuncCallInfoMut {
            name: &mut self.name,
            span: self.span,
            args: &mut self.args,
            userfunc,
        }
    }
    /// Returns immutable information about the function call in this
    /// expression.
    fn call_info<'a>(&'a self, userfunc: &'a UserFunction) -> FuncCallInfo<'a> {
        FuncCallInfo {
            name: &self.name,
            span: self.span,
            args: &self.args,
            ret_type: Some(&self.ret_type),
            userfunc,
        }
    }
    /// Returns the arguments passed to the function in this expression.
    fn args(&self) -> &Args {
        &self.args
    }
    /// Returns the span of this expression in the original source code.
    pub fn span(&self) -> Span {
        self.span
    }
    /// Returns the type that this expression evaluates to.
    pub fn ret_type(&self) -> Type {
        self.ret_type.clone()
    }
    /// Returns the type that this expression evaluates to along with its span
    /// in the original source code.
    pub fn spanned_ret_type(&self) -> Spanned<Type> {
        Spanned {
            span: self.span(),
            inner: self.ret_type.clone(),
        }
    }

    /// Compiles this expression and returns the resulting Value.
    pub fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<Value> {
        let ret_val = self.func.compile(compiler, self.call_info(userfunc))?;
        // Check return type.
        if ret_val.ty() == self.ret_type {
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
        let ret_val = self.func.const_eval(self.call_info(userfunc))?;
        // Check return type.
        if ret_val.ty() == self.ret_type {
            Ok(ret_val)
        } else {
            Err(InternalError("Expression returned wrong type".into()).with_span(self.span()))
        }
    }

    /// Returns the type that can be assigned to this expression, or an error if
    /// this expression cannot be assigned to.
    pub fn assign_type(&self, userfunc: &UserFunction) -> LangResult<Type> {
        let info = self.call_info(userfunc);
        self.func
            .as_assignable(info)
            .ok_or(CannotEvalAsConst.with_span(self.span()))?
            .assign_type(info)
    }
    /// Returns a pointer value to the assignable value resulting from this
    /// expression.
    pub fn compile_assign(
        &self,
        compiler: &mut Compiler,
        value: Value,
        userfunc: &UserFunction,
    ) -> LangResult<()> {
        self.func
            .as_assignable(self.call_info(userfunc))
            .ok_or_else(|| CannotAssignToExpr.with_span(self.span()))?
            .compile_assign(compiler, value, self.call_info(userfunc))
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
    /// Returns the return type of this function, or an error (e.g.
    /// InvalidArguments) if the function is passed invalid arguments.
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type>;

    /// Compiles this function using the given arguments and returns the Value
    /// returned from it (which should have the type of ret_ty).
    ///
    /// This function may panic or return an Err(InternalError) if ArgValues has
    /// invalid types.
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value>;

    /// Evaluates this function using the given ArgValues and returns the Value
    /// returned from it (which should have the type of ret_ty) if the
    /// expression can be evaluated at compile time, or Err(CannotEvalAsConst)
    /// if this expression cannot be evaluated at compile time.
    ///
    /// This function may panic or return an Err(InternalError) if ArgValues has
    /// invalid types.
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        Err(CannotAssignToExpr.with_span(info.span))
    }

    /// Returns this function as an AssignableFunction, or
    /// None if it is not assignable.
    fn as_assignable(&self, _info: FuncCallInfo) -> Option<&dyn AssignableFunction> {
        None
    }
}

/// Function that can be assigned to with a "set" statement.
///
/// This is used for variable assignment, and only makes sense for a handful of
/// functions (e.g. variable or vector access).
pub trait AssignableFunction: Function {
    /// Compiles an assignment of the given value into the result of this
    /// function.
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        value: Value,
        info: FuncCallInfo,
    ) -> LangResult<()>;
    /// Returns the type of value that should be assigned to this function,
    /// which by default is the same as its return type.
    fn assign_type(&self, info: FuncCallInfo) -> LangResult<Type> {
        Ok(info.ret_type().clone())
    }
}

/// Various immutable information pertaining to function calls, not including
/// the actual function being called.
#[derive(Debug, Copy, Clone)]
pub struct FuncCallInfo<'a> {
    /// End-user-friendly name for the function being called.
    pub name: &'a str,
    /// Span of this expression in the original source code.
    pub span: Span,
    /// Arguments (other expressions) passed to the function.
    pub args: &'a Args,
    /// Expected return type (None if not yet determined).
    pub ret_type: Option<&'a Type>,
    /// User funcion that this call is within.
    pub userfunc: &'a UserFunction,
}
impl FuncCallInfo<'_> {
    /// Returns the types of the arguments passed to the function, along with
    /// associated spans.
    pub fn arg_types(&self) -> Vec<Spanned<Type>> {
        self.args.types(self.userfunc)
    }
    /// Returns ArgValues for this function call.
    pub fn arg_values(&self) -> ArgValues {
        self.args.values(self.userfunc)
    }

    /// Returns the expected return type of this function call, or panics if it
    /// has not yet been determined.
    pub fn ret_type(&self) -> &Type {
        self.ret_type
            .expect("Called FuncCallInfo::unwrap_ret_type() before return type has been determined")
    }
}

/// Various mutable information pertaining to function calls, not including the
/// actual function being called. This is a mutable version of FuncCallInfo,
/// minus expected return type.
#[derive(Debug)]
pub struct FuncCallInfoMut<'a> {
    /// End-user-friendly name for the function being called.
    pub name: &'a mut String,
    /// Span of this expression in the original source code.
    pub span: Span,
    /// Arguments (other expressions) passed to the function.
    pub args: &'a mut Args,
    /// User funcion that this call is within.
    pub userfunc: &'a mut UserFunction,
}
impl<'a, 'b: 'a> From<&'a FuncCallInfoMut<'b>> for FuncCallInfo<'a> {
    fn from(info_mut: &'a FuncCallInfoMut<'b>) -> Self {
        FuncCallInfo {
            name: info_mut.name,
            span: info_mut.span,
            args: info_mut.args,
            ret_type: None,
            userfunc: info_mut.userfunc,
        }
    }
}
impl FuncCallInfoMut<'_> {
    /// Returns the types of the arguments passed to the function, along with
    /// associated spans.
    pub fn arg_types(&self) -> Vec<Spanned<Type>> {
        self.args.types(self.userfunc)
    }

    /// Returns an InvalidArguments error if this set of arguments does not have
    /// the given length.
    pub fn check_args_len(&self, len: usize) -> LangResult<()> {
        if self.args.len() != len {
            Err(self.invalid_args_err())
        } else {
            Ok(())
        }
    }
    /// Returns an InvalidArguments error for this set of arguments.
    pub fn invalid_args_err(&self) -> LangError {
        // TODO: when #[feature(never_type)] stabalizes, use that here and
        // return LangResult<!>.
        InvalidArguments {
            name: self.name.to_owned(),
            arg_types: self.arg_types().into_iter().map(|sp| sp.inner).collect(),
        }
        .with_span(self.span)
    }

    /// Adds a new error point spanning this function call.
    pub fn add_error_point(&mut self, error: LangErrorMsg) -> ErrorPointRef {
        self.userfunc.add_error_point(error.with_span(self.span))
    }
}
