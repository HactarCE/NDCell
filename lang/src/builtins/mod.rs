//! Built-in values and functions; the "standard library" of NDCA.

use codemap::{Span, Spanned};
use std::fmt;

pub mod math;

use crate::ast;
use crate::compiler::Compiler;
use crate::data::{CpVal, RtVal, Type, Val};
use crate::errors::{AlreadyReported, Error, Fallible};
use crate::runtime::{AssignableRuntimeValue, AssignableValue, Runtime};

pub type AssignableCompileValue<'f> = AssignableValue<'f, Compiler>;

/// Returns the built-in function with the given name.
pub fn resolve_function(name: &str) -> Option<Box<dyn Function>> {
    match name {
        _ => None,
    }
}

/// Returns the built-in constant with the given name.
pub fn resolve_constant(name: &str) -> Option<Val> {
    match name {
        // Type keywords
        "Integer" => Some(RtVal::Type(Type::Integer).into()),
        "Cell" => Some(RtVal::Type(Type::Cell).into()),
        "Tag" => Some(RtVal::Type(Type::Tag).into()),
        "String" => Some(RtVal::Type(Type::String).into()),
        "Type" => Some(RtVal::Type(Type::Type).into()),
        "Null" => Some(RtVal::Type(Type::Null).into()),
        "Vector" => Some(RtVal::Type(Type::Vector(None)).into()),
        "Array" => Some(RtVal::Type(Type::Array(None)).into()),
        "IntegerSet" => Some(RtVal::Type(Type::IntegerSet).into()),
        "CellSet" => Some(RtVal::Type(Type::CellSet).into()),
        "VectorSet" => Some(RtVal::Type(Type::VectorSet(None)).into()),
        "Pattern" => Some(RtVal::Type(Type::Pattern(None)).into()),
        "Regex" => Some(RtVal::Type(Type::Regex).into()),

        _ => None,
    }
}

impl From<ast::BinaryOp> for Box<dyn Function> {
    fn from(op: ast::BinaryOp) -> Self {
        match op {
            ast::BinaryOp::Add => Box::new(math::BinaryOp::Add),
            ast::BinaryOp::Sub => Box::new(math::BinaryOp::Sub),
            ast::BinaryOp::Mul => Box::new(math::BinaryOp::Mul),
            ast::BinaryOp::Div => Box::new(math::BinaryOp::Div),
            ast::BinaryOp::Mod => Box::new(math::BinaryOp::Mod),
            ast::BinaryOp::Pow => Box::new(math::BinaryOp::Pow),
            ast::BinaryOp::Shl => Box::new(math::BinaryOp::Shl),
            ast::BinaryOp::ShrSigned => Box::new(math::BinaryOp::ShrSigned),
            ast::BinaryOp::ShrUnsigned => Box::new(math::BinaryOp::ShrUnsigned),
            ast::BinaryOp::BitwiseAnd => Box::new(math::BinaryOp::BitwiseAnd),
            ast::BinaryOp::BitwiseOr => Box::new(math::BinaryOp::BitwiseOr),
            ast::BinaryOp::BitwiseXor => Box::new(math::BinaryOp::BitwiseXor),
            ast::BinaryOp::LogicalAnd => todo!(),
            ast::BinaryOp::LogicalOr => todo!(),
            ast::BinaryOp::LogicalXor => todo!(),
            ast::BinaryOp::Range => todo!(),
            ast::BinaryOp::Is => todo!(),
        }
    }
}

/// Statement that can be executed and/or compiled.
///
/// Most of the interpreter and compiler code is in implementations of this
/// trait and [`Function`].
pub trait Statement: fmt::Debug {
    /// Executes the statement.
    fn execute(&self, runtime: &mut Runtime, call: FuncCall<'_>) -> Fallible<()>;
    /// Compiles code to execute the statement.
    ///
    /// The default implementation unconditionally returns an error stating that
    /// this expression cannot be compiled.
    fn compile(&self, compiler: &mut Compiler, call: FuncCall<'_>) -> Fallible<()> {
        Err(compiler.error(Error::cannot_compile(call.span)))
    }
}

/// Function that can be evaluated and/or compiled, taking zero or more
/// arguments and returning a value.
///
/// Most of the interpreter and compiler code is in implementations of this
/// trait and [`Statement`].
pub trait Function: fmt::Debug + fmt::Display {
    /// Executes the expression and returns the resulting value.
    fn eval(&self, runtime: &mut Runtime, call: FuncCall<'_>) -> Fallible<RtVal>;
    /// Compiles code to evaluate the expression and returns the resulting
    /// value.
    ///
    /// The default implementation unconditionally returns an error stating that
    /// this expression cannot be compiled.
    fn compile(&self, compiler: &mut Compiler, call: FuncCall<'_>) -> Fallible<Val> {
        Err(compiler.error(Error::cannot_compile(call.span)))
    }

    /// Executes an assignment to the expression.
    ///
    /// The default implementation unconditionally returns an expression stating
    /// that the expression cannot be assigned to.
    fn execute_assign<'a>(
        &'a self,
        runtime: &mut Runtime,
        call: FuncCall,
        _assign_rhs: RtVal,
    ) -> Fallible<AssignableRuntimeValue<'a>> {
        Err(runtime.error(Error::cannot_assign_to(call.span).into()))
    }
    /// Compiles code to assign to the expression.
    ///
    /// **Override this method for any expression that can be assigned to, even
    /// if it cannot be compiled.** The default implementation unconditionally
    /// returns an error stating that the expression cannot be assigned to.
    fn compile_assign<'a>(
        &'a self,
        compiler: &mut Compiler,
        call: FuncCall,
        _assign_rhs: Val,
    ) -> Fallible<AssignableCompileValue<'a>> {
        Err(compiler.error(Error::cannot_assign_to(call.span)))
    }
}

/// Data associated with a function call.
#[derive(Debug, Copy, Clone)]
pub struct FuncCall<'a> {
    /// Span of this expression in the original source code.
    pub span: Span,
    /// Arguments (other expressions) passed to the function.
    pub args: &'a [ast::Expr<'a>],
}
impl FuncCall<'_> {
    pub fn eval_args(self, runtime: &mut Runtime) -> Fallible<Vec<Spanned<RtVal>>> {
        self.args
            .iter()
            .map(|&arg_expr| runtime.eval_expr(arg_expr))
            .collect()
    }

    pub fn compile_args(self, compiler: &mut Compiler) -> Fallible<Vec<Spanned<Val>>> {
        self.args
            .iter()
            .map(|&arg_expr| compiler.compile_expr(arg_expr))
            .collect()
    }
}
