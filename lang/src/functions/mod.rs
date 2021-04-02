use codemap::{Span, Spanned};
use std::fmt;

use crate::ast;
use crate::compiler::{CompileValue, Compiler};
use crate::data::Value;
use crate::errors::Error;
use crate::runtime::{
    AssignableRuntimeValue, AssignableValue, Runtime, RuntimeError, RuntimeResult,
};

pub mod math;

pub type CompileResult<T> = Result<T, Error>; // TODO: redundant with crate::errors::Result

pub type AssignableCompileValue<'f> =
    AssignableValue<'f, Compiler, CompileValue, ast::AlreadyReported>;

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
    fn execute(&self, runtime: &mut Runtime, call: FuncCall<'_>) -> RuntimeResult<()>;
    /// Compiles code to execute the statement.
    ///
    /// The default implementation unconditionally returns an error stating that
    /// this expression cannot be compiled.
    fn compile(&self, compiler: &mut Compiler, call: FuncCall<'_>) -> CompileResult<()> {
        Err(Error::cannot_compile(call.span))
    }
}

/// Function that can be evaluated and/or compiled, taking zero or more
/// arguments and returning a value.
///
/// Most of the interpreter and compiler code is in implementations of this
/// trait and [`Statement`].
pub trait Function: fmt::Debug + fmt::Display {
    /// Executes the expression and returns the resulting value.
    fn eval(&self, runtime: &mut Runtime, call: FuncCall<'_>) -> RuntimeResult<Value>;
    /// Compiles code to evaluate the expression and returns the resulting
    /// value.
    ///
    /// The default implementation unconditionally returns an error stating that
    /// this expression cannot be compiled.
    fn compile(
        &self,
        compiler: &mut Compiler,
        call: FuncCall<'_>,
    ) -> CompileResult<Spanned<CompileValue>> {
        Err(Error::cannot_compile(call.span))
    }

    /// Executes an assignment to the expression.
    ///
    /// The default implementation unconditionally returns an expression stating
    /// that the expression cannot be assigned to.
    fn execute_assign<'a>(
        &'a self,
        runtime: &mut Runtime,
        call: FuncCall,
        _assign_rhs: Value,
    ) -> RuntimeResult<AssignableRuntimeValue<'a>> {
        Err(Error::cannot_assign_to(call.span).into())
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
        _assign_rhs: CompileValue,
    ) -> CompileResult<AssignableCompileValue<'a>> {
        Err(Error::cannot_assign_to(call.span))
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
    pub fn eval_args(self, runtime: &mut Runtime) -> RuntimeResult<Vec<Spanned<Value>>> {
        self.args
            .iter()
            .map(|&arg_expr| runtime.eval_expr(arg_expr))
            .collect()
    }
}
