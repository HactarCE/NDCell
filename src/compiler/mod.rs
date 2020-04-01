use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::IntType;
use inkwell::OptimizationLevel;
use std::error::Error;

mod value;

use super::ast;
use super::errors::*;
use super::Spanned;
use value::*;

/// Convenience type alias for a transition function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't do
/// `unsafe` operations internally.
type TransitionFunction = unsafe extern "C" fn() -> i64;

pub struct Compiler<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,

    int_type: IntType<'ctx>,
    cell_state_type: IntType<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Result<Self, Box<dyn Error>> {
        let module = ctx.create_module("sum");
        let builder = ctx.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
        Ok(Self {
            ctx,
            module,
            builder,
            execution_engine,
            int_type: ctx.custom_width_int_type(INT_BITS),
            cell_state_type: ctx.custom_width_int_type(CELL_STATE_BITS),
        })
    }

    pub fn jit_compile_transition_fn(
        &self,
        statements: &ast::StatementBlock,
    ) -> LangResult<JitFunction<TransitionFunction>> {
        // Create the function type with no arguments and no varargs.
        let fn_type = self.cell_state_type.fn_type(&[], false);
        let function = self
            .module
            .add_function("transition_function", fn_type, None);

        let basic_block = self.ctx.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        for statement in statements {
            use ast::Statement::*;
            match &statement.inner {
                Become(expr) | Return(expr) => self
                    .builder
                    .build_return(Some(&self.jit_compile_expr(expr)?.as_cell_state()?)),
                Goto(_) => panic!("Got GOTO statement while compiling"),
                End => panic!("Got END statement while compiling"),
            };
        }

        if self.needs_terminator() {
            // Implicit `return #0` at the end of the transition function. TODO
            // change this to `remain`, once that's implemented.
            self.builder
                .build_return(Some(&self.cell_state_type.const_zero()));
        }

        if function.verify(true) {
            Ok(unsafe {
                self.execution_engine
                    .get_function("transition_function")
                    .expect("Failed to find JIT-compiled transition function")
            })
        } else {
            unsafe { function.delete() };
            lang_err("Internal error while compiling: function.verify() returned false")
        }
    }

    fn jit_compile_expr(&self, expression: &Spanned<ast::Expr>) -> LangResult<Spanned<Value>> {
        let span = expression.span;
        use ast::Expr::*;
        Ok(Spanned {
            span,
            inner: match &expression.inner {
                Int(i) => Value::Int(self.int_type.const_int(*i as u64, true)),
                Tag(expr) => Value::CellState(self.builder.build_int_truncate(
                    self.jit_compile_expr(expr)?.as_int()?,
                    self.cell_state_type,
                    "tmp_intToCellstate",
                )),
                Neg(expr) => Value::Int({
                    // TODO check for overflow
                    let x = self.jit_compile_expr(expr)?.as_int()?;
                    self.builder.build_int_neg(x, "tmp_neg")
                }),
                Add(expr1, expr2) => Value::Int({
                    let lhs = self.jit_compile_expr(expr1)?.as_int()?;
                    let rhs = self.jit_compile_expr(expr2)?.as_int()?;
                    // TODO check for overflow
                    self.builder.build_int_add(lhs, rhs, "tmp_add")
                }),
                Sub(expr1, expr2) => Value::Int({
                    let lhs = self.jit_compile_expr(expr1)?.as_int()?;
                    let rhs = self.jit_compile_expr(expr2)?.as_int()?;
                    // TODO check for overflow
                    self.builder.build_int_sub(lhs, rhs, "tmp_sub")
                }),
                Var(name) => unimplemented!(),
            },
        })
    }

    fn needs_terminator(&self) -> bool {
        if let Some(block) = self.builder.get_insert_block() {
            block.get_terminator().is_none()
        } else {
            false
        }
    }
}
