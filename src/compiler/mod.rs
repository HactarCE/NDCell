use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::IntType;
use inkwell::values::{FunctionValue, IntValue, PointerValue};
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use std::collections::HashMap;

mod value;

use super::types::{LangCellState, CELL_STATE_BITS, INT_BITS};
use super::{ast, errors::*, Span, Spanned, CELL_STATE_COUNT};
use value::*;
use LangErrorMsg::{CellStateOutOfRange, InternalError};

/// Convenience type alias for a transition function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't do
/// `unsafe` operations internally.
///
/// The highest bit (2 << 63) of this function's return value determines whether
/// the result was successful; 0 = success, 1 = failure. If the function was
/// successful, then the remaining bits encode the resultant cell state; if the
/// function was unsuccessful, then the remaining bits encode the error index.
type RawTransitionFunction = JitFunction<unsafe extern "C" fn() -> u64>;

pub struct TransitionFunction {
    jit_fn: RawTransitionFunction,
    error_points: Vec<LangError>,
}
impl TransitionFunction {
    pub fn call(&self) -> LangResult<LangCellState> {
        let result = unsafe { self.jit_fn.call() };
        if result & (1 << 63) == 0 {
            Ok(result as LangCellState)
        } else {
            let error_index = result & !(1 << 63);
            Err(self
                .error_points
                .get(error_index as usize)
                .cloned()
                .unwrap_or(InternalError("Error index out of range".into()).into()))
        }
    }
}

pub struct Compiler<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,

    return_type: IntType<'ctx>,
    int_type: IntType<'ctx>,
    cell_state_type: IntType<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,

    error_points: Vec<LangError>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(ctx: &'ctx Context) -> LangResult<Self> {
        let module = ctx.create_module("automaton");
        let builder = ctx.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
        Ok(Self {
            ctx,
            module,
            builder,
            execution_engine,

            return_type: ctx.i64_type(),
            int_type: ctx.custom_width_int_type(INT_BITS),
            cell_state_type: ctx.custom_width_int_type(CELL_STATE_BITS),

            variables: HashMap::new(),
            fn_value_opt: None,

            error_points: vec![],
        })
    }

    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt
            .expect("No function value in JIT compiler")
    }

    pub fn jit_compile_transition_fn(
        &mut self,
        statements: &ast::StatementBlock,
    ) -> LangResult<TransitionFunction> {
        // Create the function type with no arguments and no varargs.
        let fn_type = self.return_type.fn_type(&[], false);
        self.fn_value_opt = Some(
            self.module
                .add_function("transition_function", fn_type, None),
        );

        let basic_block = self.ctx.append_basic_block(self.fn_value(), "entry");
        self.builder.position_at_end(basic_block);

        for statement in statements {
            use ast::Statement::*;
            match &statement.inner {
                Become(expr) | Return(expr) => {
                    let return_value = self
                        .jit_compile_expr(expr)?
                        .as_cell_state()?
                        .const_z_ext(self.return_type);
                    self.builder.build_return(Some(&return_value));
                }
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

        if self.fn_value().verify(true) {
            Ok(TransitionFunction {
                jit_fn: unsafe {
                    self.execution_engine
                        .get_function("transition_function")
                        .expect("Failed to find JIT-compiled transition function")
                },
                error_points: std::mem::replace(&mut self.error_points, vec![]),
            })
        } else {
            unsafe { self.fn_value().delete() };
            Err(InternalError("LLVM function verification failed".into()).without_span())
        }
    }

    fn jit_compile_expr(
        &mut self,
        expression: &Spanned<ast::Expr>,
    ) -> LangResult<Spanned<Value<'ctx>>> {
        let span = expression.span;
        use ast::Expr::*;
        Ok(Spanned {
            span,
            inner: match &expression.inner {
                Int(i) => Value::Int(self.int_type.const_int(*i as u64, true)),
                Tag(expr) => Value::CellState({
                    let x = self.jit_compile_expr(expr)?.as_int()?;
                    let ret = self.builder.build_int_truncate(
                        x,
                        self.cell_state_type,
                        "tmp_intToCellstate",
                    );
                    self.build_cell_state_value_check(ret, span);
                    ret
                }),
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
                Var(_name) => unimplemented!(),
            },
        })
    }

    fn build_cell_state_value_check(&mut self, cell_state_value: IntValue, span: Span) {
        let parent = self.fn_value();
        // Treat the signed integer as an unsigned integer, and create a
        // condition testing whether cell_state_value is less than the number of
        // cell states. (A Negative number will be interpreted as a very large
        // positive number, which will be too large.)
        let cell_state_count_value = cell_state_value
            .get_type()
            .const_int(CELL_STATE_COUNT as u64, false);
        let condition = self.builder.build_int_compare(
            IntPredicate::ULT, // Unsigned Less-Than
            cell_state_value,
            cell_state_count_value,
            "cellStateRangeCheck",
        );

        // Build the branches
        let out_of_range_bb = self.ctx.append_basic_block(parent, "cellStateOutOfRange");
        let in_range_bb = self.ctx.append_basic_block(parent, "cellStateInRange");

        self.builder
            .build_conditional_branch(condition, in_range_bb, out_of_range_bb);

        self.builder.position_at_end(out_of_range_bb);
        self.build_error_point(CellStateOutOfRange.with_span(span));

        self.builder.position_at_end(in_range_bb);
    }

    fn build_error_point(&mut self, error: LangError) {
        let error_value = self.add_error_point(error) | (1 << 63);
        self.builder
            .build_return(Some(&self.return_type.const_int(error_value as u64, true)));
    }

    fn add_error_point(&mut self, error: LangError) -> usize {
        self.error_points.push(error);
        self.error_points.len() - 1
    }

    /// Returns true if the current basic block needs a terminator, or false
    /// otherwise.
    fn needs_terminator(&self) -> bool {
        if let Some(block) = self.builder.get_insert_block() {
            block.get_terminator().is_none()
        } else {
            false
        }
    }
}
