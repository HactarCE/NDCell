use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::{Linkage, Module};
use inkwell::types::IntType;
use inkwell::values::{FunctionValue, IntValue, PointerValue};
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use std::collections::HashMap;

mod value;

use super::types::{LangCellState, CELL_STATE_BITS, INT_BITS};
use super::{ast, errors::*, Span, Spanned, CELL_STATE_COUNT};
use value::*;
use LangErrorMsg::{
    CellStateOutOfRange, ComparisonError, IntegerOverflowDuringAddition, InternalError,
};

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
        // This module takes responsibility for JIT-related unsafety because the
        // JITting happened in this module.
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
        }
        .with_extern_prototypes())
    }

    fn get_checked_int_arithmetic_function_name(&self, name: &str) -> String {
        format!(
            "llvm.{}.with.overflow.i{}",
            name,
            self.int_type.get_bit_width()
        )
    }

    fn with_extern_prototypes(self) -> Self {
        let int_param_types = &[self.int_type.into(), self.int_type.into()];
        let int_with_error_type = self
            .ctx
            .struct_type(&[self.int_type.into(), self.ctx.bool_type().into()], false);
        for name in &["sadd", "ssub", "smul"] {
            self.module.add_function(
                &self.get_checked_int_arithmetic_function_name(name),
                int_with_error_type.fn_type(int_param_types, false),
                Some(Linkage::External),
            );
        }
        self
    }

    fn get_function(&self, name: &str) -> LangResult<FunctionValue<'ctx>> {
        self.module.get_function(name).ok_or_else(|| {
            InternalError(format!("Could not find LLVM function '{}'", name).into()).without_span()
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

        let basic_block = self.append_basic_block("entry");
        self.builder.position_at_end(basic_block);
        self.build_statements(statements)?;

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
            eprintln!(
                "Error encountered during function compilation; dumping LLVM function to stderr"
            );
            self.fn_value().print_to_stderr();
            unsafe { self.fn_value().delete() };
            Err(InternalError("LLVM function verification failed".into()).without_span())
        }
    }

    pub fn build_statements(&mut self, statements: &ast::StatementBlock) -> LangResult<()> {
        for statement in statements {
            use ast::Statement::*;
            match &statement.inner {
                If(expr, if_true, if_false_maybe) => {
                    // Evaluate the condition and get a boolean value.
                    let test_value = self.build_expr(&expr)?.as_int()?;
                    let condition_value = self.builder.build_int_compare(
                        IntPredicate::EQ,
                        test_value,
                        self.int_type.const_zero(),
                        "tmp_ifCond",
                    );
                    self.build_conditional(
                        condition_value,
                        |c| c.build_statements(if_true),
                        |c| match if_false_maybe {
                            Some(if_false) => c.build_statements(if_false),
                            None => Ok(()),
                        },
                    )?;
                }
                Become(expr) | Return(expr) => {
                    let return_cell_state = self.build_expr(&expr)?.as_cell_state()?;
                    let return_value = self.builder.build_int_cast(
                        return_cell_state,
                        self.return_type,
                        "tmp_retValFromCellState",
                    );
                    self.builder.build_return(Some(&return_value));
                    return Ok(());
                }
                Goto(_) => panic!("Got GOTO statement while compiling"),
                End => panic!("Got END statement while compiling"),
            };
        }
        Ok(())
    }

    fn build_expr(&mut self, expression: &Spanned<ast::Expr>) -> LangResult<Spanned<Value<'ctx>>> {
        let span = expression.span;
        use ast::Expr::*;
        Ok(Spanned {
            span,
            inner: match &expression.inner {
                Int(i) => Value::Int(self.int_type.const_int(*i as u64, true)),
                Tag(expr) => Value::CellState({
                    let x = self.build_expr(expr)?.as_int()?;
                    let ret = self.builder.build_int_cast(
                        x,
                        self.cell_state_type,
                        "tmp_cellStateFromInt",
                    );
                    self.build_cell_state_value_check(ret, span);
                    ret
                }),
                Neg(expr) => Value::Int({
                    let x = self.build_expr(expr)?.as_int()?;
                    self.build_neg_check(x, span)?;
                    self.builder.build_int_neg(x, "tmp_neg")
                }),
                Add(expr1, expr2) => Value::Int({
                    let lhs = self.build_expr(expr1)?.as_int()?;
                    let rhs = self.build_expr(expr2)?.as_int()?;
                    self.build_checked_int_arithmetic(lhs, rhs, span, "sadd")?
                }),
                Sub(expr1, expr2) => Value::Int({
                    let lhs = self.build_expr(expr1)?.as_int()?;
                    let rhs = self.build_expr(expr2)?.as_int()?;
                    self.build_checked_int_arithmetic(lhs, rhs, span, "ssub")?
                }),
                Comparison(expr1, comparisons) => {
                    Value::Int(self.build_multi_comparison(expr1, comparisons)?)
                }
                Var(_name) => unimplemented!(),
            },
        })
    }

    fn build_checked_int_arithmetic(
        &mut self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        span: Span,
        intrinsic_name: &str,
    ) -> LangResult<IntValue<'ctx>> {
        // Build a call to an LLVM intrinsic to do the operation.
        let call_site_value = self.builder.build_call(
            self.get_function(&self.get_checked_int_arithmetic_function_name(intrinsic_name))?,
            &[lhs.into(), rhs.into()],
            &format!("tmp_{}", intrinsic_name),
        );

        // Get the actual return value of the function.
        let return_value = call_site_value
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_struct_value();

        // This return value is a struct with two elements: the integer result
        // of the addition, and a boolean value which is true if overflow
        // occurred. Extract each of those.
        let result_value = self
            .builder
            .build_extract_value(return_value, 0, &format!("tmp_{}Result", intrinsic_name))
            .unwrap()
            .into_int_value();
        let overflow_value = self
            .builder
            .build_extract_value(return_value, 1, &format!("tmp_{}Overflow", intrinsic_name))
            .unwrap()
            .into_int_value();

        self.build_conditional(
            overflow_value,
            // Return an error if there was overflow.
            |c| Ok(c.build_error_point(IntegerOverflowDuringAddition.with_span(span))),
            // Otherwise proceed.
            |_| Ok(()),
        )?;

        Ok(result_value)
    }

    fn build_neg_check(&mut self, val: IntValue<'ctx>, span: Span) -> LangResult<()> {
        // self.build_statements(&[ast::Statement::If()])
        unimplemented!()
    }

    fn build_div_check(
        &mut self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        span: Span,
    ) -> LangResult<()> {
        unimplemented!()
    }

    fn build_cell_state_value_check(&mut self, cell_state_value: IntValue, span: Span) {
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
        let out_of_range_bb = self.append_basic_block("cellStateOutOfRange");
        let in_range_bb = self.append_basic_block("cellStateInRange");

        self.builder
            .build_conditional_branch(condition, in_range_bb, out_of_range_bb);

        self.builder.position_at_end(out_of_range_bb);
        self.build_error_point(CellStateOutOfRange.with_span(span));

        self.builder.position_at_end(in_range_bb);
    }

    fn build_multi_comparison(
        &mut self,
        expr1: &Spanned<ast::Expr>,
        comparisons: &[(ast::Comparison, Spanned<ast::Expr>)],
    ) -> LangResult<IntValue<'ctx>> {
        let old_bb = self.builder.get_insert_block().unwrap();

        // Build a basic block to skip to if the condition is false. The last
        // condition will have an unconditional jump.
        let merge_bb = self.append_basic_block("multiCompareShortCircuit");
        self.builder.position_at_end(merge_bb);
        // Create a phi node for the final result.
        let phi = self.builder.build_phi(self.int_type, "multiCompareMerge");

        self.builder.position_at_end(old_bb);
        let mut lhs = self.build_expr(&expr1)?;
        for (comparison, expr2) in comparisons {
            let rhs = self.build_expr(expr2)?;
            let compare_result = self.build_comparison(*comparison, &lhs, &rhs)?;
            // If the condition is false, skip ahead to the merge and give the
            // phi node a value of 0. If it is true, continue on to check the
            // next condition.
            let next_bb = self.append_basic_block("compare");
            self.builder
                .build_conditional_branch(compare_result, next_bb, merge_bb);
            phi.add_incoming(&[(
                &self.int_type.const_zero(),
                self.builder.get_insert_block().unwrap(),
            )]);
            self.builder.position_at_end(next_bb);
            // The current RHS will be the next condition's LHS.
            lhs = rhs;
        }

        // After the last comparison, unconditionally jump directly to the merge
        // block and give the phi node a value of 1 because all conditions were
        // true.
        self.builder.build_unconditional_branch(merge_bb);
        phi.add_incoming(&[(
            &self.int_type.const_int(1, false),
            self.builder.get_insert_block().unwrap(),
        )]);

        // Position the builder at the end of the merge block for later
        // instructions.
        self.builder.position_at_end(merge_bb);

        // This phi node now contains 1 if all conditions were true and 0 if all
        // conditions were false.
        Ok(phi.as_basic_value().into_int_value())
    }

    fn build_comparison(
        &mut self,
        comparison: ast::Comparison,
        lhs: &Spanned<Value<'ctx>>,
        rhs: &Spanned<Value<'ctx>>,
    ) -> LangResult<IntValue<'ctx>> {
        use ast::Comparison::*;
        use Value::*;
        let int_predicate = match comparison {
            Equal => IntPredicate::EQ,
            NotEqual => IntPredicate::NE,
            LessThan => IntPredicate::ULT,
            GreaterThan => IntPredicate::UGT,
            LessThanOrEqual => IntPredicate::ULE,
            GreaterThanOrEqual => IntPredicate::UGE,
        };
        Ok(match (&lhs.inner, &rhs.inner, comparison) {
            // Integer comparison
            (Int(a), Int(b), _) => self
                .builder
                .build_int_compare(int_predicate, *a, *b, "intCmp"),
            // Cell state comparison
            (CellState(a), CellState(b), Equal) | (CellState(a), CellState(b), NotEqual) => self
                .builder
                .build_int_compare(int_predicate, *a, *b, "cellStateCmp"),
            // Error
            _ => Err(ComparisonError {
                cmp_sym: comparison.get_symbol(),
                lhs: lhs.inner.get_type(),
                rhs: rhs.inner.get_type(),
            }
            .with_span(Span::merge(lhs, rhs)))?,
        })
    }

    fn build_conditional(
        &mut self,
        condition_value: IntValue<'ctx>,
        build_if_true: impl FnOnce(&mut Self) -> LangResult<()>,
        build_if_false: impl FnOnce(&mut Self) -> LangResult<()>,
    ) -> LangResult<()> {
        // Build the destination blocks.
        let if_true_bb = self.append_basic_block("ifTrue");
        let if_false_bb = self.append_basic_block("ifFalse");
        let merge_bb = self.append_basic_block("endIf");
        // Build the branch instruction.
        self.builder
            .build_conditional_branch(condition_value, if_false_bb, if_true_bb);
        // Build the instructions to execute if true.
        self.builder.position_at_end(if_true_bb);
        build_if_true(self)?;
        if self.needs_terminator() {
            self.builder.build_unconditional_branch(merge_bb);
        }
        // Build the instructions to execute if false.
        self.builder.position_at_end(if_false_bb);
        build_if_false(self)?;
        if self.needs_terminator() {
            self.builder.build_unconditional_branch(merge_bb);
        }
        self.builder.position_at_end(merge_bb);
        Ok(())
    }

    fn needs_terminator(&self) -> bool {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
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

    fn append_basic_block(&self, name: &str) -> BasicBlock<'ctx> {
        self.ctx.append_basic_block(self.fn_value(), name)
    }
}
