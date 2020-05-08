//! The JIT compiler for NDCA.

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicTypeEnum, IntType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::rc::Rc;
use thread_local::ThreadLocal;

mod value;
pub use value::Value;

use super::types::{LangCellState, Type, CELL_STATE_BITS, INT_BITS};
use super::{ast, errors::*, Span, Spanned, CELL_STATE_COUNT};
use LangErrorMsg::{
    CellStateOutOfRange, DivideByZero, IntegerOverflow, InternalError, Unimplemented,
};

lazy_static! {
    static ref CTX: ThreadLocal<Context> = ThreadLocal::new();
}
fn get_ctx() -> &'static Context {
    &CTX.get_or(Context::create)
}

/// Jit-compiles a rule.
pub fn jit_compile_rule(rule: ast::Rule) -> CompleteLangResult<CompiledRule> {
    let source_code = rule.source_code.clone();
    _jit_compile_rule(rule).map_err(|e| e.with_source(&source_code))
}

fn _jit_compile_rule(rule: ast::Rule) -> LangResult<CompiledRule> {
    let source_code = rule.source_code;
    let jit_transition_fn: RawTransitionFunction;
    let error_points;
    {
        let mut compiler = Compiler::new(get_ctx())?;
        compiler.jit_compile_fn(&rule.transition_fn)?;
        jit_transition_fn = unsafe {
            compiler
                .execution_engine
                .get_function("transition_function")
                .expect("Failed to find JIT-compiled transition function")
        };
        error_points = compiler.error_points;
    }
    Ok(CompiledRule {
        source_code,
        jit_transition_fn: Some(jit_transition_fn),
        error_points,
    })
}

/// Convenience type alias for a transition function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't do
/// `unsafe` operations internally.
///
/// The highest bit (2 << 63) of this function's return value determines whether
/// the result was successful; 0 = success, 1 = failure. If the function was
/// successful, then the remaining bits encode the resultant cell state; if the
/// function was unsuccessful, then the remaining bits encode the error index.
type RawTransitionFunction = JitFunction<'static, unsafe extern "C" fn() -> u64>;

/// Compiled rule.
#[derive(Debug)]
pub struct CompiledRule {
    /// Original source code for the rule.
    source_code: Rc<String>,
    /// JIT transition function.
    jit_transition_fn: Option<RawTransitionFunction>,
    /// List of possible errors that can be returned.
    error_points: Vec<LangError>,
}
impl CompiledRule {
    /// Call the JIT-compiled transition function and get a cell state result.
    pub fn call_transition_fn(&self) -> CompleteLangResult<LangCellState> {
        // This module takes responsibility for JIT-related unsafety because the
        // JITting happened in this module.
        let jit_fn = self.jit_transition_fn.as_ref().unwrap();
        let result = unsafe { jit_fn.call() };
        // Test the highest bit to see if there was an error.
        if result & (1 << 63) == 0 {
            // There was no error.
            Ok(result as LangCellState)
        } else {
            // There was an error. Get the index of the error and return the
            // corresponding LangError.
            let error_index = result & !(1 << 63);
            Err(self
                .error_points
                .get(error_index as usize)
                .cloned()
                .unwrap_or(InternalError("Error index out of range".into()).without_span())
                .with_source(&self.source_code))
        }
    }
}

/// A variable with associated allocated memory.
#[derive(Debug, Copy, Clone)]
struct Variable<'ctx> {
    /// Type of this variable.
    ty: Type,
    /// LLVM pointer to the memory allocated for this variable.
    ptr: PointerValue<'ctx>,
}

#[derive(Debug)]
/// JIT-compiler.
struct Compiler<'ctx> {
    /// LLVM context.
    ctx: &'ctx Context,
    /// LLVM module.
    module: Module<'ctx>,
    /// LLVM instruction builder, always positioned on a basic block in the
    /// module.
    builder: Builder<'ctx>,
    /// The JIT execution engine.
    execution_engine: ExecutionEngine<'ctx>,

    /// The LLVM type of the return type of this function.
    llvm_return_type: IntType<'ctx>,
    /// The LLVM type used to represent an integer.
    llvm_int_type: IntType<'ctx>,
    /// The LLVM type used to represent a cell state.
    llvm_cell_state_type: IntType<'ctx>,

    /// The function type of the function that is being compiled (e.g.
    /// transition vs. helper function and return type).
    function_type: Option<ast::FunctionType>,
    /// Variables by name.
    vars: HashMap<String, Variable<'ctx>>,
    /// LLVM function.
    fn_value_opt: Option<FunctionValue<'ctx>>,

    /// Possible errors that could be returned from this function.
    error_points: Vec<LangError>,
}

impl<'ctx> Compiler<'ctx> {
    /// Constructs a new Compiler from an LLVM context.
    pub fn new(ctx: &'ctx Context) -> LangResult<Self> {
        let module = ctx.create_module("automaton");
        let builder = ctx.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;

        Ok(Self {
            ctx,
            module,
            builder,
            execution_engine,

            llvm_return_type: ctx.i64_type(),
            llvm_int_type: ctx.custom_width_int_type(INT_BITS),
            llvm_cell_state_type: ctx.custom_width_int_type(CELL_STATE_BITS),

            function_type: None,
            vars: HashMap::new(),
            fn_value_opt: None,

            error_points: vec![],
        }
        .with_extern_prototypes())
    }

    /// Returns the name of the LLVM intrinsic that performs the given operation
    /// with overflow checking.
    fn get_checked_int_arithmetic_function_name(&self, name: &str) -> String {
        // LLVM has a bunch of overflow-checking intrinsics with names like
        // `llvm.sadd.with.overflow.i64` ("sadd" = "signed add"), and they are
        // predictable enough that we can generate them like this.
        format!(
            "llvm.{}.with.overflow.i{}",
            name,
            self.llvm_int_type.get_bit_width()
        )
    }

    /// Adds extern function prototypes for some handy LLVM intrinsics.
    fn with_extern_prototypes(self) -> Self {
        // Define extern function prototypes for various LLVM intrinsics so that
        // we can call them like normal functions.
        let int_param_types = &[self.llvm_int_type.into(), self.llvm_int_type.into()];
        let int_with_error_type = self.ctx.struct_type(
            &[self.llvm_int_type.into(), self.ctx.bool_type().into()],
            false,
        );
        // "sadd" = signed addition, "ssub" = signed subtraction, "smul" =
        // signed multiplication
        for name in &["sadd", "ssub", "smul"] {
            self.module.add_function(
                &self.get_checked_int_arithmetic_function_name(name),
                int_with_error_type.fn_type(int_param_types, false),
                Some(Linkage::External),
            );
        }
        self
    }

    /// Returns an LLVM function by name.
    fn get_function(&self, name: &str) -> LangResult<FunctionValue<'ctx>> {
        self.module.get_function(name).ok_or_else(|| {
            InternalError(format!("Could not find LLVM function '{}'", name).into()).without_span()
        })
    }

    /// Returns the LLVM function that is currently being built.
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt
            .expect("No function value in JIT compiler")
    }

    /// JIT-compiles a function.
    fn jit_compile_fn(&mut self, function: &ast::Function) -> LangResult<()> {
        self.function_type = Some(function.fn_type);

        // Create the function type with no arguments and no varargs.
        let fn_type = self.llvm_return_type.fn_type(&[], false);
        self.fn_value_opt = Some(
            // TODO helper functions need a different name.
            self.module
                .add_function("transition_function", fn_type, None),
        );

        let entry_bb = self.append_basic_block("entry");
        self.builder.position_at_end(entry_bb);

        // Declare and initialize variables.
        for (var_name, ty) in function.vars.iter() {
            self.declare_var(var_name.to_owned(), *ty)?;
            self.initialize_var(var_name)?;
        }

        // Build statements.
        self.build_statements(&function.statements)?;

        if self.needs_terminator() {
            // Implicit `return #0` at the end of the transition function. TODO
            // change this to `remain`, once that's implemented.
            self.builder
                .build_return(Some(&self.llvm_return_type.const_zero()));
        }

        // Make sure that the LLVM code we generated is valid.
        if self.fn_value().verify(true) {
            Ok(())
        } else {
            eprintln!(
                "Error encountered during function compilation; dumping LLVM function to stderr"
            );
            self.fn_value().print_to_stderr();
            unsafe { self.fn_value().delete() };
            Err(InternalError("LLVM function verification failed".into()).without_span())
        }
    }

    /// Builds LLVM instructions to execute the given statements.
    pub fn build_statements(&mut self, statements: &ast::StatementBlock) -> LangResult<()> {
        for statement in statements {
            use ast::Statement::*;
            match &statement.inner {
                SetVar {
                    var_name,
                    value_expr,
                } => {
                    let var = self.vars[&var_name.inner];
                    if var.ty != value_expr.ty() {
                        Err(InternalError(
                            "Invalid variable assignment not caught by type checker".into(),
                        )
                        .without_span())?;
                    }
                    // Compile the expression and store the result in the
                    // variable, depending on the type.
                    match value_expr {
                        ast::Expr::Int(e) => {
                            let value = self.build_int_expr(e)?.inner;
                            self.builder.build_store(var.ptr, value)
                        }
                        ast::Expr::CellState(e) => {
                            let value = self.build_cell_state_expr(e)?.inner;
                            self.builder.build_store(var.ptr, value)
                        }
                    };
                }

                If {
                    cond_expr,
                    if_true,
                    if_false,
                } => {
                    // Evaluate the condition and get a boolean value.
                    let condition_value = self.build_int_expr(&cond_expr)?.inner;
                    // Check whether condition_value != 0.
                    // Now branch based on that boolean value.
                    self.build_conditional(
                        condition_value,
                        |c| c.build_statements(if_true),  // != 0
                        |c| c.build_statements(if_false), // == 0
                    )?;
                }

                Return(return_expr) => {
                    match self.function_type.unwrap() {
                        ast::FunctionType::Transition => {
                            // This is a transition function, so the return type
                            // should be a cell state.
                            if Type::CellState != return_expr.ty() {
                                Err(InternalError(
                                    "Invalid return statement not caught by type checker".into(),
                                )
                                .without_span())?;
                            }
                            let return_cell_state =
                                self.build_cell_state_expr(return_expr.as_cell_state_expr()?)?;
                            let return_value = self.builder.build_int_cast(
                                return_cell_state.inner,
                                self.llvm_return_type,
                                "tmp_retValFromCellState",
                            );
                            self.builder.build_return(Some(&return_value));
                        }
                        ast::FunctionType::Helper(_) => Err(Unimplemented.with_span(return_expr))?,
                    };
                    // Don't build any more statements after this!
                    return Ok(());
                }

                // Goto and End statements are only meant for the interpreter.
                Goto(_) => panic!("GOTO statement found in AST given to compiler"),
                End => panic!("END statement found in AST given to compiler"),
            };
        }
        Ok(())
    }

    /// Builds LLVM instructions to evaluate the given expression that evaluates
    /// to an integer.
    fn build_int_expr(
        &mut self,
        expression: &Spanned<ast::IntExpr>,
    ) -> LangResult<Spanned<IntValue<'ctx>>> {
        let span = expression.span;
        use ast::IntExpr::*;
        Ok(Spanned {
            span,
            inner: match &expression.inner {
                FnCall(_) => Err(Unimplemented.with_span(span))?,

                Var(var_name) => self
                    .builder
                    .build_load(self.vars[var_name].ptr, var_name)
                    .into_int_value(),

                Literal(i) => self.llvm_int_type.const_int(*i as u64, true),

                Op { lhs, op, rhs } => {
                    let lhs = self.build_int_expr(lhs)?.inner;
                    let rhs = self.build_int_expr(rhs)?.inner;
                    match op {
                        ast::Op::Math(math_op) => {
                            use ast::MathOp::*;
                            match math_op {
                                // Use LLVM intrinsics to check for overflow.
                                Add => self.build_checked_int_arithmetic(lhs, rhs, span, "sadd")?,
                                Sub => self.build_checked_int_arithmetic(lhs, rhs, span, "ssub")?,
                                Mul => self.build_checked_int_arithmetic(lhs, rhs, span, "smul")?,
                                // LLVM does not provide intrinsics to check for
                                // overflow and division by zero, so we have to
                                // check ourselves with build_div_check().
                                Div => {
                                    self.build_div_check(lhs, rhs, span)?;
                                    self.builder.build_int_signed_div(lhs, rhs, "tmp_div")
                                }
                                Rem => {
                                    self.build_div_check(lhs, rhs, span)?;
                                    self.builder.build_int_signed_rem(lhs, rhs, "tmp_rem")
                                }
                                // TODO: how to implement exponentiation?
                                Exp => Err(Unimplemented.with_span(span))?,
                            }
                        }
                        _ => Err(Unimplemented.with_span(span))?,
                    }
                }

                Neg(x) => {
                    let x = self.build_int_expr(x)?.inner;
                    // To negate an integer, subtract it from zero.
                    self.build_checked_int_arithmetic(
                        self.llvm_int_type.const_zero(),
                        x,
                        span,
                        "ssub",
                    )?
                }

                CmpInt(cmp_expr) => self.build_multi_comparison(
                    cmp_expr,
                    Self::build_int_expr,
                    Self::build_int_comparison,
                )?,

                CmpCellState(cmp_expr) => self.build_multi_comparison(
                    cmp_expr,
                    Self::build_cell_state_expr,
                    // Compare cell states by integer ID.
                    Self::build_int_comparison,
                )?,
            },
        })
    }

    /// Builds LLVM instructions to evaluate the given expression that evaluates
    /// to a cell state.
    fn build_cell_state_expr(
        &mut self,
        expression: &Spanned<ast::CellStateExpr>,
    ) -> LangResult<Spanned<IntValue<'ctx>>> {
        let span = expression.span;
        use ast::CellStateExpr::*;
        Ok(Spanned {
            span,
            inner: match &expression.inner {
                FnCall(_) => Err(Unimplemented.with_span(span))?,

                Var(var_name) => self
                    .builder
                    .build_load(self.vars[var_name].ptr, var_name)
                    .into_int_value(),

                FromId(id_expr) => {
                    let id_value = self.build_int_expr(id_expr)?;
                    self.build_cell_state_value_check(id_value)?;
                    let ret = self.builder.build_int_cast(
                        id_value.inner,
                        self.llvm_cell_state_type,
                        "tmp_cellStateFromInt",
                    );
                    ret
                }
            },
        })
    }

    /// Builds LLVM instructions to perform integer arithmetic that checks for
    /// overflow and division-by-zero errors.
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
        // of the operation, and a boolean value which is true if overflow
        // occurred. Extract each of those.
        let result_value = self
            .builder
            .build_extract_value(return_value, 0, &format!("tmp_{}Result", intrinsic_name))
            .unwrap()
            .into_int_value();
        let is_overflow = self
            .builder
            .build_extract_value(return_value, 1, &format!("tmp_{}Overflow", intrinsic_name))
            .unwrap()
            .into_int_value();

        // Branch based on whether there is overflow.
        self.build_conditional(
            is_overflow,
            // Return an error if there is overflow.
            |c| Ok(c.build_error_point(IntegerOverflow.with_span(span))),
            // Otherwise proceed.
            |_| Ok(()),
        )?;

        Ok(result_value)
    }

    /// Builds LLVM instructions to check integer division arguments for
    /// overflow and division-by-zero errors (but does not actually perform said
    /// division).
    fn build_div_check(
        &mut self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        span: Span,
    ) -> LangResult<()> {
        // If the denominator (rhs) is zero, that's a DivideByZero error.
        let is_div_by_zero = self.builder.build_int_compare(
            IntPredicate::EQ,
            rhs,
            self.llvm_int_type.const_zero(),
            "isDivByZero",
        );

        // Branch based on whether the denominator is zero.
        self.build_conditional(
            is_div_by_zero,
            // The denominator is zero.
            |c| Ok(c.build_error_point(DivideByZero.with_span(span))),
            // The denominator is not zero.
            |c| {
                // If the numerator is the minimum possible value and the denominator is
                // -1, that's an IntegerOverflow error.
                let min_value = c.get_min_int_value();
                let num_is_min_value =
                    c.builder
                        .build_int_compare(IntPredicate::EQ, lhs, min_value, "isMinValue");
                let denom_is_neg_one = c.builder.build_int_compare(
                    IntPredicate::EQ,
                    rhs,
                    c.llvm_int_type.const_int(-1i64 as u64, true),
                    "isNegOne",
                );
                let is_overflow =
                    c.builder
                        .build_and(num_is_min_value, denom_is_neg_one, "isOverflow");

                // Branch based on whether there is overflow.
                c.build_conditional(
                    is_overflow,
                    // Overflow would occur.
                    |c| Ok(c.build_error_point(IntegerOverflow.with_span(span))),
                    // Overflow would not occur; it is safe to perform the division.
                    |_| Ok(()),
                )
            },
        )
    }

    /// Builds LLVM instructions to check the value of a cell state and return
    /// an error if it is not valid.
    fn build_cell_state_value_check(
        &mut self,
        cell_state_value: Spanned<IntValue<'ctx>>,
    ) -> LangResult<()> {
        // Treat the signed integer as an unsigned integer, and build a
        // condition testing whether that value is less than the number of cell
        // states. (A negative number will be interpreted as a very large
        // positive number, which will be too large.)
        let cell_state_count_value = cell_state_value
            .inner
            .get_type()
            .const_int(CELL_STATE_COUNT as u64, false);
        let condition = self.builder.build_int_compare(
            IntPredicate::ULT, // Unsigned Less-Than
            cell_state_value.inner,
            cell_state_count_value,
            "cellStateRangeCheck",
        );

        // Branch based on the whether the cell state is in range.
        self.build_conditional(
            condition,
            // The cell state is in range.
            |_| Ok(()),
            // The cell state is out of range.
            |c| {
                c.build_error_point(CellStateOutOfRange.with_span(cell_state_value));
                Ok(())
            },
        )
    }

    /// Builds LLVM instructions to perform a chained comparison check,
    /// short-circuiting when possible.
    fn build_multi_comparison<ExprType, ValueType: Copy, CmpType: Copy>(
        &mut self,
        cmp_expr: &ast::CmpExpr<ExprType, CmpType>,
        mut build_expr_fn: impl FnMut(&mut Self, &Spanned<ExprType>) -> LangResult<Spanned<ValueType>>,
        mut build_compare_fn: impl FnMut(&mut Self, CmpType, ValueType, ValueType) -> IntValue<'ctx>,
    ) -> LangResult<IntValue<'ctx>> {
        let old_bb = self.builder.get_insert_block().unwrap();

        // Build a basic block to skip to if the condition is false. The last
        // condition will have an unconditional jump.
        let merge_bb = self.append_basic_block("multiCompareShortCircuit");
        self.builder.position_at_end(merge_bb);

        // Create a phi node for the final result.
        let phi = self
            .builder
            .build_phi(self.llvm_int_type, "multiCompareMerge");

        self.builder.position_at_end(old_bb);
        let expr1 = &cmp_expr.initial;
        let mut lhs = build_expr_fn(self, expr1)?.inner;
        for (comparison, expr2) in &cmp_expr.comparisons {
            let rhs = build_expr_fn(self, expr2)?.inner;
            let compare_result = build_compare_fn(self, *comparison, lhs, rhs);
            // If the condition is false, skip ahead to the merge and give the
            // phi node a value of 0. If it is true, continue on to check the
            // next condition.
            let next_bb = self.append_basic_block("compare");
            self.builder
                .build_conditional_branch(compare_result, next_bb, merge_bb);
            phi.add_incoming(&[(
                &self.llvm_int_type.const_zero(),
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
            &self.llvm_int_type.const_int(1, false),
            self.builder.get_insert_block().unwrap(),
        )]);

        // Position the builder at the end of the merge block for later
        // instructions.
        self.builder.position_at_end(merge_bb);

        // This phi node now contains 1 if all conditions were true and 0 if all
        // conditions were false.
        Ok(phi.as_basic_value().into_int_value())
    }

    /// Build an LLVM instruction to compare two integers.
    fn build_int_comparison(
        &mut self,
        comparison: impl Into<ast::Cmp>,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        // Convert from ast::Cmp to inkwell::IntPredicate.
        let int_predicate = match comparison.into() {
            ast::Cmp::Eql => IntPredicate::EQ,
            ast::Cmp::Neq => IntPredicate::NE,
            ast::Cmp::Lt => IntPredicate::ULT,
            ast::Cmp::Gt => IntPredicate::UGT,
            ast::Cmp::Lte => IntPredicate::ULE,
            ast::Cmp::Gte => IntPredicate::UGE,
        };
        self.builder
            .build_int_compare(int_predicate, lhs, rhs, "intCmp")
    }

    /// Builds an LLVM conditional branch, using the given lambdas to generate
    /// instructions for each branch. Both branches converge afterwards.
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

        // Build the switch instruction (because condition_value might not be
        // 1-bit).
        self.builder.build_switch(
            condition_value,
            if_true_bb,
            &[(condition_value.get_type().const_zero(), if_false_bb)],
        );

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

    /// Adds a possible error that can be returned by the function and builds an
    /// LLVM instruction to return that error.
    fn build_error_point(&mut self, error: LangError) {
        let error_value = self.add_error_point(error) | (1 << 63);
        self.builder.build_return(Some(
            &self.llvm_return_type.const_int(error_value as u64, true),
        ));
    }

    /// Adds a possible error that can be returned by the function and returns
    /// the index of the new error point in self.error_points.
    fn add_error_point(&mut self, error: LangError) -> usize {
        self.error_points.push(error);
        self.error_points.len() - 1
    }

    /// Returns whether the current LLVM BasicBlock still needs a terminator
    /// instruction (i.e. whether it does NOT yet have one).
    fn needs_terminator(&self) -> bool {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
    }

    /// Allocates space for a variable.
    fn declare_var(&mut self, var_name: String, ty: Type) -> LangResult<()> {
        let llvm_type = self.get_llvm_type(ty).ok_or_else(|| {
            InternalError("Invalid variable type not caught by type checker".into()).without_span()
        })?;
        let ptr = self
            .builder
            .build_alloca(llvm_type, &format!("uservar__{}", var_name));
        self.vars.insert(var_name, Variable { ty, ptr });
        Ok(())
    }

    /// Initializes a variable to a reasonable default value (generally zero).
    /// Panics if the variable has not been declared.
    fn initialize_var(&mut self, var_name: &str) -> LangResult<()> {
        let Variable { ty, ptr } = self.vars[var_name];
        let initial_value = self.get_default_var_value(ty).ok_or_else(|| {
            InternalError("Invalid variable type not caught by type checker".into()).without_span()
        })?;
        self.builder.build_store(ptr, initial_value);
        self.vars.insert(var_name.to_owned(), Variable { ty, ptr });
        Ok(())
    }

    /// Appends an LLVM BasicBlock to the end of the current LLVM function and
    /// returns the new BasicBlock.
    fn append_basic_block(&self, name: &str) -> BasicBlock<'ctx> {
        self.ctx.append_basic_block(self.fn_value(), name)
    }

    /// Returns the LLVM type corresponding to the given type in NDCA.
    fn get_llvm_type(&self, ty: Type) -> Option<BasicTypeEnum<'ctx>> {
        match ty {
            Type::Int => Some(self.llvm_int_type.into()),
            Type::CellState => Some(self.llvm_cell_state_type.into()),
            Type::Vector(len) => Some(self.llvm_int_type.vec_type(len.into()).into()),
        }
    }

    /// Returns the default value for variables of the given type.
    fn get_default_var_value(&self, ty: Type) -> Option<BasicValueEnum<'ctx>> {
        match ty {
            Type::Int => Some(self.llvm_int_type.const_zero().into()),
            Type::CellState => Some(self.llvm_cell_state_type.const_zero().into()),
            Type::Vector(len) => Some(self.llvm_int_type.vec_type(len.into()).const_zero().into()),
        }
    }

    /// Returns the minimum value representable by signed integers of NDCA's
    /// signed integer type.
    fn get_min_int_value(&self) -> IntValue<'ctx> {
        self.llvm_int_type.const_int(1, false).const_shl(
            self.llvm_int_type
                .const_int(self.llvm_int_type.get_bit_width() as u64 - 1, false),
        )
    }
}
