use std::collections::HashMap;
use thread_local::ThreadLocal;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, FunctionType, IntType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::{IntPredicate, OptimizationLevel};

mod function;
mod value;

pub use function::CompiledFunction;
pub use value::Value;

use super::errors::*;
use super::types::{CELL_STATE_BITS, INT_BITS};
use super::Type;
use LangErrorMsg::InternalError;

const MODULE_NAME: &'static str = "ndca";

type JitFunction = inkwell::execution_engine::JitFunction<'static, unsafe extern "C" fn() -> u64>;

lazy_static! {
    static ref CTX: ThreadLocal<Context> = ThreadLocal::new();
}
fn get_ctx() -> &'static Context {
    &CTX.get_or(Context::create)
}

/// JIT compiler providing a slightly higher-level interface to produce LLVM IR.
///
/// Inkwell (LLVM wrapper used here) only requires immutable references to most
/// of its things, but this doesn't make sense because most of the operations
/// are inherently mutable. To indicate that, many of these methods take mutable
/// references even though it isn't strictly required.
#[derive(Debug)]
pub struct Compiler {
    /// LLVM module.
    module: Module<'static>,
    /// LLVM instruction builder.
    builder_opt: Option<Builder<'static>>,
    /// LLVM function currently being built.
    fn_value_opt: Option<FunctionValue<'static>>,
    /// LLVM functions built by this compiler (which will all be deleted when this
    /// compiler is dropped).
    fn_values: Vec<FunctionValue<'static>>,
    /// LLVM JIT execution engine.
    execution_engine: ExecutionEngine<'static>,

    /// PointerValues for all variables, indexed by name.
    var_ptrs: HashMap<String, PointerValue<'static>>,
}
impl Drop for Compiler {
    fn drop(&mut self) {
        // TODO: is this necessary? awaiting response on Gitter.
        self.fn_value_opt = None;
        for fn_value in self.fn_values.drain(..) {
            unsafe { fn_value.delete() };
        }
    }
}
impl Compiler {
    /// Constructs a new compiler with a blank module and "main" function.
    ///
    /// After constructing a Compiler, call begin_function() before building any
    /// instructions.
    pub fn new() -> LangResult<Self> {
        let module = get_ctx().create_module(MODULE_NAME);
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .map_err(|e| {
                InternalError(format!("Error creating JIT execution engine: {:?}", e).into())
                    .without_span()
            })?;
        Ok(Self {
            module,
            builder_opt: None,
            fn_value_opt: None,
            fn_values: vec![],
            execution_engine,

            var_ptrs: HashMap::new(),
        })
    }
    /// Begins building a new LLVM function, initializing variables and
    /// positioning the instruction builder accordingly.
    pub fn begin_function<'a>(
        &'a mut self,
        name: &'a str,
        ty: FunctionType<'static>,
        variables: &HashMap<String, Type>,
    ) -> LangResult<()> {
        let fn_value = self.module.add_function(name, ty, None);
        self.fn_values.push(fn_value);
        self.fn_value_opt = Some(fn_value);
        self.builder_opt = Some(get_ctx().create_builder());
        let entry_bb = self.append_basic_block("entry");
        self.builder().position_at_end(entry_bb);
        // Allocate and initialize variables.
        self.var_ptrs = variables
            .iter()
            .map(|(name, &ty)| {
                let llvm_type = self.get_llvm_type(ty)?;
                // Allocate space.
                let ptr = self
                    .builder()
                    .build_alloca(llvm_type, &format!("uservar__{}", name));
                // Initialize to a default value.
                let default_value = self.get_default_var_value(ty).ok_or_else(|| {
                    InternalError("Uncaught invalid type for variable".into()).without_span()
                })?;
                self.builder().build_store(ptr, default_value);
                Ok((name.to_owned(), ptr))
            })
            .collect::<LangResult<_>>()?;
        Ok(())
    }

    pub unsafe fn get_jit_function(&self) -> LangResult<JitFunction> {
        let fn_value = self.fn_value();
        let fn_name = fn_value
            .get_name()
            .to_str()
            .expect("Invalid UTF-8 in LLVM function name (seriously, wtf?)");
        self.execution_engine.get_function(fn_name).map_err(|_| {
            InternalError(format!("Failed to find JIT-compiled function {:?}", fn_name).into())
                .without_span()
        })
    }

    /// Returns the LLVM return type of the function currently being built.
    pub fn return_type(&self) -> IntType<'static> {
        get_ctx().i64_type()
    }
    /// Returns the LLVM type used to represent an integer.
    pub fn int_type(&self) -> IntType<'static> {
        get_ctx().custom_width_int_type(INT_BITS)
    }
    /// Returns the LLVM type used to represent a cell state.
    pub fn cell_state_type(&self) -> IntType<'static> {
        get_ctx().custom_width_int_type(CELL_STATE_BITS)
    }

    /// Returns the Inkwell instruction builder.
    pub fn builder(&mut self) -> &Builder<'static> {
        self.builder_opt
            .as_ref()
            .expect("Compiler::begin_function() must be called before building LLVM instructions")
    }
    /// Returns a HashMap of variable pointers, indexed by name.
    pub fn vars(&self) -> &HashMap<String, PointerValue<'static>> {
        &self.var_ptrs
    }

    /// Returns an LLVM intrinsic given its name and function signature.
    pub fn get_llvm_intrinisic(
        &mut self,
        name: &str,
        fn_type: FunctionType<'static>,
    ) -> LangResult<FunctionValue<'static>> {
        match self.module.get_function(name) {
            Some(fn_value) => {
                if fn_value.get_type() == fn_type {
                    Ok(fn_value)
                } else {
                    Err(InternalError(
                        "Requested multiple LLVM intrinsics with same name but different type signatures"
                            .into(),
                    )
                    .without_span())
                }
            }
            None => Ok(self.module.add_function(name, fn_type, None)),
        }
    }

    /// Returns the LLVM function that is currently being built.
    pub fn fn_value(&self) -> FunctionValue<'static> {
        self.fn_value_opt
            .expect("Compiler::begin_function() must be called before retrieving function value")
    }
    /// Appends an LLVM BasicBlock to the end of the current LLVM function and
    /// returns the new BasicBlock.
    pub fn append_basic_block(&mut self, name: &str) -> BasicBlock<'static> {
        get_ctx().append_basic_block(self.fn_value(), name)
    }
    /// Returns whether the current LLVM BasicBlock still needs a terminator
    /// instruction (i.e. whether it does NOT yet have one).
    pub fn needs_terminator(&mut self) -> bool {
        self.builder()
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
    }

    /// Builds a conditional expression, using an IntValue of any width. Any
    /// nonzero value is truthy, and zero is falsey.
    pub fn build_conditional(
        &mut self,
        condition_value: IntValue<'static>,
        build_if_true: impl FnOnce(&mut Self) -> LangResult<()>,
        build_if_false: impl FnOnce(&mut Self) -> LangResult<()>,
    ) -> LangResult<()> {
        // Build the destination blocks.
        let if_true_bb = self.append_basic_block("ifTrue");
        let if_false_bb = self.append_basic_block("ifFalse");
        let merge_bb = self.append_basic_block("endIf");

        // Build the switch instruction (because condition_value might not be
        // 1-bit).
        self.builder().build_switch(
            condition_value,
            if_true_bb,
            &[(condition_value.get_type().const_zero(), if_false_bb)],
        );

        // Build the instructions to execute if true.
        self.builder().position_at_end(if_true_bb);
        build_if_true(self)?;
        if self.needs_terminator() {
            self.builder().build_unconditional_branch(merge_bb);
        }

        // Build the instructions to execute if false.
        self.builder().position_at_end(if_false_bb);
        build_if_false(self)?;
        if self.needs_terminator() {
            self.builder().build_unconditional_branch(merge_bb);
        }

        self.builder().position_at_end(merge_bb);
        Ok(())
    }

    /// Builds instructions to return a cell state.
    pub fn build_return_cell_state(&mut self, value: IntValue<'static>) {
        let return_type = self.return_type();
        let return_value = self.builder().build_int_cast(value, return_type, "ret");
        self.builder().build_return(Some(&return_value));
    }
    /// Builds instructions to return an error.
    pub fn build_return_error(&mut self, error_index: usize) {
        let return_value = error_index | (1 << 63);
        let return_value = self.return_type().const_int(return_value as u64, true);
        self.builder().build_return(Some(&return_value));
    }

    /// Builds instructions to perform checked integer arithmetic using an LLVM
    /// intrinsic and returns an error if overflow occurs.
    pub fn build_checked_int_arithmetic(
        &mut self,
        lhs: IntValue<'static>,
        rhs: IntValue<'static>,
        name: &str,
        on_overflow: impl FnOnce(&mut Self) -> LangResult<()>,
    ) -> LangResult<IntValue<'static>> {
        let intrinsic_name = format!(
            "llvm.{}.with.overflow.i{}",
            name,
            self.int_type().get_bit_width()
        );
        let intrinsic_return_type = get_ctx().struct_type(
            &[self.int_type().into(), get_ctx().bool_type().into()],
            false,
        );
        let intrinsic_fn_type = intrinsic_return_type.fn_type(&[self.int_type().into(); 2], false);
        let intrinsic_fn = self.get_llvm_intrinisic(&intrinsic_name, intrinsic_fn_type)?;
        let intrinsic_args = &[lhs.into(), rhs.into()];

        // Build a call to an LLVM intrinsic to do the operation.
        let call_site_value = self.builder().build_call(
            intrinsic_fn,
            intrinsic_args,
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
            .builder()
            .build_extract_value(return_value, 0, &format!("tmp_{}Result", intrinsic_name))
            .unwrap()
            .into_int_value();
        let is_overflow = self
            .builder()
            .build_extract_value(return_value, 1, &format!("tmp_{}Overflow", intrinsic_name))
            .unwrap()
            .into_int_value();

        // Branch based on whether there is overflow.
        self.build_conditional(
            is_overflow,
            // Return an error if there is overflow.
            on_overflow,
            // Otherwise proceed.
            |_| Ok(()),
        )?;

        Ok(result_value)
    }
    /// Builds an overflow and division-by-zero check for arguments to a
    /// division operation (but does not actually perform the division).
    pub fn build_div_check(
        &mut self,
        dividend: IntValue<'static>,
        divisor: IntValue<'static>,
        on_overflow: impl FnOnce(&mut Self) -> LangResult<()>,
        on_div_by_zero: impl FnOnce(&mut Self) -> LangResult<()>,
    ) -> LangResult<()> {
        // If the divisor is zero, that's a DivideByZero error.
        let zero = self.int_type().const_zero();
        let is_div_by_zero =
            self.builder()
                .build_int_compare(IntPredicate::EQ, divisor, zero, "isDivByZero");

        // Branch based on whether the divisor is zero.
        self.build_conditional(
            is_div_by_zero,
            // The divisor is zero.
            on_div_by_zero,
            // The divisor is not zero.
            |c| {
                // If the dividend is the minimum possible value and the divisor
                // is -1, that's an IntegerOverflow error.
                let min_value = c.get_min_int_value();
                let num_is_min_value = c.builder().build_int_compare(
                    IntPredicate::EQ,
                    dividend,
                    min_value,
                    "isMinValue",
                );
                let negative_one = c.int_type().const_int(-1i64 as u64, true);
                let denom_is_neg_one = c.builder().build_int_compare(
                    IntPredicate::EQ,
                    divisor,
                    negative_one,
                    "isNegOne",
                );
                let is_overflow =
                    c.builder()
                        .build_and(num_is_min_value, denom_is_neg_one, "isOverflow");

                // Branch based on whether there is overflow.
                c.build_conditional(
                    is_overflow,
                    // Overflow would occur.
                    on_overflow,
                    // Overflow would not occur; it is safe to perform the
                    // division.
                    |_| Ok(()),
                )
            },
        )
    }

    /// Returns the minimum value representable by signed integers of NDCA's
    /// signed integer type.
    fn get_min_int_value(&self) -> IntValue<'static> {
        self.int_type().const_int(1, false).const_shl(
            self.int_type()
                .const_int(self.int_type().get_bit_width() as u64 - 1, false),
        )
    }

    /// Returns the default value for variables of the given type.
    pub fn get_default_var_value(&self, ty: Type) -> Option<BasicValueEnum<'static>> {
        match ty {
            Type::Int => Some(self.int_type().const_zero().into()),
            Type::CellState => Some(self.cell_state_type().const_zero().into()),
            Type::Vector(len) => Some(self.int_type().vec_type(len.into()).const_zero().into()),
        }
    }

    /// Returns the LLVM type corresponding to the given type in NDCA.
    pub fn get_llvm_type(&self, ty: Type) -> LangResult<BasicTypeEnum<'static>> {
        match ty {
            Type::Int => Ok(self.int_type().into()),
            Type::CellState => Ok(self.cell_state_type().into()),
            Type::Vector(len) => Ok(self.int_type().vec_type(len.into()).into()),
            _ => Err(InternalError(
                "Attempt to get LLVM representation of type that has none".into(),
            )
            .without_span()),
        }
    }
}
