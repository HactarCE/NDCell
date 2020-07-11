//! JIT compiler for the language.
//!
//! Most of the logic of compiling individual statements and functions is
//! present in methods on the AST nodes themselves, but this module manages all
//! the setup and teardown required.
//!
//! When we compile a function, we don't actually know how many arguments it
//! takes or what its return type is, so we can't encode this in Rust's type
//! system. Instead we create a struct containing all of the inputs to the
//! function and pass a pointer to that as the first argument, then create a
//! variable for the output of the function and pass a pointer to that as the
//! second argument. The actual return value of the function is just an integer
//! to indicate any error.
//!
//! The values in that first struct I've called "in/out" values, or `inouts`.
//! Actual function arguments only matter as inputs, but when debugging a
//! function we can pass variable values as "in/out" values, and read the value
//! after executing part of the function.

use itertools::Itertools;
use std::collections::HashMap;
use thread_local::ThreadLocal;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, IntType, StructType, VectorType};
use inkwell::values::{
    BasicValueEnum, FunctionValue, IntMathValue, IntValue, PhiValue, PointerValue, StructValue,
    VectorValue,
};
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};

mod config;
pub mod convert;
mod function;
mod iter;
pub mod types;
mod value;

pub use config::CompilerConfig;
pub use function::CompiledFunction;
pub use value::{PatternValue, Value};

use crate::errors::*;
use crate::types::{CellStateFilter, TypeDesc, CELL_STATE_BITS, INT_BITS};
use crate::{ConstValue, Type};

/// Name of the LLVM module.
const MODULE_NAME: &'static str = "ndca";

lazy_static! {
    /// Per-thread LLVM context.
    static ref CTX: ThreadLocal<Context> = ThreadLocal::new();
}
/// Returns this thread's LLVM context.
fn get_ctx() -> &'static Context {
    &CTX.get_or(Context::create)
}

/// Returns LLVM boolean (i1) IntValue of 0.
fn llvm_false() -> IntValue<'static> {
    get_ctx().bool_type().const_zero()
}
/// Returns LLVM boolean (i1) IntValue of 1.
fn llvm_true() -> IntValue<'static> {
    get_ctx().bool_type().const_all_ones()
}

/// Returns a constant sign-extended IntValue from the given integer.
pub fn const_int(value: i64) -> IntValue<'static> {
    types::int().const_int(value as u64, true)
}
/// Returns a constant zero-extended IntValue from the given integer.
pub fn const_uint(value: u64) -> IntValue<'static> {
    types::int().const_int(value, false)
}
/// Returns the minimum value representable by signed integers of NDCA's
/// signed integer type.
fn min_int_value() -> IntValue<'static> {
    const_uint(1).const_shl(const_uint(INT_BITS as u64 - 1))
}

/// Returns the name of an LLVM type used in names of intrinsics (e.g. "i32" for
/// a 32-bit integer, or "v3i64" for a vector of three 64-bit integers).
fn llvm_intrinsic_type_name(ty: BasicTypeEnum<'static>) -> String {
    match ty {
        BasicTypeEnum::ArrayType(_) => unimplemented!(),
        BasicTypeEnum::IntType(ty) => format!("i{}", ty.get_bit_width()),
        BasicTypeEnum::FloatType(_) => unimplemented!(),
        BasicTypeEnum::PointerType(_) => unimplemented!(),
        BasicTypeEnum::StructType(_) => unimplemented!(),
        BasicTypeEnum::VectorType(ty) => format!(
            "v{}{}",
            ty.get_size(),
            llvm_intrinsic_type_name(ty.get_element_type())
        ),
    }
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
    /// LLVM JIT execution engine.
    execution_engine: ExecutionEngine<'static>,
    /// Function currently being built.
    function: Option<FunctionInProgress>,
    /// Compiler configuration.
    config: CompilerConfig,

    /// Stack of jump targets for 'continue' statements.
    loop_entry_points: Vec<BasicBlock<'static>>,
    /// Stack of jump targets for 'break' statements.
    loop_exit_points: Vec<BasicBlock<'static>>,
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
            .map_err(|e| internal_error_value!("Error creating JIT execution engine: {:?}", e))?;
        Ok(Self {
            module,
            execution_engine,
            function: None,
            config: CompilerConfig::default(),

            loop_entry_points: vec![],
            loop_exit_points: vec![],
        })
    }
    /// Sets the configuration for this compiler.
    pub fn with_config(mut self, config: CompilerConfig) -> Self {
        self.config = config;
        self
    }

    /// Begins building a new LLVM function that can be called only from LLVM,
    /// initializing variables and positioning the instruction pointer
    /// accordingly.
    pub fn begin_intern_function(
        &mut self,
        name: &str,
        return_type: Type,
        param_names: &[String],
        var_types: &HashMap<String, Type>,
    ) -> LangResult<()> {
        // Determine the LLVM function type (signature).
        let llvm_return_type = types::get(&return_type)?;
        let llvm_arg_types = param_names
            .iter()
            .map(|name| &var_types[name])
            .map(types::get)
            .collect::<LangResult<Vec<_>>>()?;
        let fn_type = llvm_return_type.fn_type(&llvm_arg_types, false);
        // Construct the FunctionInProgress.
        self.function = Some(FunctionInProgress {
            llvm_fn: self.module.add_function(name, fn_type, None),
            builder: get_ctx().create_builder(),

            return_type,
            return_value_ptr: None,

            inout_struct_type: None,
            vars_by_name: HashMap::new(),
        });
        // Allocate and initialize variables and add them to the HashMap of all
        // variables.
        for (name, ty) in var_types {
            let var = self.alloca_and_init_var(name.clone(), ty.clone())?;
            self.function_mut().vars_by_name.insert(name.clone(), var);
        }

        Ok(())
    }
    /// Begins building a new LLVM function that can be called from Rust code,
    /// initializing variables and positioning the instruction builder
    /// accordingly.
    pub fn begin_extern_function(
        &mut self,
        name: &str,
        return_type: Type,
        param_names: &[String],
        var_types: &HashMap<String, Type>,
    ) -> LangResult<()> {
        // TODO: maybe sort variables (and arguments?) by alignment to reduce
        // unnecessary padding
        let mut inout_var_names: Vec<&String> = param_names.iter().collect();
        let mut alloca_var_names: Vec<&String> = vec![];
        for (name, _ty) in var_types {
            if !param_names.contains(name) {
                if self.config.enable_debug_mode {
                    inout_var_names.push(name);
                } else {
                    alloca_var_names.push(name);
                }
            }
        }

        // Determine the LLVM function type (signature).
        // The first parameter is a pointer to a struct containing all of the
        // inout parameters.
        let inout_var_types = inout_var_names
            .iter()
            .map(|&name| &var_types[name])
            .map(types::get)
            .collect::<LangResult<Vec<_>>>()?;
        let inout_struct_type = get_ctx().struct_type(&inout_var_types, false);
        let inout_struct_ptr_type = inout_struct_type
            .ptr_type(AddressSpace::Generic)
            .as_basic_type_enum();
        // The second parameter is a pointer to hold the return value.
        let return_ptr_type = types::get(&return_type)?
            .ptr_type(AddressSpace::Generic)
            .as_basic_type_enum();
        // The actual LLVM return value just signals whether there was an error.
        let fn_type = self
            .get_llvm_return_type()
            .fn_type(&[inout_struct_ptr_type, return_ptr_type], false);

        // Construct the FunctionInProgress.
        self.function = Some(FunctionInProgress {
            llvm_fn: self.module.add_function(name, fn_type, None),
            builder: get_ctx().create_builder(),

            return_type,
            return_value_ptr: None,

            inout_struct_type: Some(inout_struct_type),
            vars_by_name: HashMap::new(),
        });
        let entry_bb = self.append_basic_block("entry");
        self.builder().position_at_end(entry_bb);

        // Get pointers to the arguments.
        let shared_data_ptr = self
            .llvm_fn()
            .get_nth_param(0)
            .unwrap()
            .into_pointer_value();
        self.function_mut().return_value_ptr = Some(
            self.llvm_fn()
                .get_nth_param(1)
                .unwrap()
                .into_pointer_value(),
        );

        // Add inout variables to the HashMap of all variables.
        for (element_idx, &name) in inout_var_names.iter().enumerate() {
            // Get the byte offset of this field (so that Rust code can read and
            // modify this element).
            let byte_offset = self
                .execution_engine
                .get_target_data()
                .offset_of_element(&inout_struct_type, element_idx as u32)
                .unwrap() as usize;
            // Get a pointer to the corresponding field in the struct.
            let ptr = self
                .builder()
                .build_struct_gep(shared_data_ptr, element_idx as u32, name)
                .unwrap();
            // Insert this into the main HashMap of all variables.
            self.function_mut().vars_by_name.insert(
                name.clone(),
                Variable {
                    name: name.clone(),
                    ty: var_types[name].clone(),
                    is_arg: param_names.contains(name),
                    ptr,
                    inout_byte_offset: Some(byte_offset),
                },
            );
        }
        // Allocate and initialize alloca'd variables and add them to the
        // HashMap of all variables.
        for name in alloca_var_names {
            let ty = var_types[name].clone();
            let var = self.alloca_and_init_var(name.clone(), ty)?;
            self.function_mut().vars_by_name.insert(name.clone(), var);
        }

        Ok(())
    }
    /// Allocate space on the stack for the given variable and initialize it to
    /// a default value.
    fn alloca_and_init_var(&mut self, name: String, ty: Type) -> LangResult<Variable> {
        // Allocate space.
        let ptr = self.builder().build_alloca(types::get(&ty)?, &name);
        // Initialize to a default value.
        let default_value = self.get_default_var_value(&ty).into_basic_value()?;
        self.builder().build_store(ptr, default_value);
        Ok(Variable {
            name,
            ty,
            ptr,
            is_arg: false,
            inout_byte_offset: None,
        })
    }

    /// Returns the LLVM TargetData that this compiler uses when JIT compiling
    /// code.
    pub fn target_data(&self) -> &TargetData {
        self.execution_engine.get_target_data()
    }
    /// Finishes JIT compiling a function and returns a function pointer to
    /// executable assembly.
    pub unsafe fn finish_jit_function<F: UnsafeFunctionPointer>(
        &self,
    ) -> LangResult<JitFunction<'static, F>> {
        let llvm_fn = self.llvm_fn();
        let fn_name = llvm_fn
            .get_name()
            .to_str()
            .expect("Invalid UTF-8 in LLVM function name (seriously, wtf?)");
        self.execution_engine.get_function(fn_name).map_err(|_| {
            internal_error_value!("Failed to find JIT-compiled function {:?}", fn_name)
        })
    }

    /// Returns the function currently being built, panicking if there is none.
    fn function(&self) -> &FunctionInProgress {
        self.function.as_ref().expect("Tried to access function being built, but there is none; call Compiler::begin_function() first")
    }
    /// Returns a mutable reference to the function currently being built,
    /// panicking if there is none.
    fn function_mut(&mut self) -> &mut FunctionInProgress {
        self.function.as_mut().expect("Tried to access function being built, but there is none; call Compiler::begin_function() first")
    }
    /// Returns the LLVM function that is currently being built.
    pub fn llvm_fn(&self) -> FunctionValue<'static> {
        self.function().llvm_fn
    }
    /// Returns the Inkwell instruction builder.
    pub fn builder(&mut self) -> &Builder<'static> {
        &self.function().builder
    }
    /// Returns the Inkwell basic block currently being appended to, panicking
    /// if there is none.
    pub fn current_block(&mut self) -> BasicBlock<'static> {
        self.builder()
            .get_insert_block()
            .expect("Tried to access current insert block, but there is none")
    }
    /// Returns a HashMap of variables, indexed by name.
    pub fn vars(&self) -> &HashMap<String, Variable> {
        &self.function().vars_by_name
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
                    internal_error!("Requested multiple LLVM intrinsics with same name but different type signatures")
                }
            }
            None => Ok(self.module.add_function(name, fn_type, None)),
        }
    }

    /// Appends an LLVM BasicBlock to the end of the current LLVM function and
    /// returns the new BasicBlock.
    pub fn append_basic_block(&mut self, name: &str) -> BasicBlock<'static> {
        get_ctx().append_basic_block(self.llvm_fn(), name)
    }
    /// Appends an LLVM BasicBlock with a PHI node to the end of the current
    /// LLVM function and returns the new BasicBlock and PhiValue.
    ///
    /// The instruction builder is placed at the end of the basic block it was
    /// on before calling this function (so if it's already at the end of a
    /// basic block, it will stay there).
    pub fn append_basic_block_with_phi(
        &mut self,
        bb_name: &str,
        ty: impl BasicType<'static>,
        phi_name: &str,
    ) -> (BasicBlock<'static>, PhiValue<'static>) {
        let old_bb = self.current_block();
        let new_bb = self.append_basic_block(bb_name);
        self.builder().position_at_end(new_bb);
        let phi = self.builder().build_phi(ty, phi_name);
        self.builder().position_at_end(old_bb);
        (new_bb, phi)
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
    ///
    /// If the given closures return a basic value, then this method will return
    /// the value of a phi node that takes the result of whichever of the two
    /// closures executes.
    pub fn build_conditional<V: PhiMergeable>(
        &mut self,
        condition_value: IntValue<'static>,
        build_if_true: impl FnOnce(&mut Self) -> LangResult<V>,
        build_if_false: impl FnOnce(&mut Self) -> LangResult<V>,
    ) -> LangResult<V> {
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
        let value_if_true = build_if_true(self)?;
        let if_true_end_bb = self.current_block();
        if self.needs_terminator() {
            self.builder().build_unconditional_branch(merge_bb);
        }

        // Build the instructions to execute if false.
        self.builder().position_at_end(if_false_bb);
        let value_if_false = build_if_false(self)?;
        let if_false_end_bb = self.current_block();
        if self.needs_terminator() {
            self.builder().build_unconditional_branch(merge_bb);
        }

        // Merge values if the closures provided any.
        self.builder().position_at_end(merge_bb);
        let ret = PhiMergeable::merge(
            value_if_true,
            value_if_false,
            if_true_end_bb,
            if_false_end_bb,
            self,
        );
        Ok(ret)
    }
    /// Adds a new basic block intended to be unreachable and positions the
    /// builder at the end of it.
    pub fn build_new_unreachable_bb(&mut self) {
        let bb = self.append_basic_block("unreachableBlock");
        self.builder().position_at_end(bb);
    }

    /// Records the beginning of a loop (jump target for "continue" statements),
    /// executing instructions built by the given closure before every loop
    /// iteration EXCEPT the first. For each call to begin_loop(), end_loop()
    /// must be called once later.
    pub fn enter_loop(&mut self) {
        let loop_entry = self.append_basic_block("loopEntry");
        let loop_exit = self.append_basic_block("loopExit");

        self.loop_entry_points.push(loop_entry);
        self.loop_exit_points.push(loop_exit);

        if self.needs_terminator() {
            self.builder().build_unconditional_branch(loop_entry);
        }
        self.builder().position_at_end(loop_entry);
    }
    /// Records the end of a loop (jump target for "break" statements). For each
    /// call to begin_loop(), end_loop() must be called once later.
    pub fn exit_loop(&mut self) {
        let _loop_entry = self
            .loop_entry_points
            .pop()
            .expect("end_loop() called without corresponding begin_loop()");
        let loop_exit = self.loop_exit_points.pop().unwrap();

        if self.needs_terminator() {
            self.builder().build_unconditional_branch(loop_exit);
        }
        self.builder().position_at_end(loop_exit);
    }
    /// Builds a "continue" statement, returning error if there is no loop.
    pub fn build_jump_to_loop_entry(&mut self) -> Result<(), ()> {
        let loop_entry = *self.loop_entry_points.last().ok_or(())?;
        self.builder().build_unconditional_branch(loop_entry);
        Ok(())
    }
    /// Builds a "break" statement, returning error if there is no loog.
    pub fn build_jump_to_loop_exit(&mut self) -> Result<(), ()> {
        let loop_exit = *self.loop_exit_points.last().ok_or(())?;
        self.builder().build_unconditional_branch(loop_exit);
        Ok(())
    }

    /// Builds instructions to return a value.
    pub fn build_return_ok(&mut self, value: Value) -> LangResult<()> {
        let ptr = self.function().return_value_ptr.unwrap();
        self.builder().build_store(ptr, value.into_basic_value()?);
        let llvm_return_value = self.get_llvm_return_type().const_int(u64::MAX, true);
        self.builder().build_return(Some(&llvm_return_value));
        Ok(())
    }
    /// Builds instructions to return an error.
    pub fn build_return_err(&mut self, error_index: usize) {
        let llvm_return_value = self
            .get_llvm_return_type()
            .const_int(error_index as u64, false);
        self.builder().build_return(Some(&llvm_return_value));
    }
    /// Builds instructions to return an internal error.
    pub fn build_return_internal_err(&mut self) {
        // Error index 0 is reserved for internal errors.
        self.build_return_err(0);
    }

    /* MATH */

    /// Builds instructions to perform checked integer arithmetic using an LLVM
    /// intrinsic and returns an error if overflow occurs. Both operands must
    /// either be integers or vectors of the same length.
    pub fn build_checked_int_arithmetic<T: IntMathValue<'static>>(
        &mut self,
        lhs: T,
        rhs: T,
        name: &str,
        on_overflow: impl FnOnce(&mut Self) -> LangResult<()>,
    ) -> LangResult<BasicValueEnum<'static>> {
        let arg_type = lhs.as_basic_value_enum().get_type();

        // LLVM has intrinsics that perform some math with overflow checks.
        // First, get the name of the intrinsic we want to use (e.g.
        // "llvm.sadd.with.overflow.i64" for signed addition on i64).
        let intrinsic_name = format!(
            "llvm.{}.with.overflow.{}",
            name,
            llvm_intrinsic_type_name(arg_type)
        );
        // That intrinsic will return a struct containing the result and a
        // boolean indicated whether overflow occurred. But if we're doing this
        // on a vector then the overflow flag will be a whole vector of booleans
        // instead.
        let bool_type;
        if arg_type.is_vector_type() {
            bool_type = get_ctx()
                .bool_type()
                .vec_type(arg_type.into_vector_type().get_size())
                .into();
        } else {
            bool_type = get_ctx().bool_type().into();
        }
        let intrinsic_return_type = get_ctx().struct_type(&[arg_type, bool_type], false);
        let intrinsic_fn_type = intrinsic_return_type.fn_type(&[arg_type; 2], false);
        let intrinsic_fn = self.get_llvm_intrinisic(&intrinsic_name, intrinsic_fn_type)?;
        let intrinsic_args = &[lhs.as_basic_value_enum(), rhs.as_basic_value_enum()];

        // Build a call to an LLVM intrinsic to do the operation.
        let call_site_value =
            self.builder()
                .build_call(intrinsic_fn, intrinsic_args, "tmp_checked_result");

        // Get the actual return value of the function.
        let return_value = call_site_value
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_struct_value();

        // This return value is a struct with two elements: the result of the
        // operation, and a boolean value which is true if overflow occurred.
        // Extract each of those.
        let result_value = self
            .builder()
            .build_extract_value(return_value, 0, "tmp_result")
            .unwrap();
        let is_overflow_vec = self
            .builder()
            .build_extract_value(return_value, 1, "tmp_overflow")
            .unwrap();
        let is_overflow = self.build_reduce("or", is_overflow_vec)?;

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

    /// Builds an overflow and division-by-zero check for **integer** arguments
    /// to a division operation (but does not actually perform the division).
    pub fn build_int_div_check(
        &mut self,
        dividend: IntValue<'static>,
        divisor: IntValue<'static>,
        on_overflow: impl FnOnce(&mut Self) -> LangResult<()>,
        on_div_by_zero: impl FnOnce(&mut Self) -> LangResult<()>,
    ) -> LangResult<()> {
        // Generate the required constants.
        let zero = const_int(0);
        let min_value = min_int_value();
        let negative_one = const_int(-1);
        // Call the generic function.
        self.build_generic_div_check(
            dividend,
            divisor,
            zero,
            min_value,
            negative_one,
            on_overflow,
            on_div_by_zero,
        )
    }
    /// Builds an overflow and division-by-zero check for **vector** arguments
    /// to a division operation (but does not actually perform the division).
    pub fn build_vec_div_check(
        &mut self,
        dividend: VectorValue<'static>,
        divisor: VectorValue<'static>,
        on_overflow: impl FnOnce(&mut Self) -> LangResult<()>,
        on_div_by_zero: impl FnOnce(&mut Self) -> LangResult<()>,
    ) -> LangResult<()> {
        let len = dividend.get_type().get_size() as usize;
        // Generate the required constants.
        let zero = const_int(0);
        let min_value = min_int_value();
        let negative_one = const_int(-1);
        // Convert them to vectors of the proper length.
        let zero = self.build_vector_cast(Value::Int(zero), len)?;
        let min_value = self.build_vector_cast(Value::Int(min_value), len)?;
        let negative_one = self.build_vector_cast(Value::Int(negative_one), len)?;
        // Call the generic function.
        self.build_generic_div_check(
            dividend,
            divisor,
            zero,
            min_value,
            negative_one,
            on_overflow,
            on_div_by_zero,
        )
    }
    /// Builds an overflow and division-by-zero check for arguments to a
    /// division operation (but does not actually perform the division).
    fn build_generic_div_check<T: IntMathValue<'static> + Copy>(
        &mut self,
        dividend: T,
        divisor: T,
        zero: T,
        min_value: T,
        negative_one: T,
        on_overflow: impl FnOnce(&mut Self) -> LangResult<()>,
        on_div_by_zero: impl FnOnce(&mut Self) -> LangResult<()>,
    ) -> LangResult<()> {
        // If the divisor is zero, that's a DivideByZero error.
        let is_div_by_zero =
            self.builder()
                .build_int_compare(IntPredicate::EQ, divisor, zero, "isDivByZero");
        let is_div_by_zero = self.build_reduce("or", is_div_by_zero.as_basic_value_enum())?;
        // Branch based on whether the divisor is zero.
        self.build_conditional(
            is_div_by_zero,
            // The divisor is zero.
            on_div_by_zero,
            // The divisor is not zero.
            |c| {
                // If the dividend is the minimum possible value and the divisor
                // is -1, that's an IntegerOverflow error.
                let num_is_min_value = c.builder().build_int_compare(
                    IntPredicate::EQ,
                    dividend,
                    min_value,
                    "isMinValue",
                );
                let denom_is_neg_one = c.builder().build_int_compare(
                    IntPredicate::EQ,
                    divisor,
                    negative_one,
                    "isNegOne",
                );
                let is_overflow =
                    c.builder()
                        .build_and(num_is_min_value, denom_is_neg_one, "isOverflow");
                let is_overflow = c.build_reduce("or", is_overflow.as_basic_value_enum())?;

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

    /// Builds an overflow check for **integer** RHS argument to a bitshift
    /// operation (but does not actually perform the bitshift).
    pub fn build_bitshift_int_check(
        &mut self,
        shift_amt: IntValue<'static>,
        on_overflow: impl FnOnce(&mut Self) -> LangResult<()>,
    ) -> LangResult<()> {
        // Generate the required constants.
        let max_shift = const_uint(INT_BITS as u64);
        // Call the generic function.
        self.build_generic_bitshift_check(shift_amt, max_shift, on_overflow)
    }
    /// Builds an overflow check for **vector** RHS argument to a bitshift
    /// operation (but does not actually perform the bitshift).
    pub fn build_bitshift_vec_check(
        &mut self,
        shift_amt: VectorValue<'static>,
        on_overflow: impl FnOnce(&mut Self) -> LangResult<()>,
    ) -> LangResult<()> {
        let len = shift_amt.get_type().get_size() as usize;
        // Generate the required constants.
        let max_shift = const_uint(INT_BITS as u64);
        // Convert them to vectors of the proper length.
        let max_shift = self.build_vector_cast(Value::Int(max_shift), len)?;
        // Call the generic function.
        self.build_generic_bitshift_check(shift_amt, max_shift, on_overflow)
    }
    /// Builds an overflow check for RHS argument to a bitshift operation (but
    /// does not actually perform the bitshift).
    pub fn build_generic_bitshift_check<T: IntMathValue<'static>>(
        &mut self,
        shift_amt: T,
        max_shift: T,
        on_overflow: impl FnOnce(&mut Self) -> LangResult<()>,
    ) -> LangResult<()> {
        // If we are shifting a negative number of bits, or at least as many
        // bits as there are in the integer type, that's an IntegerOverflow
        // error.
        let is_overflow = self.builder().build_int_compare(
            IntPredicate::UGE, // Unsigned Greater-Than or Equal
            shift_amt,
            max_shift,
            "bitshiftOverflowCheck",
        );
        let is_overflow = self.build_reduce("or", is_overflow.as_basic_value_enum())?;
        // Branch based on whether the shift amount is out of range.
        self.build_conditional(is_overflow, on_overflow, |_| Ok(()))?;
        Ok(())
    }

    /// Selects between two basic values or shuffles two vectors.
    pub fn build_generic_select(
        &mut self,
        condition: BasicValueEnum<'static>,
        if_true: BasicValueEnum<'static>,
        if_false: BasicValueEnum<'static>,
        name: &str,
    ) -> BasicValueEnum<'static> {
        match condition {
            BasicValueEnum::IntValue(condition) => self
                .builder()
                .build_select(condition, if_true, if_false, name),
            BasicValueEnum::VectorValue(mask) => {
                // Instead of trying to use shufflevector, it's simpler to just
                // do bit manipulation: (mask & if_true) | (~mask & if_false).
                let vec_len = mask.get_type().get_size() as usize;
                let sext_mask_type = types::vec(vec_len);
                // Sign-extend so that the entire value is either 0 or 1.
                let sext_mask_lhs =
                    self.builder()
                        .build_int_s_extend(mask, sext_mask_type, "extendedMaskLhs");
                // Bitwise-invert the value for the other argument.
                let sext_mask_rhs = self.builder().build_xor(
                    sext_mask_lhs,
                    VectorType::const_vector(&vec![types::int().const_all_ones(); vec_len]),
                    "extendedMaskRhs",
                );
                // Compute mask & if_true.
                let masked_if_true = self.builder().build_and(
                    sext_mask_lhs,
                    if_true.into_vector_value(),
                    "maskedLhs",
                );
                // Compute mask & ~if_true.
                let masked_if_false = self.builder().build_and(
                    sext_mask_rhs,
                    if_false.into_vector_value(),
                    "maskedRhs",
                );
                // OR those to get the final result.
                self.builder()
                    .build_or(masked_if_true, masked_if_false, name)
                    .into()
            }
            _ => unimplemented!("Invalid type of 'generic select' operation"),
        }
    }

    /// Builds computation of the index of a cell state in a cell state filter.
    pub fn build_compute_cell_state_filter_idx(
        &mut self,
        state_count: usize,
        cell_state: IntValue<'static>,
    ) -> LangResult<CellStateFilterIndex> {
        if cell_state.get_type().get_bit_width() != CELL_STATE_BITS {
            internal_error!(
                "Argument to build_compute_cell_state_filter_idx() has wrong number of bits!"
            );
        }
        let element_type = types::cell_state_filter(state_count)
            .get_element_type()
            .into_int_type();
        let bits_per_element =
            types::cell_state().const_int(element_type.get_bit_width() as u64, false);
        // vec_idx = cell_state / INT_BITS
        let vec_idx = self.builder().build_int_unsigned_div(
            cell_state,
            bits_per_element,
            "cellStateFilter_vecIdx",
        );
        // bit_idx = cell_state % INT_BITS
        let bit_idx = self.builder().build_int_unsigned_rem(
            cell_state,
            bits_per_element,
            "cellStateFilter_bitIdx",
        );
        // Zero-extend that bit_idx from cell state to whatever the element type
        // is for this cell state filter.
        let bit_idx =
            self.builder()
                .build_int_z_extend(bit_idx, element_type, "cellStateFilter_bitIdx_zext");
        // bitmask = 1 << bit_idx
        let bitmask = self.builder().build_left_shift(
            element_type.const_int(1, false),
            bit_idx,
            "cellStateFilter_bitmask",
        );
        Ok(CellStateFilterIndex {
            vec_idx,
            bit_idx,
            bitmask,
        })
    }

    /* CONSTRUCT / SPLIT */

    /// Builds construction of a vector from the given components.
    pub fn build_construct_vector(
        &mut self,
        components: &[IntValue<'static>],
    ) -> VectorValue<'static> {
        let mut ret = types::vec(components.len()).get_undef();
        for (i, component) in components.iter().enumerate() {
            ret = self.builder().build_insert_element(
                ret,
                BasicValueEnum::from(*component),
                const_uint(i as u64),
                &format!("vec_build_{}", i),
            );
        }
        ret
    }
    /// Splits a vector of integers into its components.
    pub fn build_split_vector(
        &mut self,
        vector_value: VectorValue<'static>,
    ) -> Vec<IntValue<'static>> {
        (0..vector_value.get_type().get_size())
            .map(|i| {
                self.builder()
                    .build_extract_element(vector_value, const_uint(i as u64), "")
                    .into_int_value()
            })
            .collect()
    }

    /// Builds instructions to infer the step of a range (+1 if start <= end, or
    /// -1 if start > end) based on the given start and end.
    pub fn build_infer_range_step(
        &mut self,
        start: IntValue<'static>,
        end: IntValue<'static>,
    ) -> IntValue<'static> {
        let use_positive_step = self.builder().build_int_compare(
            IntPredicate::SLE, // Signed Less-Than or Equal
            start,
            end,
            "rangeStepTest",
        );
        self.builder()
            .build_select(use_positive_step, const_int(1), const_int(-1), "rangeStep")
            .into_int_value()
    }
    /// Builds construction of an integer range. If no step is given, then it is
    /// inferred at runtime based on the values of the start and end.
    pub fn build_construct_range(
        &mut self,
        start: IntValue<'static>,
        end: IntValue<'static>,
        step: Option<IntValue<'static>>,
    ) -> VectorValue<'static> {
        let step = step.unwrap_or_else(|| self.build_infer_range_step(start, end));
        self.build_construct_vector(&[start, end, step])
    }
    /// Splits an integer range into a start, end, and step.
    pub fn build_split_range(
        &mut self,
        range_value: VectorValue<'static>,
    ) -> (IntValue<'static>, IntValue<'static>, IntValue<'static>) {
        assert_eq!(
            3,
            range_value.get_type().get_size(),
            "Invalid vector length for range"
        );
        let values = self.build_split_vector(range_value);
        (values[0], values[1], values[2])
    }

    /// Builds construction of an N-dimensional rectangle.
    pub fn build_construct_rectangle(
        &mut self,
        start: VectorValue<'static>,
        end: VectorValue<'static>,
    ) -> StructValue<'static> {
        let ndim1 = start.get_type().get_size();
        let ndim2 = end.get_type().get_size();
        assert_eq!(
            ndim1, ndim2,
            "Cannot construct rectangle from differently-sized vectors",
        );
        let mut ret = types::rectangle(ndim1 as usize).get_undef();
        ret = self
            .builder()
            .build_insert_value(ret, start, 0, "rectTmp")
            .unwrap()
            .into_struct_value();
        ret = self
            .builder()
            .build_insert_value(ret, end, 1, "rect")
            .unwrap()
            .into_struct_value();
        ret
    }
    /// Splits a rectangle into a start vector and an end vector.
    pub fn build_split_rectangle(
        &mut self,
        rect_value: StructValue<'static>,
    ) -> (VectorValue<'static>, VectorValue<'static>) {
        (
            self.builder()
                .build_extract_value(rect_value, 0, "rectStart")
                .unwrap()
                .into_vector_value(),
            self.builder()
                .build_extract_value(rect_value, 1, "rectEnd")
                .unwrap()
                .into_vector_value(),
        )
    }

    /// Builds construction of a cell state filter that matches all cells less
    /// than (but not including) the given upper bound.
    pub fn build_construct_cell_state_filter_below(
        &mut self,
        state_count: usize,
        upper_bound: IntValue<'static>,
    ) -> LangResult<VectorValue<'static>> {
        // Compute index of the given cell state.
        let idx = self.build_compute_cell_state_filter_idx(state_count, upper_bound)?;

        // Start with a range of numbers, then compare them to to idx.vec_idx so
        // that each element is 1 if its index is less than idx.vec_idx or 0 if
        // its index is greater.
        let vec_len = CellStateFilter::vec_len_for_state_count(state_count);
        let vec_idx_zext = self
            .builder()
            .build_int_z_extend(idx.vec_idx, types::int(), "");
        let vec_idx_vector = self.build_vector_cast(Value::Int(vec_idx_zext), vec_len)?;
        let bools = self.builder().build_int_compare(
            IntPredicate::ULT,
            VectorType::const_vector(&(0..vec_len as u64).map(const_uint).collect_vec()),
            vec_idx_vector,
            "cellStateFilterBelow_bools",
        );
        // Now sign-extend those booleans to whatever the element size is. At
        // this point, the elements that need to be all ones or all zeros are
        // done.
        let zext_bools = self.builder().build_int_s_extend(
            bools,
            types::cell_state_filter(state_count),
            "cellStateFilterBelow_zextBools",
        );

        // Subtract one (add -1) from the bitmask to get the bitmask of all bits
        // below.
        let bitmask_below_n = self.builder().build_int_sub(
            idx.bitmask,
            idx.bitmask.get_type().const_int(1, false),
            "bitmaskBelowN",
        );
        let is_bitmask_zero = self.builder().build_int_compare(
            IntPredicate::EQ,
            bitmask_below_n,
            bitmask_below_n.get_type().const_zero(),
            "isBitmaskZero",
        );

        // Apply the bitmask if necessary.
        let ret = self
            .build_conditional(
                is_bitmask_zero,
                // If the bitmask is empty, then we're done.
                |_| Ok(BasicValueEnum::from(zext_bools)),
                // If the bitmask is NOT empty, then insert it into the vector.
                |c| {
                    Ok(c.builder()
                        .build_insert_element(
                            zext_bools,
                            bitmask_below_n,
                            idx.vec_idx,
                            "cellStateFilterConstructBelowN",
                        )
                        .into())
                },
            )?
            .into_vector_value();

        // Return the final result.
        Ok(ret)
    }

    /* CASTS */

    /// Builds a cast from any type to a boolean, represented using the normal
    /// integer type (either 0 or 1). Returns an InternalError if the given
    /// value cannot be converted to a boolean.
    pub fn build_convert_to_bool(&mut self, value: Value) -> LangResult<IntValue<'static>> {
        let is_truthy = match value {
            // Integers and cell states are truthy if nonzero. Vectors are
            // truthy if any component is nonzero.
            Value::Int(_) | Value::CellState(_) | Value::Vector(_) => {
                // Reduce using bitwise OR if it is a vector.
                let value = self.build_reduce("or", value.into_basic_value()?)?;
                // Compare to zero.
                let zero = value.get_type().const_zero();
                let is_nonzero =
                    self.builder()
                        .build_int_compare(IntPredicate::NE, value, zero, "isTrue");
                is_nonzero
            }
            // Patterns are truthy if any cell within their mask is nonzero.
            Value::Pattern(pattern) => {
                // If there is any nonzero cell, then return a truthy value.
                // This phi node should be true if any cell is nonzero and false
                // if all cells are zero.
                let (end_bb, end_phi) = self.append_basic_block_with_phi(
                    "endOfPatternTruthinessCheck",
                    get_ctx().bool_type(),
                    "isPatternTruthy",
                );

                // Check every single cell.
                self.build_pattern_iter(&pattern, |c, _pos, state| {
                    // Jump straight to the end if it's nonzero; otherwise keep
                    // searching for a nonzero cell.
                    let next_bb = c.append_basic_block("continuePatternTruthinessCheck");
                    c.builder().build_switch(
                        state,
                        // Jump straight to the end if nonzero.
                        end_bb,
                        // Keep checking cells if zero.
                        &[(state.get_type().const_zero(), next_bb)],
                    );
                    // If this cell is nonzero, then send true to the phi node.
                    end_phi.add_incoming(&[(&llvm_true(), c.current_block())]);
                    c.builder().position_at_end(next_bb);
                    Ok(())
                })?;
                // If all cells were zero, then send false to the phi node.
                self.builder().build_unconditional_branch(end_bb);
                end_phi.add_incoming(&[(&llvm_false(), self.current_block())]);
                self.builder().position_at_end(end_bb);

                // Finally, return the value of the phi node.
                end_phi.as_basic_value().into_int_value()
            }
            // Other types have no truthiness.
            Value::Void
            | Value::IntRange(_)
            | Value::Rectangle(_)
            | Value::CellStateFilter(_, _) => uncaught_type_error!(),
        };

        // Cast to integer.
        let ret = self
            .builder()
            .build_int_z_extend(is_truthy, types::int(), "toBool");
        Ok(ret)
    }
    /// Builds a cast of an integer to a vector (by using that integer value for
    /// each vector component) or from a vector of one length to another (by
    /// trimming excess values or by extending with zeros).
    pub fn build_vector_cast(
        &mut self,
        value: Value,
        len: usize,
    ) -> LangResult<VectorValue<'static>> {
        match value {
            Value::Int(i) => Ok(self.build_construct_vector(&vec![i; len])),
            Value::Vector(v) if v.get_type().get_size() as usize == len => Ok(v),
            Value::Vector(v) => {
                let original_len = v.get_type().get_size();
                let zeros = vec![const_uint(0); original_len as usize];
                let left = v;
                let right = VectorType::const_vector(&zeros);
                let mask_values = (0..original_len)
                    .chain(std::iter::repeat(original_len))
                    .map(|i| get_ctx().i32_type().const_int(i as u64, false))
                    .take(len)
                    .collect_vec();
                let mask = VectorType::const_vector(&mask_values);
                Ok(self
                    .builder()
                    .build_shuffle_vector(left, right, mask, "vectorCast"))
            }
            _ => internal_error!("Cannot convert {} to {}", value.ty(), TypeDesc::Vector),
        }
    }
    /// Builds a cast from an integer, vector, integer range, or rectangle of
    /// one dimensionality to another (by trimming excess values or by extending
    /// with the value `0..0`).
    pub fn build_rectangle_cast(
        &mut self,
        value: Value,
        ndim: usize,
    ) -> LangResult<StructValue<'static>> {
        let (old_start, old_end) = match value {
            Value::Int(i) => (Value::Int(i), Value::Int(i)),
            Value::Vector(v) => (Value::Vector(v), Value::Vector(v)),
            Value::IntRange(r) => {
                let (start, end, _step) = self.build_split_range(r);
                (Value::Int(start), Value::Int(end))
            }
            Value::Rectangle(r) => {
                let (start, end) = self.build_split_rectangle(r);
                (Value::Vector(start), Value::Vector(end))
            }
            _ => internal_error!("Cannot convert {} to {}", value.ty(), TypeDesc::Rectangle),
        };
        let new_start = self.build_vector_cast(old_start, ndim)?;
        let new_end = self.build_vector_cast(old_end, ndim)?;
        Ok(self.build_construct_rectangle(new_start, new_end))
    }
    /// Builds a cast from a cell state or cell state filter to a cell state
    /// filter.
    pub fn build_cell_state_filter_cast(
        &mut self,
        value: Value,
        state_count: usize,
    ) -> LangResult<VectorValue<'static>> {
        match value {
            Value::CellState(i) => {
                let ret = types::cell_state_filter(state_count).const_zero();
                let idx = self.build_compute_cell_state_filter_idx(state_count, i)?;
                Ok(self.builder().build_insert_element(
                    ret,
                    idx.bitmask,
                    idx.vec_idx,
                    "singleCellStateFilter",
                ))
            }
            Value::CellStateFilter(_, f) => Ok(f),
            _ => internal_error!(
                "Cannot convert {} to {}",
                value.ty(),
                TypeDesc::CellStateFilter
            ),
        }
    }
    /// Builds a reduction of a vector to an integer using the given operation.
    /// If the argument is already an integer, returns the integer.
    pub fn build_reduce(
        &mut self,
        op: &str,
        value: BasicValueEnum<'static>,
    ) -> LangResult<IntValue<'static>> {
        match value {
            BasicValueEnum::ArrayValue(_) => unimplemented!(),
            BasicValueEnum::FloatValue(_) => unimplemented!(),
            BasicValueEnum::IntValue(i) => Ok(i),
            BasicValueEnum::PointerValue(_) => unimplemented!(),
            BasicValueEnum::StructValue(_) => unimplemented!(),
            BasicValueEnum::VectorValue(v) => {
                let fn_type = v
                    .get_type()
                    .get_element_type()
                    .fn_type(&[value.get_type()], false);
                let reduce_fn = self.get_llvm_intrinisic(
                    &format!(
                        "llvm.experimental.vector.reduce.{}.{}",
                        op,
                        // Get name of input type.
                        llvm_intrinsic_type_name(value.get_type())
                    ),
                    fn_type,
                )?;
                Ok(self
                    .builder()
                    .build_call(reduce_fn, &[value], &format!("reduce_{}", op))
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value())
            }
        }
    }

    /* PATTERNS */

    /// Builds a read of a cell state pattern and executes instructions built by
    /// one of the given closures.
    pub fn build_get_pattern_cell_state(
        &mut self,
        pattern: &PatternValue,
        pos: VectorValue<'static>,
        on_ok: impl FnOnce(&mut Self, IntValue<'static>) -> LangResult<()>,
        on_err: impl FnOnce(&mut Self) -> LangResult<()>,
    ) -> LangResult<()> {
        let ndim = pattern.shape.ndim();
        let bounds = pattern.shape.bounds();
        let mask = pattern.shape.flat_mask();

        // Check the number of dimensions.
        if pos.get_type().get_size() != ndim as u32 {
            internal_error!("Dimension mismatch for pattern coordinates");
        }

        // Check that the position is in bounds.
        let is_out_of_bounds: IntValue<'static>;
        {
            // The position cannot be less than the minimum.
            let min = VectorType::const_vector(
                &bounds
                    .min()
                    .into_iter()
                    .map(|x| const_int(x as i64))
                    .collect_vec(),
            );
            let too_low_vec =
                self.builder()
                    .build_int_compare(IntPredicate::SLT, pos, min, "tooLow");
            let is_too_low = self.build_reduce("or", too_low_vec.into())?;
            // The position cannot be greater than the maximum.
            let max = VectorType::const_vector(
                &bounds
                    .max()
                    .into_iter()
                    .map(|x| const_int(x as i64))
                    .collect_vec(),
            );
            let too_high_vec =
                self.builder()
                    .build_int_compare(IntPredicate::SLT, pos, max, "tooHigh");
            let is_too_high = self.build_reduce("or", too_high_vec.into())?;
            // Combine with OR.
            is_out_of_bounds = self
                .builder()
                .build_or(is_too_low, is_too_high, "outOfBounds");
        }

        // Check that the position is inside the mask.
        let is_excluded_by_mask: IntValue<'static>;
        if pattern.shape.is_rect() {
            // This position can't be excluded by the mask if the mask includes
            // everything.
            is_excluded_by_mask = llvm_true();
        } else {
            // Make a flat array for the mask and store it on the stack. NOTE:
            // we invert each element here, so that FALSE indicates that a cell
            // is INCLUDED, while TRUE indicates a cell is EXCLUDED. This is to
            // avoid inverting at the end.
            let mask_array = get_ctx().bool_type().const_array(
                &mask
                    .iter()
                    .map(|&x| get_ctx().bool_type().const_int(!x as u64, false))
                    .collect_vec(),
            );
            let mask_array_ptr = self
                .builder()
                .build_alloca(mask_array.get_type(), "maskArray");
            self.builder().build_store(mask_array_ptr, mask_array);
            // Determine the strides of this flat array by producing cumulative products based on the sizes.
            let strides: Vec<u64> = bounds
                .size()
                .into_iter()
                .scan(1, |cumulative_product, len_along_axis| {
                    let this_stride = *cumulative_product;
                    *cumulative_product *= len_along_axis;
                    Some(this_stride as u64)
                })
                .collect();
            let strides_vector_value =
                VectorType::const_vector(&strides.iter().map(|&x| const_uint(x)).collect_vec());
            // Compute the index of the origin in this flat array.
            let origin_idx = const_int(
                strides
                    .iter()
                    .zip(bounds.min())
                    .map(|(&stride, lower_bound)| stride as isize * -lower_bound)
                    .product::<isize>() as i64,
            );
            // Now we can write some LLVM instructions. Multiply the strides by
            // the position to get the array index offset from the origin.
            let array_offset_from_origin_vec =
                self.builder()
                    .build_int_nuw_mul(pos, strides_vector_value, "maskOffsetFromOrigin");
            let array_offset_from_origin =
                self.build_reduce("add", array_offset_from_origin_vec.into())?;
            // Now add the index of the origin to get the final array index.
            let array_idx =
                self.builder()
                    .build_int_nsw_add(origin_idx, array_offset_from_origin, "maskIndex");
            // Finally, index into the array.
            let mask_element_ptr = unsafe {
                self.builder()
                    .build_gep(mask_array_ptr, &[array_idx], "maskElementPtr")
            };
            // See note above for why this tells whether the cells is EXCLUDED
            // rather than INCLUDED.
            is_excluded_by_mask = self
                .builder()
                .build_load(mask_element_ptr, "excludedByMask")
                .into_int_value();
        }

        let is_err =
            self.builder()
                .build_or(is_out_of_bounds, is_excluded_by_mask, "outOfBoundsOrMask");
        // Branch based on whether the position is in bounds.
        self.build_conditional(is_err, on_err, |c| {
            let cell_state_value = c.build_get_pattern_cell_state_unchecked(pattern, pos)?;
            on_ok(c, cell_state_value)
        })
    }
    /// Builds an unchecked read of a cell state patternand returns the value.
    /// Do not use this unless you are absolutely sure that the given position
    /// is within the cell state pattern.
    pub fn build_get_pattern_cell_state_unchecked(
        &mut self,
        pattern: &PatternValue,
        pos: VectorValue<'static>,
    ) -> LangResult<IntValue<'static>> {
        let cells_ptr = self
            .builder()
            .build_extract_value(pattern.value, 0, "cellArrayPtr")
            .unwrap()
            .into_pointer_value();
        let strides = self
            .builder()
            .build_extract_value(pattern.value, 1, "patternStrides")
            .unwrap()
            .into_vector_value();
        // Multiply strides by position to get pointer offset.
        let ptr_offset_vec = self
            .builder()
            .build_int_nsw_mul(pos, strides, "cellPtrOffset");
        let ptr_offset = self.build_reduce("add", ptr_offset_vec.into())?;
        // Use the result of that as a pointer offset.
        let cell_ptr = unsafe {
            self.builder()
                .build_gep(cells_ptr, &[ptr_offset], "cellPtr")
        };
        // And finally load from that pointer.
        Ok(self
            .builder()
            .build_load(cell_ptr, "cellState")
            .into_int_value())
    }

    /* MISCELLANY */

    /// Constructs a Value from a ConstValue.
    pub fn value_from_const(&self, const_value: ConstValue) -> Value {
        match const_value {
            ConstValue::Void => Value::Void,
            ConstValue::Int(i) => Value::Int(const_int(i)),
            ConstValue::CellState(i) => {
                Value::CellState(types::cell_state().const_int(i as u64, false))
            }
            ConstValue::Vector(values) => Value::Vector(VectorType::const_vector(
                &values.iter().map(|&i| const_int(i)).collect_vec(),
            )),
            ConstValue::IntRange { start, end, step } => {
                Value::IntRange(VectorType::const_vector(&[
                    const_int(start),
                    const_int(end),
                    const_int(step),
                ]))
            }
            ConstValue::Rectangle(start, end) => {
                assert_eq!(start.len(), end.len(), "Rect dimension mismatch");
                let ndim = start.len();
                let start = self
                    .value_from_const(ConstValue::Vector(start))
                    .into_basic_value()
                    .unwrap();
                let end = self
                    .value_from_const(ConstValue::Vector(end))
                    .into_basic_value()
                    .unwrap();
                Value::Rectangle(types::rectangle(ndim).const_named_struct(&[start, end]))
            }
            ConstValue::CellStateFilter(f) => Value::CellStateFilter(
                f.state_count(),
                VectorType::const_vector(
                    &f.as_bits()
                        .iter()
                        .copied()
                        .map(|x| {
                            types::cell_state_filter(f.state_count())
                                .get_element_type()
                                .into_int_type()
                                .const_int(x, false)
                        })
                        .collect_vec(),
                ),
            ),
            ConstValue::String(_) => panic!(NO_RUNTIME_REPRESENTATION),
        }
    }
    /// Returns the default value for variables of the given type, panicking if
    /// the given type has no LLVM representation.
    pub fn get_default_var_value(&self, ty: &Type) -> Value {
        self.value_from_const(ConstValue::default(ty))
    }

    /// Returns the LLVM type actually returned from this function (as opposed
    /// to the type semantically returned).
    pub fn get_llvm_return_type(&self) -> IntType<'static> {
        get_ctx().i32_type()
    }
}

/// Byte and bit indices indicating the location of a single bit in the vector
/// value of a cell state filter.
#[derive(Debug, Copy, Clone)]
pub struct CellStateFilterIndex {
    pub vec_idx: IntValue<'static>,
    pub bit_idx: IntValue<'static>,
    pub bitmask: IntValue<'static>,
}

/// A function in the process of being compiled to LLVM.
#[derive(Debug)]
struct FunctionInProgress {
    /// LLVM function currently being built.
    llvm_fn: FunctionValue<'static>,
    /// LLVM instruction builder.
    builder: Builder<'static>,

    /// Struct type used to input arguments, output a return value, and debug
    /// variables if debugging is enabled.
    inout_struct_type: Option<StructType<'static>>,

    /// Return type of this function.
    return_type: Type,
    /// Pointer to the place to put the return value.
    return_value_ptr: Option<PointerValue<'static>>,

    /// Variables, indexed by name.
    vars_by_name: HashMap<String, Variable>,
}

/// Compiled variable.
#[derive(Debug, PartialEq, Eq)]
pub struct Variable {
    /// Name of this variable.
    pub name: String,
    /// Type of this variable.
    pub ty: Type,
    /// Whether this variable is an argument.
    pub is_arg: bool,

    /// The LLVM pointer to where this variable is stored.
    pub ptr: PointerValue<'static>,
    /// The offset of this variable in `inout_bytes`, if it is stored there.
    pub inout_byte_offset: Option<usize>,
}

/// Trait implemented for () and BasicValueEnum.
pub trait PhiMergeable: Sized {
    fn merge(
        self,
        other: Self,
        self_bb: BasicBlock<'static>,
        other_bb: BasicBlock<'static>,
        compiler: &mut Compiler,
    ) -> Self;
}
impl PhiMergeable for () {
    fn merge(
        self,
        _other: (),
        _self_bb: BasicBlock<'static>,
        _other_bb: BasicBlock<'static>,
        _compiler: &mut Compiler,
    ) -> () {
        ()
    }
}
impl<'a> PhiMergeable for BasicValueEnum<'a> {
    fn merge(
        self,
        other: Self,
        self_bb: BasicBlock<'static>,
        other_bb: BasicBlock<'static>,
        compiler: &mut Compiler,
    ) -> Self {
        let phi = compiler
            .builder()
            .build_phi(self.get_type(), "mergeValuesEndIf");
        phi.add_incoming(&[(&self, self_bb), (&other, other_bb)]);
        phi.as_basic_value()
    }
}
