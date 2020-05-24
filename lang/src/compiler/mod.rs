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

use std::collections::HashMap;
use thread_local::ThreadLocal;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, IntType, StructType, VectorType};
use inkwell::values::{FunctionValue, IntValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};

mod function;
mod value;

pub use function::CompiledFunction;
pub use value::Value;

use super::errors::*;
use super::types::{CELL_STATE_BITS, INT_BITS};
use super::{ConstValue, Type};
use LangErrorMsg::InternalError;

/// Name of the LLVM module.
const MODULE_NAME: &'static str = "ndca";

/// Whether to enable debug mode. TODO: move this to CompilerConfig
const DEBUG_MODE: bool = false;

lazy_static! {
    /// Per-thread LLVM context.
    static ref CTX: ThreadLocal<Context> = ThreadLocal::new();
}
/// Returns this thread's LLVM context.
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
    /// LLVM JIT execution engine.
    execution_engine: ExecutionEngine<'static>,
    /// Function currently being built.
    function: Option<FunctionInProgress>,
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
            execution_engine,
            function: None,
        })
    }

    /// Begins building a new LLVM function that can be called only from LLVM,
    /// initializing variables and positioning the instruction pointer
    /// accordingly.
    pub fn begin_intern_function(
        &mut self,
        name: &str,
        return_type: Type,
        arg_names: &[String],
        var_types: &HashMap<String, Type>,
    ) -> LangResult<()> {
        // Determine the LLVM function type (signature).
        let llvm_return_type = self.get_llvm_type(return_type)?;
        let llvm_arg_types = arg_names
            .iter()
            .map(|name| self.get_llvm_type(var_types[name]))
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
        for (name, &ty) in var_types {
            let var = self.alloca_and_init_var(name.clone(), ty)?;
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
        arg_names: &[String],
        var_types: &HashMap<String, Type>,
    ) -> LangResult<()> {
        // TODO: maybe sort variables (and arguments?) by alignment to reduce
        // unnecessary padding
        let mut inout_var_names: Vec<&String> = arg_names.iter().collect();
        let mut alloca_var_names: Vec<&String> = vec![];
        for (name, _ty) in var_types {
            if !arg_names.contains(name) {
                if DEBUG_MODE {
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
            .map(|&name| var_types[name])
            .map(|ty| self.get_llvm_type(ty))
            .collect::<LangResult<Vec<_>>>()?;
        let inout_struct_type = get_ctx().struct_type(&inout_var_types, false);
        let inout_struct_ptr_type = inout_struct_type
            .ptr_type(AddressSpace::Generic)
            .as_basic_type_enum();
        // The second parameter is a pointer to hold the return value.
        let return_ptr_type = self
            .get_llvm_type(return_type)?
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
                    ty: var_types[name],
                    is_arg: arg_names.contains(name),
                    ptr,
                    inout_byte_offset: Some(byte_offset),
                },
            );
        }
        // Allocate and initialize alloca'd variables and add them to the
        // HashMap of all variables.
        for name in alloca_var_names {
            let ty = var_types[name];
            let var = self.alloca_and_init_var(name.clone(), ty)?;
            self.function_mut().vars_by_name.insert(name.clone(), var);
        }

        Ok(())
    }
    /// Allocate space on the stack for the given variable and initialize it to a default value.
    fn alloca_and_init_var(&mut self, name: String, ty: Type) -> LangResult<Variable> {
        let llvm_type = self.get_llvm_type(ty)?;
        // Allocate space.
        let ptr = self.builder().build_alloca(llvm_type, &name);
        // Initialize to a default value.
        let default_value = self.get_default_var_value(ty).unwrap().into_basic_value()?;
        self.builder().build_store(ptr, default_value);
        Ok(Variable {
            name,
            ty,
            ptr,
            is_arg: false,
            inout_byte_offset: None,
        })
    }

    /// Finishes JIT compiling a function and returns a function pointer to
    /// executable assembly.
    pub unsafe fn get_jit_function<F: UnsafeFunctionPointer>(
        &self,
    ) -> LangResult<JitFunction<'static, F>> {
        let llvm_fn = self.llvm_fn();
        let fn_name = llvm_fn
            .get_name()
            .to_str()
            .expect("Invalid UTF-8 in LLVM function name (seriously, wtf?)");
        self.execution_engine.get_function(fn_name).map_err(|_| {
            InternalError(format!("Failed to find JIT-compiled function {:?}", fn_name).into())
                .without_span()
        })
    }

    /// Returns the LLVM type used to represent an integer.
    pub fn int_type(&self) -> IntType<'static> {
        get_ctx().custom_width_int_type(INT_BITS)
    }
    /// Returns the LLVM type used to represent a cell state.
    pub fn cell_state_type(&self) -> IntType<'static> {
        get_ctx().custom_width_int_type(CELL_STATE_BITS)
    }

    /// Returns the function currently being built, panicking if there is none.
    fn function(&self) -> &FunctionInProgress {
        self.function.as_ref().expect("Tried to access function being built, but there is none; call Compiler::begin_function() first")
    }
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

    /// Appends an LLVM BasicBlock to the end of the current LLVM function and
    /// returns the new BasicBlock.
    pub fn append_basic_block(&mut self, name: &str) -> BasicBlock<'static> {
        get_ctx().append_basic_block(self.llvm_fn(), name)
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

    /// Constructs a Value from a ConstValue.
    pub fn value_from_const(&self, const_value: ConstValue) -> Value {
        match const_value {
            ConstValue::Int(i) => Value::Int(self.int_type().const_int(i as u64, true)),
            ConstValue::CellState(i) => {
                Value::CellState(self.cell_state_type().const_int(i as u64, false))
            }
            ConstValue::Vector(values) => Value::Vector(VectorType::const_vector(
                &values
                    .iter()
                    .map(|&i| {
                        self.value_from_const(ConstValue::Int(i))
                            .into_basic_value()
                            .expect("Failed to convert ConstValue to Value")
                    })
                    .collect::<Vec<_>>(),
            )),
        }
    }
    /// Returns the default value for variables of the given type.
    pub fn get_default_var_value(&self, ty: Type) -> Option<Value> {
        Some(self.value_from_const(ConstValue::default(ty)?))
    }

    /// Returns the LLVM type corresponding to the given type in NDCA.
    pub fn get_llvm_type(&self, ty: Type) -> LangResult<BasicTypeEnum<'static>> {
        match ty {
            Type::Int => Ok(self.int_type().into()),
            Type::CellState => Ok(self.cell_state_type().into()),
            Type::Vector(len) => Ok(self.int_type().vec_type(len as u32).into()),
            _ => Err(InternalError(
                "Attempt to get LLVM representation of type that has none".into(),
            )
            .without_span()),
        }
    }
    /// Returns the LLVM type actually returned from this function (as opposed
    /// to the type semantically returned).
    pub fn get_llvm_return_type(&self) -> IntType<'static> {
        get_ctx().i32_type()
    }
}

#[derive(Debug)]
/// A function in the process of being compiled to LLVM.
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
