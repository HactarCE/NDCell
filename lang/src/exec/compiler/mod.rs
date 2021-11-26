//! JIT compiler for the language.
//!
//! Most of the logic of compiling individual statements and expressions is
//! present in implementations of [`Statement`] and [`Expression`], but this
//! module manages all the setup and teardown required and provides low-level
//! utilities.
//!
//! # Passing arguments
//!
//! When we compile a function, we don't statically know how many arguments it
//! takes or what its return type is, so we can't encode this in Rust's type
//! system. Instead we create an array containing pointers to all of the inputs
//! and outputs of the function and pass a pointer to that array as the
//! argument. The actual return value of the function is just an integer to
//! indicate any error.
//!
//! # Thread safety
//!
//! We would like to share the same JIT-compiled function across multiple
//! threads but the [`llvm::JitFunction`] must be dropped from the same thread
//! that originally compiled it. So we spawn a new thread to the compile the
//! function and then wake that thread when the last [`CompiledFunction`] is
//! dropped so that it can then drop the underlying [`llvm::JitFunction`]. This
//! guarantees that the [`llvm::JitFunction`] (and therefore the
//! [`llvm::ExecutionEngine`]) will last as long as the [`CompiledFunction`].

use codemap::{Span, Spanned};
use inkwell::values::AnyValue;
use itertools::Itertools;
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::sync::{mpsc, Arc};

use ndcell_core::ndrect::IRect6D;

mod config;
mod function;
mod loops;
mod param;
mod var;

use super::builtins::expressions;
use crate::data::{
    CellArray, CellSet, CpVal, GetType, LangInt, LlvmCellArray, RtVal, SpannedCompileValueExt,
    SpannedRuntimeValueExt, SpannedVal, Type, Val, VectorSet, INT_BITS,
};
use crate::errors::{Error, Result};
use crate::exec::{Ctx, CtxTrait, Runtime};
use crate::llvm::{self, traits::*};
use crate::{ast, LangMode};
pub use config::CompilerConfig;
pub use function::CompiledFunction;
use loops::{Loop, VarInLoop};
pub use param::{Param, ParamType};
use var::{Var, VarError, VarResult};

#[derive(Debug)]
pub struct Compiler {
    /// Compiler configuration.
    config: CompilerConfig,

    /// LLVM module.
    module: llvm::Module,
    /// LLVM JIT execution engine.
    execution_engine: llvm::ExecutionEngine,
    /// Function being built (not immediately initialized).
    llvm_fn: Option<llvm::FunctionValue>,
    /// LLVM instruction builder.
    builder: llvm::Builder,

    /// Context.
    ctx: Ctx,
    /// Variable values.
    vars: HashMap<Arc<String>, Var>,
    /// Stack of overwritten variable values (used for reconciling variables
    /// after conditionals and loops).
    ///
    /// There is one stack entry for every conditional statement or loop that we
    /// are currently inside of.
    overwritten_vars_stack: Vec<HashMap<Arc<String>, Option<Var>>>,
    /// Stack of loops containing the statement currently being built. The
    /// innermost loop is at the top of the stack (end of the list).
    loop_stack: Vec<Loop>,

    /// Parameter types for the function being built.
    param_types: Vec<Spanned<ParamType>>,
    /// List of possible runtime errors.
    runtime_errors: Vec<Error>,

    cached_cell_array_masks: HashMap<Arc<VectorSet>, llvm::NdArrayValue>,
}

impl CtxTrait for Compiler {
    fn ctx(&mut self) -> &mut Ctx {
        &mut self.ctx
    }
    fn mode(&self) -> LangMode {
        self.ctx.mode
    }
}

impl Compiler {
    /// Name of the LLVM module.
    const MODULE_NAME: &'static str = "ndca";
    /// Name of the LLVM main function.
    const MAIN_FUNCTION_NAME: &'static str = "main";

    /// Compiles a program, given a `Runtime` that has already executed the
    /// initialization section.
    pub fn compile(
        ast: Arc<ast::Program>,
        mut runtime: Runtime,
        config: CompilerConfig,
    ) -> std::result::Result<CompiledFunction, Vec<Error>> {
        if runtime.has_errors() {
            return Err(runtime.ctx.errors);
        }

        // See module documentation for justification of this thread spawning
        // and channel nonsense.
        let (tx, rx) = mpsc::channel::<std::result::Result<CompiledFunction, Vec<Error>>>();

        std::thread::spawn(move || {
            // SAFETY: We take responsibility here for dropping the JIT function
            // on the same thread that created it after all references to the
            // `CompiledFunction` have been dropped.
            let result = runtime
                .ctx
                .compile_directive
                .ok_or_else(|| vec![Error::missing_directive(None, "@compile")])
                .and_then(|directive_id| {
                    let compile_directive = ast.get_node(directive_id);
                    unsafe { Self::compile_on_same_thread(compile_directive, runtime, config) }
                });

            match result {
                Err(e) => {
                    // If the receiver is dropped, we don't care.
                    let _ = tx.send(Err(e));
                }

                Ok((jit_fn, wrapper)) => {
                    // Get another reference to the counter and condvar.
                    let counter = Arc::clone(&wrapper.counter);
                    let condvar = Arc::clone(&wrapper.condvar);

                    // If the receiver is dropped, we don't care.
                    let _ = tx.send(Ok(wrapper));

                    // Wait until there are no other references to the JIT
                    // function.
                    let mut counter = counter.lock();
                    while *counter != 0 {
                        condvar.wait(&mut counter);
                    }

                    // Now drop the JIT function.
                    drop(jit_fn);
                }
            }
        });

        rx.recv()
            .unwrap_or_else(|e| Err(vec![internal_error_value!("compiler thread exited: {}", e)]))
    }

    /// Compiles a program, assuming initialization sections have already been
    /// executed using the interpreter.
    ///
    /// # Safety
    ///
    /// The [`llvm::JitFunction`] returned by this function must be dropped on
    /// the same thread it was created (i.e., the one that called this function)
    /// _after_ the last reference to the [`CompiledFunction`] returned by this
    /// function is dropped. Prefer [`Compiler::compile()`] instead.
    unsafe fn compile_on_same_thread(
        compile_directive: ast::Directive<'_>,
        mut runtime: Runtime,
        config: CompilerConfig,
    ) -> std::result::Result<(llvm::JitFunction, CompiledFunction), Vec<Error>> {
        let module = llvm::ctx().create_module(Self::MODULE_NAME);
        let execution_engine = module
            .create_jit_execution_engine(config.optimization_level)
            .map_err(|e| {
                vec![internal_error_value!(
                    "Error creating JIT execution engine: {:?}",
                    e,
                )]
            })?;

        // Make sure we have an `@compile` directive.
        let ast = compile_directive.ast;
        let (param_type_exprs, function_body) = match compile_directive.data() {
            ast::DirectiveData::Compile { param_types, body } => (param_types, body),
            _ => {
                runtime.report_error(internal_error_value!(
                    "cannot compile non-@compile directive",
                ));
                return Err(runtime.ctx.errors);
            }
        };

        // Determine argument types.
        let param_types = param_type_exprs
            .node
            .iter()
            .map(|&expr_id| match runtime.eval_expr(ast.get_node(expr_id)) {
                Ok(Spanned {
                    node: RtVal::Type(ty),
                    span,
                }) => match ty {
                    Type::Integer => Ok(ParamType::Integer),
                    Type::Cell => Ok(ParamType::Cell),
                    Type::Tag => todo!("tag param"),
                    Type::Vector(Some(len)) => Ok(ParamType::Vector(len)),
                    Type::CellArrayMut(Some(shape)) if !shape.is_empty() => {
                        Ok(ParamType::CellArray(shape))
                    }
                    Type::CellSet => todo!("cell set param"),
                    _ => Err(Error::cannot_compile(
                        span,
                        format!("parameter of type {}", ty),
                    )),
                }
                .map(|ok| Spanned { node: ok, span }),

                Ok(other) => Err(Error::expected(other.span, Type::Type)),

                Err(e) => Err(e),
            })
            .collect::<Result<Vec<Spanned<ParamType>>>>()
            .map_err(|e| {
                runtime.report_error(e);
                runtime.ctx.errors.clone()
            })?;

        if runtime.has_errors() {
            return Err(runtime.ctx.errors);
        }

        // Initialize variables in compiled code with the values from the
        // `@init` sections.
        let vars = runtime
            .vars
            .into_iter()
            .map(|(k, v)| {
                let initial_type = Some(v.ty());
                let v = Var {
                    value: Ok(Val::Rt(v)),
                    initial_type,
                    assign_spans: vec![],
                    loop_placeholder_index: None,
                };
                (k, v)
            })
            .collect();

        let mut this = Self {
            config,

            module,
            execution_engine,
            llvm_fn: None,
            builder: llvm::ctx().create_builder(),

            ctx: runtime.ctx,
            vars,
            overwritten_vars_stack: vec![],
            loop_stack: vec![],

            param_types,
            runtime_errors: vec![],

            cached_cell_array_masks: HashMap::new(),
        };

        // The LLVM function will take a single argument, a pointer to a struct
        // containing the real arguments and return values, if any.
        let llvm_param_types = this
            .param_types
            .iter()
            .map(|param_type| this.llvm_param_type(param_type))
            .collect_vec();
        let params_struct_type = llvm::ctx().struct_type(&llvm_param_types, false);
        let params_struct_ptr_type = params_struct_type.ptr_type(llvm::AddressSpace::Generic);

        // Declare the LLVM function. The actual return value just signals
        // whether there was an error.
        let fn_name = Self::MAIN_FUNCTION_NAME;
        let fn_type = llvm::error_index_type().fn_type(&[params_struct_ptr_type.into()], false);
        let fn_linkage = None;
        this.llvm_fn = Some(this.module.add_function(fn_name, fn_type, fn_linkage));

        match this.build_jit_function(ast.get_node(*function_body)) {
            Ok(ret) => {
                if this.ctx.errors.is_empty() {
                    Ok(ret)
                } else {
                    Err(this.ctx.errors)
                }
            }
            Err(e) => {
                this.report_error(e);
                Err(this.ctx.errors)
            }
        }
    }
    /// JIT-compiles a new function that can be called from Rust code.
    ///
    /// All parameters to the function are "in/out" parameters, so any return
    /// values must be included as parameters.
    fn build_jit_function(
        &mut self,
        function_body: ast::Stmt<'_>,
    ) -> Result<(llvm::JitFunction, CompiledFunction)> {
        // Build the LLVM IR for the function.
        let entry_bb = self.append_basic_block("entry");
        self.builder().position_at_end(entry_bb);
        match self.build_stmt(function_body)? {
            IsTerminated::Unterminated => {
                self.build_return_ok();
            }
            IsTerminated::Terminated => (),
        }

        // Don't compile the JIT function if there are any errors.
        if !self.ctx.errors.is_empty() {
            return Err(Error::AlreadyReported);
        }

        // Here we take responsibility for the inherent unsafety of compiling
        // JITted code and turning it into a raw function pointer.
        let jit_fn = unsafe { self.finish_jit_function() }?;
        // You won't find `raw_fn_ptr()` in the Inkwell docs because this
        // project uses a custom version of Inkwell with it.
        let jit_fn_ptr = unsafe { jit_fn.raw_fn_ptr() };
        let llvm_source = self.llvm_fn().print_to_string().to_string();

        // Prepare the parameter info.
        let params_struct_type = self.params_struct_type();
        let params = llvm::struct_offsets(&params_struct_type, self.target_data())
            .zip(&params_struct_type.get_field_types())
            .zip(&self.param_types)
            .map(|((offset, llvm_type), param_type)| Param {
                offset,
                size: self.target_data().get_store_size(llvm_type) as usize,

                ty: param_type.node.clone(),
            })
            .collect();

        // Construct the `CompiledFunction`.
        Ok((
            jit_fn,
            CompiledFunction::new(jit_fn_ptr, params, llvm_source, self.runtime_errors.clone()),
        ))
    }

    /// Finishes JIT compiling a function and returns a function pointer to
    /// executable assembly.
    unsafe fn finish_jit_function(&mut self) -> Result<llvm::JitFunction> {
        // Check that there are no errors in the LLVM code.
        if !self.llvm_fn().verify(true) {
            eprintln!(
                "Error encountered during function compilation; dumping LLVM function to stderr:",
            );
            self.llvm_fn().print_to_stderr();
            internal_error!("LLVM function is invalid");
        }

        match self.llvm_fn().get_name().to_str() {
            Ok(fn_name) => self.execution_engine.get_function(fn_name).map_err(|e| {
                internal_error_value!("Failed to find JIT-compiled function {:?}: {}", fn_name, e)
            }),
            Err(e) => internal_error!(
                "Invalid UTF-8 in LLVM function name (seriously, wtf?): {}",
                e,
            ),
        }
    }

    /// Returns the struct type used to hold parameters to the LLVM function.
    pub fn params_struct_type(&self) -> llvm::StructType {
        self.llvm_fn()
            .get_first_param()
            .unwrap()
            .get_type()
            .into_pointer_type()
            .get_element_type()
            .into_struct_type()
    }

    /// Returns the LLVM TargetData that this compiler uses when JIT compiling
    /// code.
    pub fn target_data(&self) -> &llvm::TargetData {
        self.execution_engine.get_target_data()
    }

    /// Returns the LLVM instruction builder.
    pub fn builder(&mut self) -> &mut llvm::Builder {
        &mut self.builder
    }

    /// Adds a possible runtime error and returns the error index.
    pub fn add_runtime_error(&mut self, e: Error) -> usize {
        debug_assert!(
            !matches!(e, Error::AlreadyReported),
            "runtime error can't be already reported",
        );
        self.runtime_errors.push(e);
        self.runtime_errors.len() - 1
    }
}

/*
 * TYPES AND VALUES
 */
impl Compiler {
    pub(crate) fn try_get_cp_val(&mut self, v: &Val) -> Result<Option<CpVal>> {
        Ok(match v {
            Val::Rt(v) => match v {
                RtVal::Integer(i) => Some(CpVal::Integer(llvm::const_int(*i))),
                RtVal::Cell(i) => Some(CpVal::Cell(llvm::const_cell(*i))),
                RtVal::Vector(v) => Some(CpVal::Vector(llvm::const_vector(v.iter().copied()))),
                RtVal::CellArray(a) => Some(CpVal::CellArray(self.get_const_immut_cell_array(a)?)),
                RtVal::CellSet(s) => Some(CpVal::CellSet(self.get_const_cell_set(s))),
                _ => None,
            },
            Val::Cp(v) => Some(v.clone()),
        })
    }
    // TODO: probably remove this
    pub(crate) fn get_cp_val(&mut self, v: &Spanned<Val>) -> Result<Spanned<CpVal>> {
        match self.try_get_cp_val(&v.node)? {
            Some(node) => Ok(Spanned { span: v.span, node }),
            None => Err(Error::cannot_compile(
                v.span,
                format!("non-constant use of type {}", v.ty()),
            )),
        }
    }

    /// Returns the LLVM type used for a JIT function parameter.
    pub fn llvm_param_type(&self, ty: &ParamType) -> llvm::BasicTypeEnum {
        match ty {
            ParamType::Integer => llvm::int_type().as_basic_type_enum(),
            ParamType::Cell => llvm::cell_type().as_basic_type_enum(),
            ParamType::Vector(len) => llvm::vector_type(*len).as_basic_type_enum(),
            ParamType::CellArray(_shape) => llvm::cell_type()
                .ptr_type(llvm::AddressSpace::Generic)
                .as_basic_type_enum(),
        }
    }

    /// Returns the LLVM type used for cell sets.
    pub fn cell_set_type(&self) -> llvm::VectorType {
        todo!("cell set type, depends on max cell state ID")
    }

    /// Returns the LLVM type used for an NDCA type.
    pub fn basic_type(&self, ty: &Type) -> Option<llvm::BasicTypeEnum> {
        Some(match ty {
            Type::Integer => llvm::int_type().into(),
            Type::Cell => llvm::cell_type().into(),
            Type::Tag => llvm::tag_type().into(),
            Type::Vector(Some(len)) => llvm::vector_type(*len).into(),
            Type::CellArray(Some(shape)) => llvm::cell_ndarray_type(shape.vec_len()).into(),
            Type::CellArrayMut(Some(shape)) => llvm::cell_ndarray_type(shape.vec_len()).into(),
            Type::CellSet => self.cell_set_type().into(),
            _ => return None,
        })
    }

    /// Builds instructions to construct a tag with a constant value.
    pub fn get_const_tag(&mut self, t: &str) -> llvm::VectorValue {
        let b = self.builder();
        todo!("build const tag")
    }
    /// Builds instructions to construct a cell set with a constant value.
    pub fn get_const_cell_set(&mut self, s: &CellSet) -> llvm::VectorValue {
        let b = self.builder();
        todo!("cell set type")
    }
    /// Returns an immutable cell array with constant contents.
    pub fn get_const_immut_cell_array(&mut self, array: &CellArray) -> Result<LlvmCellArray> {
        let array_contents = array
            .cells_array()
            .as_flat_slice()
            .iter()
            .map(|&c| llvm::const_cell(c))
            .collect_vec();
        let array_value = llvm::cell_type().const_array(&array_contents);
        let shape = Arc::clone(array.shape());
        let ndarray = match shape.bounds() {
            Some(bounds) => Some(self.get_const_ndarray(
                array.ndim(),
                bounds,
                array_value,
                "const_immut_cell_array",
            )?),
            None => None,
        };
        Ok(LlvmCellArray::new(shape, ndarray))
    }

    /// Returns the value inside if given an `Integer` value or subtype of one;
    /// otherwise returns a type error.
    pub fn as_integer(&mut self, v: &Spanned<CpVal>) -> Result<llvm::IntValue> {
        match &v.node {
            CpVal::Integer(x) => Ok(*x),
            _ => Err(Error::type_error(v.span, Type::Integer, &v.ty())),
        }
    }
    /// Returns the value inside if given a `Cell` value or subtype of one;
    /// otherwise returns a type error.
    pub fn as_cell(&mut self, v: &Spanned<CpVal>) -> Result<llvm::IntValue> {
        match &v.node {
            CpVal::Cell(x) => Ok(*x),
            _ => Err(Error::type_error(v.span, Type::Cell, &v.ty())),
        }
    }
    /// Returns the value inside if given a `Vector` value or subtype of one;
    /// otherwise returns a type error.
    pub fn as_vector(&mut self, v: &Spanned<CpVal>) -> Result<llvm::VectorValue> {
        match &v.node {
            CpVal::Vector(x) => Ok(*x),
            _ => Err(Error::type_error(v.span, Type::Vector(None), &v.ty())),
        }
    }
    /// Returns the value inside if given an `CellArray` value or subtype of
    /// one; otherwise returns a type error.
    pub fn as_cell_array(&mut self, v: &Spanned<CpVal>) -> Result<LlvmCellArray> {
        match &v.node {
            CpVal::CellArray(a) | CpVal::CellArrayMut(a) => Ok(a.clone()),
            _ => Err(Error::type_error(v.span, Type::CellArray(None), &v.ty())),
        }
    }
    /// Returns the value inside if given an `CellArrayMut` value or subtype of
    /// one; otherwise returns a type error.
    pub fn as_cell_array_mut(&mut self, v: &Spanned<CpVal>) -> Result<LlvmCellArray> {
        match &v.node {
            CpVal::CellArrayMut(a) => Ok(a.clone()),
            _ => Err(Error::type_error(v.span, Type::CellArrayMut(None), &v.ty())),
        }
    }
    /// Returns the value inside if given a `CellSet` value or subtype of one;
    /// otherwise returns a type error.
    pub fn as_cell_set(&mut self, v: &Spanned<CpVal>) -> Result<llvm::VectorValue> {
        match &v.node {
            CpVal::CellSet(x) => Ok(*x),
            _ => Err(Error::type_error(v.span, Type::CellSet, &v.ty())),
        }
    }

    /// Builds instructions to cast a value to a boolean and zero-extend that
    /// boolean to the width of an integer; returns a constant value if
    /// possible.
    pub fn build_convert_to_bool(&mut self, value: &Spanned<Val>) -> Result<BoolVal> {
        match value.clone().into() {
            SpannedVal::Rt(v) => Ok(BoolVal::Rt(v.to_bool()?)),
            SpannedVal::Cp(v) => {
                use llvm::IntPredicate::NE;

                let bool_result = match v.node {
                    CpVal::Integer(i) | CpVal::Cell(i) => {
                        self.build_any_cmp(NE, i, i.same_type_const_zero())?
                    }
                    CpVal::Vector(v) => self.build_any_cmp(NE, v, v.same_type_const_zero())?,
                    CpVal::CellArray(_) | CpVal::CellArrayMut(_) => {
                        todo!("convert cell array to bool")
                    }
                    CpVal::CellSet(_) => todo!("convert cell set to bool"),
                };

                Ok(BoolVal::Cp(self.builder().build_int_z_extend(
                    bool_result,
                    llvm::int_type(),
                    "zext_bool",
                )))
            }
        }
    }

    /// Builds instructions to cast a value to a vector.
    pub fn build_convert_to_vector(
        &mut self,
        value: &Spanned<Val>,
        len: usize,
    ) -> Result<llvm::VectorValue> {
        let cp_val = self.get_cp_val(value)?;
        self.build_convert_cp_val_to_vector(&cp_val, len)
    }
    /// Builds instructions to cast a value to a vector.
    pub fn build_convert_cp_val_to_vector(
        &mut self,
        value: &Spanned<CpVal>,
        len: usize,
    ) -> Result<llvm::VectorValue> {
        match value.node {
            CpVal::Integer(i) => Ok(self.build_construct_vector(&vec![i; len])),
            CpVal::Vector(v) => Ok(self.build_convert_vector_length(v, len)),
            _ => Err(Error::type_error(
                value.span,
                "type that can be converted to a vector",
                &value.node.ty(),
            )),
        }
    }
    /// Builds instructions to cast a vector to a different length.
    pub fn build_convert_vector_length(
        &mut self,
        v: llvm::VectorValue,
        len: usize,
    ) -> llvm::VectorValue {
        let v_len = v.get_type().get_size() as usize;
        if v_len == len {
            return v;
        }
        let shuffle_mask = (0..len).map(|i| std::cmp::min(i, v_len));
        self.builder().build_shuffle_vector(
            v,
            v.same_type_const_zero(),
            llvm::const_shuffle_vector(shuffle_mask),
            "resized_vector",
        )
    }

    /// Coerce two values to vectors of the same length.
    pub fn build_coerce_vectors_together(
        &mut self,
        v1: &Spanned<CpVal>,
        v2: &Spanned<CpVal>,
        vec_len_merge: impl FnMut(usize, usize) -> usize,
    ) -> Option<(llvm::VectorValue, llvm::VectorValue)> {
        let len = crate::utils::map_and_merge_options(
            v1.as_vector().ok(),
            v2.as_vector().ok(),
            |v| v.get_type().get_size() as usize,
            vec_len_merge,
        )?;
        // Resize the vectors to the same length.
        Some((
            self.build_convert_cp_val_to_vector(v1, len).ok()?,
            self.build_convert_cp_val_to_vector(v2, len).ok()?,
        ))
    }

    /// Builds instructions to construct a vector from integer components.
    pub fn build_construct_vector(&mut self, components: &[llvm::IntValue]) -> llvm::VectorValue {
        let mut ret = llvm::vector_type(components.len()).get_undef();
        for (i, &component) in components.iter().enumerate() {
            ret = self.builder().build_insert_element(
                ret,
                component,
                llvm::const_int(i as LangInt),
                &format!("vec_build_{}", i),
            );
        }
        ret
    }
    /// Builds instructions to split a vector of integers into its components.
    pub fn build_split_vector(&mut self, vector_value: llvm::VectorValue) -> Vec<llvm::IntValue> {
        (0..vector_value.get_type().get_size())
            .map(|i| {
                self.builder()
                    .build_extract_element(vector_value, llvm::const_int(i as LangInt), "")
                    .into_int_value()
            })
            .collect()
    }

    /// Adds a constant array to the module and returns a pointer to the first
    /// element.
    pub fn add_const_array(
        &mut self,
        array_value: llvm::ArrayValue,
        name: &str,
    ) -> llvm::PointerValue {
        // Store the array as a global constant.
        let array_global = self.module.add_global(
            array_value.get_type(),
            Some(llvm::AddressSpace::Generic),
            name,
        );
        array_global.set_initializer(&array_value);
        // Mark this global as a constant; we don't ever intend to modify it.
        array_global.set_constant(true);
        // The address of the constant doesn't matter; please do merge it with
        // other identical values!
        array_global.set_unnamed_addr(true);

        // Get a pointer to the array.
        let ty = array_value.get_type().get_element_type();
        array_global
            .as_pointer_value()
            .const_cast(ty.ptr_type(llvm::AddressSpace::Generic))
    }
    /// Returns an N-dimensional array with constant contents.
    pub fn get_const_ndarray(
        &mut self,
        ndim: usize,
        bounds: IRect6D,
        array_value: llvm::ArrayValue,
        name: &str,
    ) -> Result<llvm::NdArrayValue> {
        let base_ptr = self.add_const_array(array_value, &format!("{}_array", name));
        let strides = llvm::const_vector(crate::utils::ndarray_strides(ndim, bounds));
        self.get_const_ndarray_from_base_ptr(bounds, base_ptr, strides)
    }
    /// Builds instructions to construct an N-dimensional array from a pointer
    /// to the **base** of the array.
    pub fn build_construct_ndarray_from_base_ptr(
        &mut self,
        bounds: IRect6D,
        base_ptr: llvm::PointerValue,
        strides: llvm::VectorValue,
        name: &str,
    ) -> Result<llvm::NdArrayValue> {
        let ndim = strides.get_type().get_size() as usize;
        let array_from_base = self.build_construct_ndarray_from_origin_ptr(
            bounds,
            base_ptr,
            strides,
            &format!("{}_from_base", name),
        );

        let negative_base = llvm::const_vector((0..ndim).map(|i| -bounds.min().0[i] as LangInt));
        let origin_ptr = self.build_ndarray_gep_unchecked(
            array_from_base,
            negative_base,
            &format!("{}_origin_ptr", name),
        )?;
        Ok(self.build_construct_ndarray_from_origin_ptr(
            bounds,
            origin_ptr,
            strides,
            &format!("{}_from_origin", name),
        ))
    }
    /// Returns an N-dimensional array from a constant pointer to the **base**
    /// of the array.
    pub fn get_const_ndarray_from_base_ptr(
        &mut self,
        bounds: IRect6D,
        base_ptr: llvm::PointerValue,
        strides: llvm::VectorValue,
    ) -> Result<llvm::NdArrayValue> {
        let ndim = strides.get_type().get_size() as usize;
        let array_from_base = self.get_const_ndarray_from_origin_ptr(bounds, base_ptr, strides);

        let negative_base = llvm::const_vector((0..ndim).map(|i| -bounds.min().0[i] as LangInt));
        let origin_ptr = self.const_ndarray_gep_unchecked(array_from_base, negative_base)?;
        Ok(self.get_const_ndarray_from_origin_ptr(bounds, origin_ptr, strides))
    }
    /// Builds instructions to construct an N-dimensional array from a pointer
    /// to the origin in the array and strides.
    pub fn build_construct_ndarray_from_origin_ptr(
        &mut self,
        bounds: IRect6D,
        origin_ptr: llvm::PointerValue,
        strides: llvm::VectorValue,
        name: &str,
    ) -> llvm::NdArrayValue {
        let b = self.builder();
        let ndim = strides.get_type().get_size() as usize;
        let element_type = origin_ptr.get_type().get_element_type().into_int_type();
        let mut struct_value = llvm::ndarray_type(ndim, element_type).get_undef();
        struct_value = b
            .build_insert_value(struct_value, origin_ptr, 0, "")
            .unwrap()
            .into_struct_value();
        struct_value = b
            .build_insert_value(struct_value, strides, 1, name)
            .unwrap()
            .into_struct_value();
        llvm::NdArrayValue {
            bounds,
            struct_value,
        }
    }
    /// Returns an N-dimensional array from a constant pointer to the origin in
    /// the array and constant strides.
    pub fn get_const_ndarray_from_origin_ptr(
        &mut self,
        bounds: IRect6D,
        origin_ptr: llvm::PointerValue,
        strides: llvm::VectorValue,
    ) -> llvm::NdArrayValue {
        let ndim = strides.get_type().get_size() as usize;
        let element_type = origin_ptr.get_type().get_element_type().into_int_type();

        let struct_value = llvm::ndarray_type(ndim, element_type).const_named_struct(&[
            origin_ptr.as_basic_value_enum(),
            strides.as_basic_value_enum(),
        ]);
        llvm::NdArrayValue {
            bounds,
            struct_value,
        }
    }
    /// Builds instructions to split an N-dimensional array into its origin
    /// pointer and strides.
    pub fn build_split_ndarray(
        &mut self,
        array: llvm::NdArrayValue,
    ) -> (llvm::PointerValue, llvm::VectorValue) {
        let b = self.builder();
        let origin_ptr = b
            .build_extract_value(array.struct_value, 0, "origin_ptr")
            .unwrap()
            .into_pointer_value();
        let strides = b
            .build_extract_value(array.struct_value, 1, "strides")
            .unwrap()
            .into_vector_value();
        (origin_ptr, strides)
    }

    /// Builds instructions to allocate a multidimensional array on the stack.
    pub fn build_alloca_ndarray(
        &mut self,
        ndim: usize,
        bounds: IRect6D,
        int_type: llvm::IntType,
        name: &str,
    ) -> Result<llvm::NdArrayValue> {
        let buffer_len = bounds.count();
        let array_base_ptr = self.builder().build_array_alloca(
            int_type,
            llvm::const_int(buffer_len as LangInt),
            "cell_array_buffer",
        );
        let strides = llvm::const_vector(crate::utils::ndarray_strides(ndim, bounds));
        self.build_construct_ndarray_from_base_ptr(bounds, array_base_ptr, strides, name)
    }
    /// Builds instructions to allocate a mutable cell array on the stack.
    pub fn build_alloca_cell_array(
        &mut self,
        shape: Arc<VectorSet>,
        name: &str,
    ) -> Result<LlvmCellArray> {
        let ndim = shape.vec_len();
        let cells = match shape.bounds() {
            Some(bounds) => {
                Some(self.build_alloca_ndarray(ndim, bounds, llvm::cell_type(), name)?)
            }
            None => None,
        };
        Ok(LlvmCellArray::new(shape, cells))
    }

    /// Builds instructions to offset a cell array by a fixed delta vector.
    pub fn build_offset_cell_array(
        &mut self,
        error_span: Span,
        array: LlvmCellArray,
        delta: &[LangInt],
        name: &str,
    ) -> Result<LlvmCellArray> {
        if let Some(cells_ndarray) = array.cells() {
            let new_shape = Arc::new(array.shape().offset(error_span, delta)?);
            let new_bounds = new_shape.bounds().unwrap();

            let negative_delta = llvm::const_vector(delta.iter().map(|&i| -i));
            let new_origin_ptr = self.build_ndarray_gep_unchecked(
                cells_ndarray,
                negative_delta,
                &format!("cell_array_origin_with_offset_{:?}", delta),
            )?;
            let (_old_origin_ptr, strides) = self.build_split_ndarray(cells_ndarray);
            let new_cells = Some(self.build_construct_ndarray_from_origin_ptr(
                new_bounds,
                new_origin_ptr,
                strides,
                name,
            ));

            Ok(LlvmCellArray::new(new_shape, new_cells))
        } else {
            Ok(array) // empty
        }
    }

    /// Builds a GEP into a cell array, including a check that the position is
    /// within bounds and within the mask.
    pub fn build_cell_array_gep(
        &mut self,
        error_span: Span,
        array: &LlvmCellArray,
        pos: llvm::VectorValue,
    ) -> Result<llvm::PointerValue> {
        if let Some(mask) = self.get_cell_array_mask_constant(array.shape())? {
            let mask_cell_ptr = self.build_ndarray_gep(error_span, mask, pos, "mask_cell_ptr")?;
            let is_pos_in_mask = self
                .builder()
                .build_load(mask_cell_ptr, "is_pos_in_mask")
                .into_int_value();
            let error_index =
                self.add_runtime_error(Error::position_excluded_by_array_mask(error_span));
            self.build_return_err_unless(is_pos_in_mask, error_index)?;
        }

        if let Some(cells) = array.cells() {
            self.build_ndarray_gep(error_span, cells, pos, "cell_ptr")
        } else {
            // Don't generate a compile-time error here because it's possible
            // that this code will never execute.

            // When the cell array has no cells, all positions are out of
            // bounds!
            let error_index = self.add_runtime_error(Error::position_out_of_bounds(error_span));
            self.build_return_err(error_index);
            self.append_and_build_at_unreachable_basic_block();
            // This code won't execute, so just return a null pointer.
            Ok(llvm::cell_type()
                .ptr_type(llvm::AddressSpace::Generic)
                .const_null())
        }
    }

    /// Builds a GEP into an N-dimensional array including a check that the
    /// position is within bounds.
    pub fn build_ndarray_gep(
        &mut self,
        error_span: Span,
        ndarray: llvm::NdArrayValue,
        pos: llvm::VectorValue,
        name: &str,
    ) -> Result<llvm::PointerValue> {
        use llvm::IntPredicate::{SGT, SLT};

        // Pad all vectors to a common length.
        let len = std::cmp::max(pos.get_type().get_size() as usize, ndarray.ndim());
        let pos = self.build_convert_vector_length(pos, len);
        let lower_bound =
            self.build_convert_vector_length(llvm::const_vector(ndarray.min_vec()), len);
        let upper_bound =
            self.build_convert_vector_length(llvm::const_vector(ndarray.max_vec()), len);

        let is_past_lower_bound = self.build_any_cmp(SLT, pos, lower_bound)?;
        let is_past_upper_bound = self.build_any_cmp(SGT, pos, upper_bound)?;

        let is_out_of_bounds = self.builder().build_or(
            is_past_lower_bound,
            is_past_upper_bound,
            "is_pos_out_of_ndarray_bounds",
        );

        let error_index = self.add_runtime_error(Error::position_out_of_bounds(error_span));
        self.build_return_err_if(is_out_of_bounds, error_index)?;

        self.build_ndarray_gep_unchecked(ndarray, pos, name)
    }
    /// Builds a GEP into an N-dimensional array WITHOUT checking whether the
    /// position is out of bounds.
    pub fn build_ndarray_gep_unchecked(
        &mut self,
        ndarray: llvm::NdArrayValue,
        pos: llvm::VectorValue,
        name: &str,
    ) -> Result<llvm::PointerValue> {
        let (origin_ptr, strides) = self.build_split_ndarray(ndarray);
        let pos = self.build_convert_vector_length(pos, ndarray.ndim());
        let tmp = self.builder().build_int_nsw_mul(strides, pos, name);
        let ptr_offset = self.build_reduce("add", tmp.into())?;
        let b = self.builder();
        Ok(unsafe { b.build_gep(origin_ptr, &[ptr_offset], name) })
    }
    /// Computes a constant GEP into an N-dimensional array WITHOUT checking
    /// whether the position is out of bounds.
    ///
    /// # Panics
    ///
    /// This function may panic if `pos` and the array strides are not both
    /// constant vectors of the same length.
    pub fn const_ndarray_gep_unchecked(
        &mut self,
        ndarray: llvm::NdArrayValue,
        pos: llvm::VectorValue,
    ) -> Result<llvm::PointerValue> {
        let (origin_ptr, strides) = self.build_split_ndarray(ndarray);
        let ptr_offset = (0..ndarray.ndim() as u32)
            .map(|i| {
                let pos_elem = pos.get_element_as_constant(i).into_int_value();
                let strides_elem = strides.get_element_as_constant(i).into_int_value();
                pos_elem.const_mul(strides_elem)
            })
            .fold(llvm::const_int(0), llvm::IntValue::const_add);
        Ok(unsafe { origin_ptr.const_gep(&[ptr_offset]) })
    }

    /// Returns a constant cell array with the specified contents.
    fn get_cell_array_mask_constant(
        &mut self,
        shape: &Arc<VectorSet>,
    ) -> Result<Option<llvm::NdArrayValue>> {
        if !self.cached_cell_array_masks.contains_key(shape) {
            let ndim = shape.vec_len();
            let bounds = match shape.bounds() {
                Some(b) => b,
                None => return Ok(None),
            };
            let mask = match shape.mask() {
                Some(m) => m,
                None => return Ok(None),
            };
            let ndarray_contents = mask
                // Convert to `&[T]`.
                .as_flat_slice()
                .iter()
                .copied()
                // Convert `bool` to LLVM constant.
                .map(llvm::const_bool)
                .collect_vec();
            let array_value = llvm::bool_type().const_array(&ndarray_contents);
            let ndarray = self.get_const_ndarray(ndim, bounds, array_value, "ndarray_mask")?;
            self.cached_cell_array_masks
                .insert(Arc::clone(shape), ndarray);
        }
        Ok(Some(self.cached_cell_array_masks[shape]))
    }
}

/*
 * SIMPLE IMMUTABLE GETTERS
 */
impl Compiler {
    /// Returns the LLVM function currently being compiled.
    ///
    /// # Panics
    ///
    /// Panics if the LLVM function has not yet been initialized.
    pub fn llvm_fn(&self) -> llvm::FunctionValue {
        self.llvm_fn.unwrap()
    }
    /// Returns the types of the parameters to the compiled function.
    pub fn param_types(&self) -> &[Spanned<ParamType>] {
        &self.param_types
    }
}

/*
 * CONTROL FLOW
 */
impl Compiler {
    /// Returns the basic block that the instruction builder is currently
    /// positioned in, or panics if there is none.
    pub fn current_block(&self) -> llvm::BasicBlock {
        self.builder
            .get_insert_block()
            .expect("Tried to access current insert block, but there is none")
    }
    /// Appends a new basic block to the end of the current LLVM function and
    /// returns the new basic block without moving the instruction builder.
    pub fn append_basic_block(&mut self, name: &str) -> llvm::BasicBlock {
        llvm::ctx().append_basic_block(self.llvm_fn(), name)
    }
    /// Appends a new basic block intended to be unreachable and positions the
    /// builder at the end of it.
    pub fn append_and_build_at_unreachable_basic_block(&mut self) {
        let bb = self.append_basic_block("unreachableBlock");
        self.builder().position_at_end(bb);
    }

    /// Builds a conditional expression, using an IntValue of any width. Any
    /// nonzero value is truthy, and zero is falsey.
    ///
    /// This method will return the value of a phi node that merges the values
    /// resulting from the closures.
    pub fn build_conditional_with_value<V>(
        &mut self,
        condition_value: impl Into<BoolVal>,
        build_if_true: impl FnOnce(&mut Self) -> Result<V>,
        build_if_false: impl FnOnce(&mut Self) -> Result<V>,
    ) -> Result<V>
    where
        Self: BuildPhi<V>,
    {
        let mut phi_incoming = vec![];
        let mut phi_incoming_2 = vec![];

        let is_terminated = self.build_conditional(
            condition_value,
            |c| {
                phi_incoming.push((build_if_true(c)?, c.current_block()));
                Ok(IsTerminated::Unterminated)
            },
            |c| {
                phi_incoming_2.push((build_if_false(c)?, c.current_block()));
                Ok(IsTerminated::Unterminated)
            },
        )?;
        if is_terminated == IsTerminated::Terminated {
            internal_error!("conditional with value is terminated");
        }

        phi_incoming.extend(phi_incoming_2);
        self.build_and_populate_phi(&phi_incoming, "")
    }
    /// Builds a conditional statement, using an IntValue of any width. Any
    /// nonzero value is truthy, and zero is falsey.
    ///
    /// This method will return whether both branches are terminated.
    pub fn build_conditional(
        &mut self,
        condition_value: impl Into<BoolVal>,
        build_if_true: impl FnOnce(&mut Self) -> Result<IsTerminated>,
        build_if_false: impl FnOnce(&mut Self) -> Result<IsTerminated>,
    ) -> Result<IsTerminated> {
        // If the condition value is known at compile-time, then only compile
        // the branch that will execute.
        let condition_value = match condition_value.into() {
            BoolVal::Rt(true) => return build_if_true(self),
            BoolVal::Rt(false) => return build_if_false(self),
            BoolVal::Cp(v) => v,
        };

        // Create the destination blocks.
        let if_true_bb = self.append_basic_block("ifTrue");
        let if_false_bb = self.append_basic_block("ifFalse");

        // Build the switch instruction (because condition_value might not be
        // 1-bit).
        self.builder().build_switch(
            condition_value,
            if_true_bb,
            &[(condition_value.get_type().const_zero(), if_false_bb)],
        );

        // Build the instructions to execute if true.
        self.push_overwritten_vars();
        self.builder().position_at_end(if_true_bb);
        let if_true_end_bb = match build_if_true(self)? {
            IsTerminated::Unterminated => Some(self.current_block()),
            IsTerminated::Terminated => None,
        };
        let vars_from_if_true = self.pop_and_restore_overwritten_vars()?;

        // Build the instructions to execute if false.
        self.push_overwritten_vars();
        self.builder().position_at_end(if_false_bb);
        let if_false_end_bb = match build_if_false(self)? {
            IsTerminated::Unterminated => Some(self.current_block()),
            IsTerminated::Terminated => None,
        };
        let vars_from_if_false = self.pop_and_restore_overwritten_vars()?;

        if if_true_end_bb.is_some() || if_false_end_bb.is_some() {
            // Merge branches.
            let merge_bb = self.append_basic_block("endIf");
            let b = self.builder();
            if let Some(bb) = if_true_end_bb {
                b.position_at_end(bb);
                b.build_unconditional_branch(merge_bb);
            }
            if let Some(bb) = if_false_end_bb {
                b.position_at_end(bb);
                b.build_unconditional_branch(merge_bb);
            }

            // Merge variables.
            b.position_at_end(merge_bb);
            match (if_true_end_bb, if_false_end_bb) {
                (None, None) => unreachable!(),
                (Some(_if_true_end_bb), None) => {
                    self.overwrite_vars(vars_from_if_true);
                }
                (None, Some(_if_false_end_bb)) => {
                    self.overwrite_vars(vars_from_if_false);
                }
                (Some(if_true_end_bb), Some(if_false_end_bb)) => {
                    // Merge variable values.
                    let mut names = HashSet::new();
                    names.extend(vars_from_if_true.keys());
                    names.extend(vars_from_if_false.keys());

                    for name in names {
                        let old_var = self.vars.get(name);

                        let var_if_true = vars_from_if_true
                            .get(name)
                            .cloned()
                            // If there was never anything assigned to the variable,
                            // assume the old value.
                            .unwrap_or(old_var.cloned())
                            // If the variable never had a value to begin with,
                            // assume it is undefined.
                            .unwrap_or_default();

                        let var_if_false = vars_from_if_false
                            .get(name)
                            .cloned()
                            // If there was never anything assigned to the variable,
                            // assume the old value.
                            .unwrap_or(old_var.cloned())
                            // If the variable never had a value to begin with,
                            // assume it is undefined.
                            .unwrap_or_default();

                        let phi_incoming = [
                            (var_if_true, if_true_end_bb),
                            (var_if_false, if_false_end_bb),
                        ];
                        // TODO workaround for https://github.com/rust-lang/rust/issues/90841
                        let merged_var_value =
                            BuildPhi::<Var>::build_and_populate_phi(self, &phi_incoming, name)?;
                        self.set_var(name, Some(merged_var_value));
                    }
                }
            }

            self.builder().position_at_end(merge_bb);
            Ok(IsTerminated::Unterminated)
        } else {
            Ok(IsTerminated::Terminated)
        }
    }

    /// Builds a loop.
    ///
    /// `build_contents()` starts at the header and may contain jumps to either
    /// the prelatch or exit (via `continue` or `break` respectively).
    /// `build_prelatch()` starts at the prelatch and must NOT contain a jump to
    /// `continue` or `break`.
    pub fn build_loop(
        &mut self,
        vars_assigned: HashSet<Arc<String>>,
        build_contents: impl FnOnce(&mut Self) -> Result<IsTerminated>,
        build_prelatch: impl FnOnce(&mut Self) -> Result<IsTerminated>,
    ) -> Result<IsTerminated> {
        let loop_depth = self.loop_stack.len();

        let preheader = self.append_basic_block("preheader");
        let header = self.append_basic_block("header");
        let prelatch = self.append_basic_block("prelatch");
        let latch = self.append_basic_block("latch");
        let exit = self.append_basic_block("exit");

        self.builder().build_unconditional_branch(preheader);

        // Build preheader.
        self.builder().position_at_end(preheader);
        self.builder().build_unconditional_branch(header);

        // Build header and loop contents.
        self.builder().position_at_end(header);

        // Replace any variables modified within the loop with placeholder
        // values, which we will later build proper phi nodes for.
        let vars_modified: HashMap<Arc<String>, VarInLoop> = vars_assigned
            .iter()
            .filter_map(|name| {
                let pre_loop_var = self.get_var(name);

                // Assign a placeholder value to the variable if it can be
                // merged later with a phi.
                self.vars
                    .entry(Arc::clone(name))
                    .or_default()
                    .loop_placeholder_index = Some(loop_depth);
                let var = self.get_var(name);
                let header_phi = self.build_phi(&var, name);
                self.vars.get_mut(name)?.value = header_phi
                    .and_then(|phi| {
                        let ty = var.try_ty()?;
                        let cp_val = CpVal::from_basic_value(ty, phi.as_basic_value()).ok()?;
                        Some(Val::Cp(cp_val))
                    })
                    .ok_or(VarError::AlreadyReported); // not already reported, but we'll report it later in this function

                let var_in_loop = VarInLoop {
                    pre_loop_var,
                    header_phi,
                    first_use: None,

                    prelatch_phi_inputs: vec![],
                    exit_phi_inputs: vec![],
                };

                Some((Arc::clone(name), var_in_loop))
            })
            .collect();

        self.loop_stack.push(Loop {
            preheader,

            header,
            prelatch,

            latch,
            exit,

            vars_modified,

            has_break: false,
            has_continue: false,
        });

        match build_contents(self)? {
            IsTerminated::Unterminated => {
                self.build_loop_continue()?;
            }
            IsTerminated::Terminated => (),
        }

        // bröther may I have some `lööp`s
        let lööp = self
            .loop_stack
            .pop()
            .ok_or_else(|| internal_error_value!("no loop"))?;

        // Build prelatch.
        self.builder().position_at_end(prelatch);
        if lööp.has_continue {
            for (name, var_in_loop) in &lööp.vars_modified {
                let var_at_prelatch =
                    self.build_and_populate_phi(&var_in_loop.prelatch_phi_inputs, name)?;
                self.set_var(name, Some(var_at_prelatch))
            }
            match build_prelatch(self)? {
                IsTerminated::Unterminated => {
                    self.builder().build_unconditional_branch(latch);
                }
                IsTerminated::Terminated => (),
            }

            // Build latch.
            self.builder().position_at_end(latch);
            self.builder().build_unconditional_branch(header);
        }

        if !lööp.has_continue {
            // Delete latch if there is no `continue` statement.
            unsafe { prelatch.delete() }.expect("failed to delete prelatch");
            unsafe { latch.delete() }.expect("failed to delete prelatch");
        }

        if lööp.has_break {
            // Build exit.
            self.builder().position_at_end(exit);
        } else {
            // Delete exit if there is no `break` statement.
            unsafe { exit.delete() }.expect("failed to delete exit");
        }

        for (name, mut var_in_loop) in lööp.vars_modified {
            // Populate phi node at the beginning of the header.
            let var_at_preheader = var_in_loop.pre_loop_var.clone();
            let var_at_latch = self.get_var(&name);
            let mut var_at_header = if lööp.has_continue {
                let phi_incoming = [(var_at_preheader, preheader), (var_at_latch, latch)];
                self.populate_phi(var_in_loop.header_phi, &phi_incoming)
            } else {
                let phi_incoming = [(var_at_preheader, preheader)];
                self.populate_phi(var_in_loop.header_phi, &phi_incoming)
            }?;
            // If the variable was used inside the loop, make sure there's no
            // error when it's used.
            if let Some(first_use_span) = var_in_loop.first_use {
                // If this is the first use of the variable in a bigger loop as
                // well, this call to `get_value()` will record that.
                let var_value_at_first_use = var_at_header.get_value(first_use_span)?;
                let ty = var_value_at_first_use.ty();
                self.try_get_cp_val(&var_value_at_first_use)?
                    .ok_or_else(|| Error::unknown_variable_value(first_use_span, &ty))?;
            }

            if lööp.has_break {
                // Replace uses of placeholder variable value with variable
                // value at start of header.
                for &mut (ref mut var, _bb) in &mut var_in_loop.exit_phi_inputs {
                    if var.loop_placeholder_index == Some(loop_depth) {
                        *var = var_at_header.clone();
                    }
                }

                // Build phi node at exit.
                let mut var_at_exit =
                    self.build_and_populate_phi(&var_in_loop.exit_phi_inputs, &name)?;

                // Restore loop placeholder index.
                var_at_exit.loop_placeholder_index =
                    var_in_loop.pre_loop_var.loop_placeholder_index;

                self.set_var(&name, Some(var_at_exit));
            }
        }

        if lööp.has_break {
            Ok(IsTerminated::Unterminated)
        } else {
            Ok(IsTerminated::Terminated)
        }
    }

    /// Builds an unconditional jump to the end of the inside of the loop.
    pub fn build_loop_continue(&mut self) -> Result<IsTerminated> {
        let bb = self.current_block();
        let lööp = self
            .loop_stack
            .last_mut()
            .ok_or_else(|| internal_error_value!("not in loop"))?;
        lööp.has_continue = true;
        for (name, var_in_loop) in &mut lööp.vars_modified {
            let var_at_bb = self.vars.get(name).cloned().unwrap_or_default();
            var_in_loop.prelatch_phi_inputs.push((var_at_bb, bb));
        }

        let prelatch = lööp.prelatch;
        self.builder().build_unconditional_branch(prelatch);
        Ok(IsTerminated::Terminated)
    }
    /// Builds an unconditional jump to immediately after the loop.
    pub fn build_loop_break(&mut self) -> Result<IsTerminated> {
        let bb = self.current_block();
        let lööp = self
            .loop_stack
            .last_mut()
            .ok_or_else(|| internal_error_value!("not in loop"))?;
        lööp.has_break = true;
        for (name, var_in_loop) in &mut lööp.vars_modified {
            let var_at_bb = self.vars.get(name).cloned().unwrap_or_default();
            var_in_loop.exit_phi_inputs.push((var_at_bb, bb));
        }

        let exit = lööp.exit;
        self.builder().build_unconditional_branch(exit);
        Ok(IsTerminated::Terminated)
    }

    /// Builds a loop over a range of integers.
    pub fn build_iterate_range(
        &mut self,
        span: Span,
        start: LangInt,
        stop: LangInt,
        vars_assigned: HashSet<Arc<String>>,
        mut build_loop_body: impl FnMut(&mut Self, llvm::IntValue) -> Result<IsTerminated>,
    ) -> Result<IsTerminated> {
        let iter_count = (stop - start + 1) as usize;
        if iter_count > crate::LOOP_COUNT_WARN_THRESHOLD && self.mode() == LangMode::User {
            // Throw a warning to the user if the loop iterates too many times.
            self.report_error(Error::loop_count_warning(span, iter_count));
        }

        let phi_ = Cell::new(None);
        let latch_ = Cell::new(None);
        self.build_loop(
            vars_assigned,
            |c| {
                let lööp = c.loop_stack.last().unwrap();
                let preheader = lööp.preheader;
                let latch = lööp.latch;

                let phi = c.builder().build_phi(llvm::int_type(), "loop_index");
                phi.add_incoming(&[(&llvm::const_int(start), preheader)]);
                phi_.set(Some(phi));
                latch_.set(Some(latch));
                let loop_index = phi.as_basic_value().into_int_value();

                let is_loop_end = c.builder().build_int_compare(
                    llvm::IntPredicate::SGT,
                    loop_index,
                    llvm::const_int(stop),
                    "is_loop_end",
                );
                c.build_conditional(
                    is_loop_end,
                    |c| c.build_loop_break(),
                    |_| Ok(IsTerminated::Unterminated),
                )?;

                build_loop_body(c, loop_index)
            },
            |c| {
                let phi = phi_.get().unwrap();
                let latch = latch_.get().unwrap();
                let old_loop_index = phi.as_basic_value().into_int_value();

                let new_loop_index =
                    c.builder()
                        .build_int_add(old_loop_index, llvm::const_int(1), "new_loop_index");
                phi.add_incoming(&[(&new_loop_index, latch)]);
                Ok(IsTerminated::Unterminated)
            },
        )
    }
    /// Builds a loop over an array of values.
    pub fn build_iterate_array(
        &mut self,
        span: Span,
        array_ptr: llvm::PointerValue,
        array_len: usize,
        vars_assigned: HashSet<Arc<String>>,
        mut build_loop_body: impl FnMut(
            &mut Self,
            llvm::IntValue,
            llvm::BasicValueEnum,
        ) -> Result<IsTerminated>,
    ) -> Result<IsTerminated> {
        if array_len == 0 {
            // Don't even build the loop for an empty array.
            return Ok(IsTerminated::Unterminated);
        }

        self.build_iterate_range(span, 0, array_len as LangInt - 1, vars_assigned, |c, i| {
            let ptr = unsafe { c.builder().build_in_bounds_gep(array_ptr, &[i], "iter_ptr") };
            let iter_array_elem = c.builder().build_load(ptr, "iter_array_elem");
            build_loop_body(c, i, iter_array_elem)
        })
    }
    /// Builds a loop over a constant array of integers.
    pub fn build_iterate_const_int_array(
        &mut self,
        span: Span,
        values: impl IntoIterator<Item = LangInt>,
        vars_assigned: HashSet<Arc<String>>,
        mut build_loop_body: impl FnMut(
            &mut Self,
            llvm::IntValue,
            llvm::IntValue,
        ) -> Result<IsTerminated>,
    ) -> Result<IsTerminated> {
        let array_contents = values.into_iter().map(llvm::const_int).collect_vec();
        let array_value = llvm::int_type().const_array(&array_contents);
        let array_ptr = self.add_const_array(array_value, "const_iter_array");
        let len = array_contents.len();
        self.build_iterate_array(span, array_ptr, len, vars_assigned, |c, i, elem| {
            build_loop_body(c, i, elem.into_int_value())
        })
    }
    /// Builds a loop over the vectors in a vector set.
    pub fn build_iterate_vector_set(
        &mut self,
        span: Span,
        vector_set: &VectorSet,
        vars_assigned: HashSet<Arc<String>>,
        mut build_loop_body: impl FnMut(
            &mut Self,
            llvm::IntValue,
            llvm::VectorValue,
        ) -> Result<IsTerminated>,
    ) -> Result<IsTerminated> {
        let vector_type = llvm::vector_type(vector_set.vec_len());
        let array_contents = vector_set.iter().map(llvm::const_vector).collect_vec();
        debug_assert_eq!(array_contents.len(), vector_set.len());
        let array_value = vector_type.const_array(&array_contents);
        let array_ptr = self.add_const_array(array_value, "const_iter_array");
        let len = array_contents.len();
        self.build_iterate_array(span, array_ptr, len, vars_assigned, |c, i, elem| {
            build_loop_body(c, i, elem.into_vector_value())
        })
    }
    /// Builds a loop over the elements in a vector.
    pub fn build_iterate_vector(
        &mut self,
        span: Span,
        vector: llvm::VectorValue,
        vars_assigned: HashSet<Arc<String>>,
        mut build_loop_body: impl FnMut(
            &mut Self,
            llvm::IntValue,
            llvm::BasicValueEnum,
        ) -> Result<IsTerminated>,
    ) -> Result<IsTerminated> {
        let start = 0;
        let stop = vector.get_type().get_size() as LangInt - 1;
        self.build_iterate_range(span, start, stop, vars_assigned, |c, i| {
            let it = c.builder().build_extract_element(vector, i, "iter_value");
            build_loop_body(c, i, it)
        })
    }

    /// Returns an LLVM intrinsic given its name and function signature.
    pub fn get_llvm_intrinisic(
        &mut self,
        name: &str,
        fn_type: llvm::FunctionType,
    ) -> Result<llvm::FunctionValue> {
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

    /// Builds instructions to return with no error.
    pub fn build_return_ok(&mut self) -> IsTerminated {
        self.build_return_err(llvm::MAX_ERROR_INDEX as usize)
    }
    /// Builds instructions to return an error.
    pub fn build_return_err(&mut self, error_index: usize) -> IsTerminated {
        let llvm_return_value = llvm::error_index_type().const_int(error_index as u64, false);
        self.builder().build_return(Some(&llvm_return_value));
        IsTerminated::Terminated
    }
    /// Builds instructions to return an error if some condition is true.
    pub fn build_return_err_if(
        &mut self,
        condition_value: llvm::IntValue,
        error_index: usize,
    ) -> Result<IsTerminated> {
        self.build_conditional(
            condition_value,
            |c| Ok(c.build_return_err(error_index)),
            |_| Ok(IsTerminated::Unterminated),
        )
    }
    /// Builds instructions to return an error if some condition is false.
    pub fn build_return_err_unless(
        &mut self,
        condition_value: impl Into<BoolVal>,
        error_index: usize,
    ) -> Result<IsTerminated> {
        self.build_conditional(
            condition_value,
            |_| Ok(IsTerminated::Unterminated),
            |c| Ok(c.build_return_err(error_index)),
        )
    }
}

/*
 * MATH
 */
impl Compiler {
    /// Builds instructions to perform checked integer arithmetic using an LLVM
    /// intrinsic and return an error if overflow occurs. Both operands must
    /// either be integers or vectors of the same length.
    pub fn build_checked_int_arithmetic<M: llvm::IntMathValue>(
        &mut self,
        error_span: Span,
        op: &str,
        lhs: M,
        rhs: M,
    ) -> Result<llvm::BasicValueEnum> {
        let arg_type = lhs.as_basic_value_enum().get_type();

        // LLVM has intrinsics that perform some math with overflow checks.
        // First, get the name of the intrinsic we want to use (e.g.,
        // "llvm.sadd.with.overflow.i64" for signed addition on i64).
        let type_name = llvm::intrinsic_type_name(arg_type);
        let intrinsic_name = format!("llvm.{}.with.overflow.{}", op, type_name);
        // That intrinsic will return a struct containing the result and a
        // boolean indicated whether overflow occurred. But if we're doing this
        // on a vector then the overflow flag will be a whole vector of booleans
        // instead.
        let bool_type;
        if arg_type.is_vector_type() {
            bool_type = llvm::bool_type()
                .vec_type(arg_type.into_vector_type().get_size())
                .into();
        } else {
            bool_type = llvm::bool_type().into();
        }
        let intrinsic_return_type = llvm::ctx().struct_type(&[arg_type, bool_type], false);
        let intrinsic_fn_type = intrinsic_return_type.fn_type(&[arg_type.into(); 2], false);
        let intrinsic_fn = self.get_llvm_intrinisic(&intrinsic_name, intrinsic_fn_type)?;
        let intrinsic_args = &[
            lhs.as_basic_value_enum().into(),
            rhs.as_basic_value_enum().into(),
        ];

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

        // Return an error if there was overflow.
        let error_index = self.add_runtime_error(Error::integer_overflow(error_span));
        self.build_return_err_if(is_overflow, error_index)?;

        Ok(result_value)
    }

    /// Builds instructions to perform checked integer Euclidean division and
    /// return an error if division by zero or overflow occurs. Both operands
    /// must either be integers or vectors of the same length.
    pub fn build_checked_int_div_euclid<M: llvm::IntMathValue>(
        &mut self,
        error_span: Span,
        lhs: M,
        rhs: M,
    ) -> Result<llvm::BasicValueEnum> {
        use llvm::IntPredicate::{SGT, SLT};

        let zero = lhs.same_type_const_zero();
        let one = lhs.same_type_const_one();

        self.build_int_div_checks(error_span, lhs, rhs)?;

        let b = self.builder();
        let q = b.build_int_signed_div(lhs, rhs, "raw_quotient");
        let r = b.build_int_signed_rem(lhs, rhs, "raw_remainder");

        // Euclidean division algorithm based on Rust std lib's
        // `div_euclid` implementation:
        // https://github.com/rust-lang/rust/blob/4f0b24fd73ec5f80cf61c4bad30538634660ce9a/library/core/src/num/int_macros.rs#L1623-L1627
        let r_lt_zero = b.build_int_compare(SLT, r, zero, "remainder_lt_zero");
        let rhs_gt_zero = b.build_int_compare(SGT, rhs, zero, "div_rhs_gt_zero");
        let q_minus_1 = b
            .build_int_sub(q, one, "raw_quotient_minus_1")
            .as_basic_value_enum();
        let q_plus_1 = b
            .build_int_add(q, one, "raw_quotient_plus_1")
            .as_basic_value_enum();
        let q = q.as_basic_value_enum();
        let tmp = b.build_select(rhs_gt_zero, q_minus_1, q_plus_1, "");
        let ret = b.build_select(r_lt_zero, tmp, q, "quotient");

        Ok(ret)
    }

    /// Builds instructions to perform checked integer Euclidean modulo and
    /// return an error if division by zero or overflow occurs. Both operands
    /// must either be integers or vectors of the same length.
    pub fn build_checked_int_rem_euclid<M: llvm::IntMathValue>(
        &mut self,
        error_span: Span,
        lhs: M,
        rhs: M,
    ) -> Result<llvm::BasicValueEnum> {
        use llvm::IntPredicate::SLT;

        let zero = lhs.same_type_const_zero();

        self.build_int_div_checks(error_span, lhs, rhs)?;

        let b = self.builder();
        let r = b.build_int_signed_rem(lhs, rhs, "raw_remainder");

        // Euclidean modulo algorithm based on Rust std lib's
        // `rem_euclid` implementation:
        // https://github.com/rust-lang/rust/blob/4f0b24fd73ec5f80cf61c4bad30538634660ce9a/library/core/src/num/int_macros.rs#L1661-L1670
        let rhs_lt_zero = b.build_int_compare(SLT, rhs, zero, "div_rhs_lt_zero");
        let r_lt_zero = b.build_int_compare(SLT, r, zero, "remainder_lt_zero");
        let r_minus_rhs = b
            .build_int_sub(r, rhs, "raw_remainder_minus_rhs")
            .as_basic_value_enum();
        let r_plus_rhs = b
            .build_int_add(r, rhs, "raw_remainder_plus_rhs")
            .as_basic_value_enum();
        let r = r.as_basic_value_enum();
        let tmp = b.build_select(rhs_lt_zero, r_minus_rhs, r_plus_rhs, "");
        let ret = b.build_select(r_lt_zero, tmp, r, "remainder");

        Ok(ret)
    }

    /// Builds instructions to check for division by zero and overflow before
    /// integer division. Control flow only proceeds if neither error occurs
    /// (i.e., it is safe to perform division).
    fn build_int_div_checks<M: llvm::IntMathValue>(
        &mut self,
        error_span: Span,
        lhs: M,
        rhs: M,
    ) -> Result<()> {
        use llvm::IntPredicate::EQ;

        let zero = lhs.same_type_const_zero();
        let neg1 = lhs.same_type_const_neg_one();
        let min = lhs.same_type_const_signed(LangInt::MIN);

        // Check whether the divisor is zero.
        let is_divisor_zero = self.build_any_cmp(EQ, rhs, zero)?;
        let error_index = self.add_runtime_error(Error::division_by_zero(error_span));
        self.build_return_err_if(is_divisor_zero, error_index)?;

        // Check whether overflow may occur.
        let b = self.builder();
        let is_dividend_min = b.build_int_compare(EQ, lhs, min, "is_dividend_min");
        let is_divisor_neg1 = b.build_int_compare(EQ, rhs, neg1, "is_divisor_neg1");
        let is_overflow = b.build_and(is_dividend_min, is_divisor_neg1, "is_overflow");
        let is_overflow = self.build_reduce("or", is_overflow.as_basic_value_enum())?;
        let error_index = self.add_runtime_error(Error::integer_overflow(error_span));
        self.build_return_err_if(is_overflow, error_index)?;

        Ok(())
    }

    /// Builds instructions to perform checked integer exponentiation and return
    /// an error if overflow occurs. Both operands must either be integers or
    /// vectors of the same length.
    pub fn build_checked_int_pow<M: llvm::IntMathValue>(
        &mut self,
        error_span: Span,
        base: M,
        exp: M,
    ) -> Result<llvm::BasicValueEnum> {
        match (base.as_basic_value_enum(), exp.as_basic_value_enum()) {
            (llvm::BasicValueEnum::IntValue(base), llvm::BasicValueEnum::IntValue(exp)) => {
                let ret = self._build_checked_int_pow(error_span, base, exp)?;
                Ok(ret.as_basic_value_enum())
            }
            (llvm::BasicValueEnum::VectorValue(bases), llvm::BasicValueEnum::VectorValue(exps)) => {
                let bases = self.build_split_vector(bases);
                let exps = self.build_split_vector(exps);
                let results = bases
                    .into_iter()
                    .zip(exps)
                    .map(|(base, exp)| self._build_checked_int_pow(error_span, base, exp))
                    .collect::<Result<Vec<_>>>()?;
                let ret = self.build_construct_vector(&results);
                Ok(ret.as_basic_value_enum())
            }
            _ => unimplemented!(),
        }
    }

    fn _build_checked_int_pow(
        &mut self,
        error_span: Span,
        base: llvm::IntValue,
        exp: llvm::IntValue,
    ) -> Result<llvm::IntValue> {
        use llvm::IntPredicate::{EQ, SGT, SLT};

        // Check for negative exponent.
        let b = self.builder();
        let exp_lt_zero = b.build_int_compare(SLT, exp, llvm::const_int(0), "exp_lt_zero");
        let error_index = self.add_runtime_error(Error::negative_exponent(error_span));
        self.build_return_err_if(exp_lt_zero, error_index)?;

        let one = llvm::const_int(1);

        // Exponentiation algorithm based on Rust std lib's `checked_pow`
        // implementation:
        // https://github.com/rust-lang/rust/blob/4f0b24fd73ec5f80cf61c4bad30538634660ce9a/library/core/src/num/int_macros.rs#L707-L724
        let b = self.builder();
        let exp_eq_zero = b.build_int_compare(EQ, exp, llvm::const_int(0), "exp_eq_zero");
        let acc = one;
        let mut ret = None;
        self.build_conditional_with_value(
            exp_eq_zero,
            |_| Ok(one),
            |c| {
                c.build_loop(
                    HashSet::new(),
                    |c| {
                        let b = c.builder();
                        let base_phi = b.build_phi(llvm::int_type(), "base");
                        let exp_phi = b.build_phi(llvm::int_type(), "exp");
                        let acc_phi = b.build_phi(llvm::int_type(), "acc");
                        let old_base = base_phi.as_basic_value().into_int_value();
                        let old_exp = exp_phi.as_basic_value().into_int_value();
                        let old_acc = acc_phi.as_basic_value().into_int_value();

                        // `if (exp & 1) == 1`
                        let exp_and_1 = b.build_and(old_exp, one, "exp_and_1");
                        let is_exp_odd = b.build_int_compare(EQ, exp_and_1, one, "is_exp_odd");

                        let new_acc = c.build_conditional_with_value(
                            is_exp_odd,
                            |c| {
                                Ok(c.build_checked_int_arithmetic(
                                    error_span, "smul", old_acc, old_base,
                                )?
                                .into_int_value())
                            },
                            |_| Ok(old_acc),
                        )?;

                        ret = Some(new_acc);

                        // break out of the loop if `!(exp > 1)`
                        let exp_gt_1 = c.builder().build_int_compare(SGT, old_exp, one, "exp_gt_1");
                        c.build_conditional(
                            exp_gt_1,
                            |_| Ok(IsTerminated::Unterminated),
                            |c| c.build_loop_break(),
                        )?;

                        let new_exp = c
                            .builder()
                            .build_right_shift(old_exp, one, true, "new_exp")
                            .as_basic_value_enum();
                        let new_base = c
                            .build_checked_int_arithmetic(error_span, "smul", old_base, old_base)?
                            .as_basic_value_enum();

                        let lööp = c
                            .loop_stack
                            .last()
                            .ok_or_else(|| internal_error_value!("not in loop"))?;
                        base_phi.add_incoming(&[(&base, lööp.preheader), (&new_base, lööp.latch)]);
                        exp_phi.add_incoming(&[(&exp, lööp.preheader), (&new_exp, lööp.latch)]);
                        acc_phi.add_incoming(&[(&acc, lööp.preheader), (&new_acc, lööp.latch)]);

                        Ok(IsTerminated::Unterminated)
                    },
                    |_| Ok(IsTerminated::Unterminated),
                )?;

                Ok(ret.unwrap())
            },
        )
    }

    /// Builds instructions to perform a checked integer left shift.
    pub fn build_checked_int_shl<M: llvm::IntMathValue>(
        &mut self,
        error_span: Span,
        lhs: M,
        rhs: M,
    ) -> Result<llvm::BasicValueEnum> {
        self._build_shift_check_rhs(error_span, rhs)?;
        Ok(self
            .builder()
            .build_left_shift(lhs, rhs, "shl")
            .as_basic_value_enum())
    }
    /// Builds instructions to perform a checked integer arithmetic right shift.
    pub fn build_checked_int_ashr<M: llvm::IntMathValue>(
        &mut self,
        error_span: Span,
        lhs: M,
        rhs: M,
    ) -> Result<llvm::BasicValueEnum> {
        self._build_shift_check_rhs(error_span, rhs)?;
        let sign_extend = true;
        Ok(self
            .builder()
            .build_right_shift(lhs, rhs, sign_extend, "ashr")
            .as_basic_value_enum())
    }
    /// Builds instructions to perform a checked integer logical right shift.
    pub fn build_checked_int_lshr<M: llvm::IntMathValue>(
        &mut self,
        error_span: Span,
        lhs: M,
        rhs: M,
    ) -> Result<llvm::BasicValueEnum> {
        self._build_shift_check_rhs(error_span, rhs)?;
        let sign_extend = false;
        Ok(self
            .builder()
            .build_right_shift(lhs, rhs, sign_extend, "lshr")
            .as_basic_value_enum())
    }

    fn _build_shift_check_rhs<M: llvm::IntMathValue>(
        &mut self,
        error_span: Span,
        rhs: M,
    ) -> Result<()> {
        use llvm::IntPredicate::UGE;

        // Treat `rhs` as unsigned; it must be in the range from 0 (inclusive)
        // to the bit width (exclusive).
        let rhs_ge_64 =
            self.build_any_cmp(UGE, rhs, rhs.same_type_const_unsigned(INT_BITS as u64))?;
        let error_index = self.add_runtime_error(Error::bitshift_out_of_range(error_span));
        self.build_return_err_if(rhs_ge_64, error_index)?;

        Ok(())
    }

    /// Builds a reduction of a vector to an integer using the given operation.
    /// If the argument is already an integer, returns the integer.
    pub fn build_reduce(
        &mut self,
        op: &str,
        value: llvm::BasicValueEnum,
    ) -> Result<llvm::IntValue> {
        match value {
            llvm::BasicValueEnum::ArrayValue(_) => unimplemented!("cannot reduce ArrayValue"),
            llvm::BasicValueEnum::FloatValue(_) => unimplemented!("cannot reduce FloatValue"),
            llvm::BasicValueEnum::IntValue(i) => Ok(i),
            llvm::BasicValueEnum::PointerValue(_) => unimplemented!("cannot reduce PointerValue"),
            llvm::BasicValueEnum::StructValue(_) => unimplemented!("cannot reduce StructValue"),
            llvm::BasicValueEnum::VectorValue(v) => {
                let type_name = llvm::intrinsic_type_name(value.get_type());
                let intrinsic_name =
                    format!("llvm.experimental.vector.reduce.{}.{}", op, type_name);
                let intrinsic_fn_type = v
                    .get_type()
                    .get_element_type()
                    .fn_type(&[value.get_type().into()], false);
                let reduce_fn = self.get_llvm_intrinisic(&intrinsic_name, intrinsic_fn_type)?;
                Ok(self
                    .builder()
                    .build_call(reduce_fn, &[value.into()], &format!("reduce_{}", op))
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value())
            }
        }
    }

    /// Build an integer/vector comparison that return true if any element-wise
    /// comparison is true.
    pub fn build_any_cmp<M: llvm::IntMathValue>(
        &mut self,
        predicate: llvm::IntPredicate,
        lhs: M,
        rhs: M,
    ) -> Result<llvm::IntValue> {
        let cmp_result = self.builder().build_int_compare(predicate, lhs, rhs, "");
        self.build_reduce("or", cmp_result.as_basic_value_enum())
    }
    /// Build an integer/vector comparison that return true if all element-wise
    /// comparisons are true.
    pub fn build_all_cmp<M: llvm::IntMathValue>(
        &mut self,
        predicate: llvm::IntPredicate,
        lhs: M,
        rhs: M,
    ) -> Result<llvm::IntValue> {
        let cmp_result = self.builder().build_int_compare(predicate, lhs, rhs, "");
        self.build_reduce("and", cmp_result.as_basic_value_enum())
    }
}

/*
 * HIGH-LEVEL CONSTRUCTS
 */
impl Compiler {
    /// Builds instructions to get a pointer to a JIT function argument.
    fn build_get_arg_ptr(&mut self, idx: Spanned<u32>) -> Result<(ParamType, llvm::PointerValue)> {
        let ty = self
            .param_types()
            .get(idx.node as usize)
            .cloned()
            .map(|ty| ty.node);
        let arg_struct_ptr = self
            .llvm_fn()
            .get_nth_param(0)
            .unwrap()
            .into_pointer_value();
        let arg_ptr = self.builder().build_struct_gep(
            arg_struct_ptr,
            idx.node,
            &format!("arg_{}_ptr", idx.node),
        );

        ty.zip(arg_ptr.ok())
            .ok_or_else(|| Error::custom(idx.span, "compiled arg index out of range"))
    }

    /// Assigns a value to a variable or removes it.
    pub fn assign_var(&mut self, name: &Arc<String>, value: Result<Val>, statement_span: Span) {
        let value = value.map_err(|e| {
            self.report_error(e);
            VarError::AlreadyReported
        });
        let assign_spans = match &value {
            Ok(v) => vec![(statement_span, v.ty())],
            Err(_) => vec![],
        };
        let var = Var {
            value,
            initial_type: None,
            assign_spans,
            loop_placeholder_index: None,
        };
        self.set_var(name, Some(var));
    }
    /// Sets a variable value. Prefer `assign_var()` when span information is
    /// available.
    pub fn set_var(&mut self, name: &Arc<String>, value: Option<Var>) {
        if &**name != crate::THROWAWAY_VARIABLE {
            let old_var = match value {
                Some(new_var) => self.vars.insert(Arc::clone(name), new_var),
                None => self.vars.remove(name),
            };
            if let Some(overwritten_vars) = self.overwritten_vars_stack.last_mut() {
                overwritten_vars.entry(Arc::clone(name)).or_insert(old_var);
            }
        }
    }
    /// Returns whether a variable exists.
    pub fn has_var(&mut self, name: &Arc<String>) -> bool {
        self.vars.contains_key(name)
    }
    /// Returns a variable value and records the use of the variable if
    /// necessary.
    pub fn access_var(&mut self, name: &Arc<String>, span: Span) -> Result<Val> {
        let var = self.vars.entry(Arc::clone(name)).or_default();
        if let Some(loop_depth) = var.loop_placeholder_index {
            self.loop_stack
                .get_mut(loop_depth)
                .ok_or_else(|| internal_error_value!("placeholder value outlasted loop"))?
                .vars_modified
                .get_mut(name)
                .unwrap()
                .first_use
                .get_or_insert(span);
        }
        var.get_value(span)
    }
    /// Returns a variable value. Prefer `access_var()` when span information is
    /// available.
    pub fn get_var(&self, name: &Arc<String>) -> Var {
        self.vars.get(name).cloned().unwrap_or_default()
    }
    /// Pushes a new blank entry to the overwritten variable stack.
    fn push_overwritten_vars(&mut self) {
        self.overwritten_vars_stack.push(HashMap::new());
    }
    /// Pops an entry off the overwritten variable stack.
    fn pop_and_restore_overwritten_vars(&mut self) -> Result<HashMap<Arc<String>, Option<Var>>> {
        Ok(self
            .overwritten_vars_stack
            .pop()
            .ok_or_else(|| internal_error_value!("overwritten vars stack empty"))?
            .into_iter()
            .map(|(name, value)| {
                let old_value = match value {
                    Some(v) => self.vars.insert(Arc::clone(&name), v),
                    None => self.vars.remove(&name),
                };
                (name, old_value)
            })
            .collect())
    }
    /// Overwrites variables.
    fn overwrite_vars(&mut self, vars_to_overwrite: HashMap<Arc<String>, Option<Var>>) {
        for (name, value) in vars_to_overwrite {
            self.set_var(&name, value)
        }
    }

    /// Builds instructions to fetch a JIT function argument value.
    pub fn build_load_arg(&mut self, idx: Spanned<u32>) -> Result<CpVal> {
        let (arg_ty, arg_ptr) = self.build_get_arg_ptr(idx)?;

        let arg_value = self
            .builder()
            .build_load(arg_ptr, &format!("arg_{}", idx.node));
        Ok(match arg_ty {
            ParamType::Integer => CpVal::Integer(arg_value.into_int_value()),
            ParamType::Cell => CpVal::Cell(arg_value.into_int_value()),
            ParamType::Vector(_) => CpVal::Vector(arg_value.into_vector_value()),
            ParamType::CellArray(shape) => {
                let cells = shape.bounds().map(|bounds| {
                    let cells_origin_ptr = arg_value.into_pointer_value();
                    let strides =
                        llvm::const_vector(crate::utils::ndarray_strides(shape.vec_len(), bounds));
                    self.build_construct_ndarray_from_origin_ptr(
                        bounds,
                        cells_origin_ptr,
                        strides,
                        &format!("arg_{}_ndarray", idx.node),
                    )
                });
                CpVal::CellArrayMut(LlvmCellArray::new(shape, cells))
            }
        })
    }
    /// Builds instructions to set a JIT function argument value.
    pub fn build_store_arg(
        &mut self,
        idx: Spanned<u32>,
        new_arg_value: &Spanned<Val>,
    ) -> Result<()> {
        let (arg_ty, arg_ptr) = self.build_get_arg_ptr(idx)?;

        // Typecheck.
        let expected_type = Type::from(arg_ty);
        let got_type = new_arg_value.ty();
        if got_type != expected_type {
            return Err(Error::type_error(
                new_arg_value.span,
                expected_type,
                &got_type,
            ));
        }

        let new_arg_llvm_value = self.get_cp_val(new_arg_value)?;
        let b = self.builder();
        match new_arg_llvm_value.node {
            CpVal::Integer(v) => b.build_store(arg_ptr, v),
            CpVal::Cell(v) => b.build_store(arg_ptr, v),
            CpVal::Vector(v) => b.build_store(arg_ptr, v),
            CpVal::CellArray(_) | CpVal::CellArrayMut(_) => {
                // Special case; no overwriting cell arrays (because we can't
                // trust that `new_arg_value` will live long enough).
                internal_error!("cannot store cell array arg");
            }
            CpVal::CellSet(v) => b.build_store(arg_ptr, v),
        };
        Ok(())
    }

    /// Builds instructions to execute a statement.
    pub fn build_stmt(&mut self, stmt: ast::Stmt<'_>) -> Result<IsTerminated> {
        let ast = stmt.ast;
        match stmt.data() {
            ast::StmtData::Block(stmt_ids) => {
                for &stmt_id in stmt_ids {
                    // If there is an error while building a statement, keep
                    // going to see if there are more errors to report.
                    match self.build_stmt(ast.get_node(stmt_id)) {
                        Ok(IsTerminated::Unterminated) => (),
                        Ok(IsTerminated::Terminated) => return Ok(IsTerminated::Terminated),
                        Err(e) => self.report_error(e),
                    }
                }
                Ok(IsTerminated::Unterminated)
            }

            ast::StmtData::Assign { lhs, rhs } => {
                let lhs = ast.get_node(*lhs);
                let rhs = ast.get_node(*rhs);
                let expr_span = lhs.span();
                let stmt_span = lhs.span().merge(rhs.span());
                let new_value = self.build_expr(rhs).map_err(|e| {
                    self.report_error(e);
                    Error::AlreadyReported
                });
                let lhs_expression = expressions::from_ast_node(lhs, self);
                lhs_expression.compile_assign(self, expr_span, stmt_span, new_value)?;
                Ok(IsTerminated::Unterminated)
            }

            ast::StmtData::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                let condition = ast.get_node(*condition);
                let condition_value = self.build_bool_expr(condition)?;
                self.build_conditional(
                    condition_value,
                    |c| match *if_true {
                        None => Ok(IsTerminated::Unterminated),
                        Some(id) => c.build_stmt(ast.get_node(id)),
                    },
                    |c| match *if_false {
                        None => Ok(IsTerminated::Unterminated),
                        Some(id) => c.build_stmt(ast.get_node(id)),
                    },
                )
            }

            ast::StmtData::Assert { condition, msg } => {
                let error_index = self.add_runtime_error(match msg {
                    Some(msg) => Error::assertion_failed_with_msg(stmt.span(), msg),
                    None => Error::assertion_failed(stmt.span()),
                });
                let condition = ast.get_node(*condition);
                let condition_value = self.build_bool_expr(condition)?;
                self.build_return_err_unless(condition_value, error_index)
            }
            ast::StmtData::Error { msg } => {
                let error_index = self.add_runtime_error(match msg {
                    Some(msg) => Error::user_error_with_msg(stmt.span(), msg),
                    None => Error::user_error(stmt.span()),
                });
                Ok(self.build_return_err(error_index))
            }

            ast::StmtData::Break => {
                if self.loop_stack.is_empty() {
                    return Err(Error::break_not_in_loop(stmt.span()));
                }
                self.build_loop_break()
            }
            ast::StmtData::Continue => {
                if self.loop_stack.is_empty() {
                    return Err(Error::continue_not_in_loop(stmt.span()));
                }
                self.build_loop_continue()
            }
            ast::StmtData::ForLoop {
                index_var,
                iter_var,
                iter_expr: iter_expr_id,
                first_line_span,
                block,
            } => {
                let stmt_span = *first_line_span;

                let iter_expr = ast.get_node(*iter_expr_id);
                let iter_value = self.build_expr(iter_expr)?;
                let block = ast.get_node(*block);
                let mut vars_assigned = HashSet::new();
                stmt.find_all_assigned_vars(&mut vars_assigned);
                let iterate_index_type_error =
                    Error::iterate_index_type_error(iter_expr.span(), &Type::IntegerSet);
                match iter_value.node {
                    Val::Rt(RtVal::Tag(t)) => Err(Error::unimplemented(stmt_span)),
                    Val::Rt(RtVal::String(_)) => Err(Error::cannot_compile(
                        stmt_span,
                        "iteration over type String",
                    )),

                    Val::Rt(RtVal::Vector(v)) => self.build_iterate_vector(
                        stmt_span,
                        llvm::const_vector(v),
                        vars_assigned,
                        |c, i, x| {
                            if let Some(index_var) = index_var {
                                let index = Ok(Val::Cp(CpVal::Integer(i)));
                                c.assign_var(index_var, index, stmt_span);
                            }
                            let it = Ok(Val::Cp(CpVal::Integer(x.into_int_value())));
                            c.assign_var(iter_var, it, stmt_span);
                            c.build_stmt(block)
                        },
                    ),
                    Val::Rt(RtVal::CellArray(a)) => {
                        let cells = a.cells_iter().map(llvm::const_cell).collect_vec();
                        debug_assert_eq!(cells.len(), a.shape().len());
                        let flat_cells_array = self.add_const_array(
                            llvm::cell_type().const_array(&cells),
                            "cell_array_contents",
                        );

                        self.build_iterate_vector_set(
                            stmt_span,
                            a.shape(),
                            vars_assigned,
                            |c, i, pos| {
                                if let Some(index_var) = index_var {
                                    let index = Ok(Val::Cp(CpVal::Vector(pos)));
                                    c.assign_var(index_var, index, stmt_span);
                                }
                                let cell_ptr = unsafe {
                                    c.builder().build_in_bounds_gep(
                                        flat_cells_array,
                                        &[i],
                                        "iter_cell_ptr",
                                    )
                                };
                                let cell = c
                                    .builder()
                                    .build_load(cell_ptr, "iter_cell")
                                    .into_int_value();
                                let it = Ok(Val::Cp(CpVal::Cell(cell)));
                                c.assign_var(iter_var, it, stmt_span);
                                c.build_stmt(block)
                            },
                        )
                    }

                    Val::Rt(RtVal::EmptySet) => Ok(IsTerminated::Unterminated),
                    Val::Rt(RtVal::IntegerSet(set)) if set.mask().is_none() => {
                        if index_var.is_some() {
                            return Err(iterate_index_type_error);
                        }

                        if let Some((min, max)) = set.bounds() {
                            self.build_iterate_range(stmt_span, min, max, vars_assigned, |c, i| {
                                let it = Ok(Val::Cp(CpVal::Integer(i)));
                                c.assign_var(iter_var, it, stmt_span);
                                c.build_stmt(block)
                            })
                        } else {
                            Ok(IsTerminated::Unterminated)
                        }
                    }
                    Val::Rt(RtVal::IntegerSet(set)) => {
                        if index_var.is_some() {
                            return Err(iterate_index_type_error);
                        }

                        self.build_iterate_const_int_array(
                            stmt_span,
                            set.iter(),
                            vars_assigned,
                            |c, _i, elem| {
                                let it = Ok(Val::Cp(CpVal::Integer(elem)));
                                c.assign_var(iter_var, it, stmt_span);
                                c.build_stmt(block)
                            },
                        )
                    }
                    Val::Rt(RtVal::CellSet(set)) => Err(Error::unimplemented(stmt_span)),
                    Val::Rt(RtVal::VectorSet(set)) => {
                        if index_var.is_some() {
                            return Err(iterate_index_type_error);
                        }

                        self.build_iterate_vector_set(
                            stmt_span,
                            &set,
                            vars_assigned,
                            |c, _i, pos| {
                                let it = Ok(Val::Cp(CpVal::Vector(pos)));
                                c.assign_var(iter_var, it, stmt_span);
                                c.build_stmt(block)
                            },
                        )
                    }

                    Val::Cp(CpVal::Vector(v)) => {
                        self.build_iterate_vector(stmt_span, v, vars_assigned, |c, i, x| {
                            if let Some(index_var) = index_var {
                                let index = Ok(Val::Cp(CpVal::Integer(i)));
                                c.assign_var(index_var, index, stmt_span);
                            }
                            let it = Ok(Val::Cp(CpVal::Integer(x.into_int_value())));
                            c.assign_var(iter_var, it, stmt_span);
                            c.build_stmt(block)
                        })
                    }
                    Val::Cp(CpVal::CellArray(a) | CpVal::CellArrayMut(a)) => self
                        .build_iterate_vector_set(
                            stmt_span,
                            a.shape(),
                            vars_assigned,
                            |c, i, pos| {
                                if let Some(index_var) = index_var {
                                    let index = Ok(Val::Cp(CpVal::Vector(pos)));
                                    c.assign_var(index_var, index, stmt_span);
                                }
                                let cells_ndarray = a
                                    .cells()
                                    .ok_or_else(|| internal_error_value!("no cells array"))?;
                                let cell_ptr = c.build_ndarray_gep_unchecked(
                                    cells_ndarray,
                                    pos,
                                    "iter_cell_ptr",
                                )?;
                                let cell = c
                                    .builder()
                                    .build_load(cell_ptr, "iter_cell")
                                    .into_int_value();
                                let it = Ok(Val::Cp(CpVal::Cell(cell)));
                                c.assign_var(iter_var, it, stmt_span);
                                c.build_stmt(block)
                            },
                        ),

                    Val::Cp(CpVal::CellSet(set)) => Err(Error::unimplemented(stmt_span)),

                    val => Err(Error::iterate_type_error(iter_value.span, &val.ty())),
                }
            }
            ast::StmtData::WhileLoop {
                condition,
                first_line_span,
                block,
            } => {
                let stmt_span = *first_line_span;

                if self.mode() == LangMode::User {
                    self.report_error(Error::cannot_compile(stmt_span, "'while' loop"))
                }

                let condition = ast.get_node(*condition);
                let block = ast.get_node(*block);
                let mut vars_assigned = HashSet::new();
                stmt.find_all_assigned_vars(&mut vars_assigned);
                self.build_loop(
                    vars_assigned,
                    |c| {
                        let condition_value = c.build_bool_expr(condition)?;
                        c.build_conditional(
                            condition_value,
                            |c| c.build_stmt(block),
                            |c| c.build_loop_break(),
                        )
                    },
                    |_| Ok(IsTerminated::Unterminated),
                )
            }

            ast::StmtData::Become(expr_id) => {
                todo!("compile become");
                // let expr = ast.get_node(*expr_id);
                // Ok(Flow::Become(stmt.span(), self.eval_expr(expr)?))
                // Ok(IsTerminated::Terminated)
            }
            ast::StmtData::Remain => {
                todo!("compile remain");
                // Ok(IsTerminated::Terminated)
            }
            ast::StmtData::Return(None) => {
                todo!("compile return");
                // Ok(IsTerminated::Terminated)
            }
            ast::StmtData::Return(Some(expr_id)) => {
                let expr = ast.get_node(*expr_id);
                todo!("compile return");
                // Ok(Flow::Return(stmt.span(), Some(self.eval_expr(expr)?)))
                // Ok(IsTerminated::Terminated)
            }
        }
    }

    /// Builds instructions to evaluate an expression.
    pub fn build_expr(&mut self, expr: ast::Expr<'_>) -> Result<Spanned<Val>> {
        let span = expr.span();
        let expression = expressions::from_ast_node(expr, self);
        let node = expression.compile(self, span)?;
        Ok(Spanned { node, span })
    }
    /// Builds instructions to evaluate an expression and convert the result to
    /// a boolean.
    pub fn build_bool_expr(&mut self, expr: ast::Expr<'_>) -> Result<BoolVal> {
        let value = self.build_expr(expr)?;
        self.build_convert_to_bool(&value)
    }
}

pub trait BuildPhi<V> {
    /// Builds a phi node but does not add any incoming values. (The first value
    /// is merely used to determine the type.)
    fn build_phi(&mut self, first_incoming: &V, name: &str) -> Option<llvm::PhiValue>;
    /// Adds more incoming values to the phi node. Deletes the phi node if the
    /// value cannot be constructed.
    fn populate_phi(
        &mut self,
        phi: Option<llvm::PhiValue>,
        incoming: &[(V, llvm::BasicBlock)],
    ) -> Result<V>;
    /// Builds and populates a phi node.
    fn build_and_populate_phi(
        &mut self,
        incoming: &[(V, llvm::BasicBlock)],
        name: &str,
    ) -> Result<V> {
        let first_incoming = &incoming
            .first()
            .ok_or_else(|| internal_error_value!("phi node has no incoming"))?
            .0;
        let phi = self.build_phi(first_incoming, name);
        self.populate_phi(phi, incoming)
    }
}
macro_rules! impl_build_phi_for_basic_value_types {
    ( $($method:ident() -> $type_name:ty),+ $(,)? ) => {
        $(
            impl BuildPhi<$type_name> for Compiler {
                fn build_phi(
                    &mut self,
                    first_incoming: &$type_name,
                    name: &str,
                ) -> Option<llvm::PhiValue> {
                     Some(self.builder().build_phi(first_incoming.get_type(), name))
                }
                fn populate_phi(
                    &mut self,
                    phi: Option<llvm::PhiValue>,
                    incoming: &[($type_name, llvm::BasicBlock)],
                ) -> Result<$type_name> {
                    let phi = phi.ok_or_else(|| internal_error_value!("phi is None"))?;
                    phi.add_incoming(
                        &incoming
                            .iter()
                            .map(|(v, bb)| (v as &dyn llvm::BasicValue, *bb))
                            .collect_vec(),
                    );
                    Ok(phi.as_basic_value().$method())
                }
            }
        )+
    };
}
impl_build_phi_for_basic_value_types!(
    into_int_value() -> llvm::IntValue,
    into_vector_value() -> llvm::VectorValue,
    into_pointer_value() -> llvm::PointerValue,
);
impl BuildPhi<VarResult> for Compiler {
    fn build_phi(&mut self, first_incoming: &VarResult, name: &str) -> Option<llvm::PhiValue> {
        let ty = first_incoming.as_ref().ok()?.ty();
        let basic_type = self.basic_type(&ty)?;
        Some(self.builder().build_phi(basic_type, name))
    }
    fn populate_phi(
        &mut self,
        phi: Option<llvm::PhiValue>,
        incoming: &[(VarResult, llvm::BasicBlock)],
    ) -> Result<VarResult> {
        fn delete_phi(phi: Option<llvm::PhiValue>) {
            if let Some(phi) = phi {
                // erase_from_basic_block() would be more semantically correct,
                // but it's much less memory-safe.
                phi.as_instruction().remove_from_basic_block();
            }
        }

        // Collect a list of all the incoming errors and values.
        let mut errors = vec![];
        let mut vals = vec![];
        for (result, _bb) in incoming {
            match result {
                Ok(v) => vals.push(v),
                Err(e) => errors.push(e),
            }
        }

        // If there are any incoming errors, return the most severe error.
        let merged_error = errors.into_iter().cloned().fold1(VarError::merge);
        if merged_error.is_some() {
            delete_phi(phi);
        }
        match merged_error {
            Some(VarError::Undefined) if !vals.is_empty() => {
                // Replace Undefined with MaybeUninit if there is some value.
                return Ok(Err(VarError::MaybeUninit));
            }
            Some(VarError::NonConstValue(ty)) if vals.iter().any(|v| v.ty() != ty) => {
                // AmbiguousType takes precedence over NonConstValue, if it
                // applies.
                return Ok(Err(VarError::AmbiguousType));
            }
            Some(e) => return Ok(Err(e)),
            None => (),
        }

        // There are no incoming errors; if all the values are the same (and the
        // phi node hasn't been used yet), exit early by returning that value.
        let phi_is_unused = match phi {
            Some(phi) => phi.as_basic_value().get_first_use().is_none(),
            None => true,
        };
        if vals.iter().map(|v| v.as_rt_val()).all_equal() && phi_is_unused {
            if let Some(rt_val) = vals[0].as_rt_val() {
                delete_phi(phi);
                return Ok(Ok(Val::Rt(rt_val.clone())));
            }
        }

        // There are no incoming errors; check that the types are all the same.
        if !vals.iter().map(|v| v.ty()).all_equal() {
            delete_phi(phi);
            return Ok(Err(VarError::AmbiguousType));
        }

        // All values are the same type; figure out what that type is.
        let ty = vals[0].ty();

        // If we weren't able to construct a phi node, it must be impossible to
        // represent this value in compiled code.
        let phi = match phi {
            Some(phi) => phi,
            None => return Ok(Err(VarError::NonConstValue(ty))),
        };

        // Convert the incoming values (which we now know are all the same type
        // type) to basic values.
        let basic_values = vals
            .iter()
            .map(|v| {
                Ok(self
                    .try_get_cp_val(v)?
                    .ok_or_else(|| internal_error_value!("unable to convert value to CpVal"))?
                    .to_basic_value())
            })
            .collect::<Result<Vec<llvm::BasicValueEnum>>>()?;

        // Build the phi node and add incoming values.
        let dyn_basic_values = basic_values.iter().map(|v| v as &dyn llvm::BasicValue);
        let basic_blocks = incoming.iter().map(|(_v, bb)| *bb);
        let phi_incoming = dyn_basic_values.zip(basic_blocks).collect_vec();
        phi.add_incoming(&phi_incoming);

        Ok(Ok(Val::Cp(CpVal::from_basic_value(
            ty,
            phi.as_basic_value(),
        )?)))
    }
}
impl BuildPhi<Var> for Compiler {
    fn build_phi(&mut self, first_incoming: &Var, name: &str) -> Option<llvm::PhiValue> {
        self.build_phi(&first_incoming.value, name)
    }
    fn populate_phi(
        &mut self,
        phi: Option<llvm::PhiValue>,
        incoming: &[(Var, llvm::BasicBlock)],
    ) -> Result<Var> {
        Ok(Var {
            value: self.populate_phi(
                phi,
                &incoming
                    .iter()
                    .map(|(var, bb)| (var.value.clone(), *bb))
                    .collect_vec(),
            )?,
            initial_type: incoming
                .iter()
                .find_map(|(var, _bb)| var.initial_type.clone()),
            assign_spans: incoming
                .iter()
                .flat_map(|(var, _bb)| var.assign_spans.iter().cloned())
                .sorted_by_key(|(span, _ty)| (span.low(), span.high()))
                .dedup()
                .collect(),
            loop_placeholder_index: incoming
                .iter()
                .find_map(|(var, _bb)| var.loop_placeholder_index),
        })
    }
}

/// Boolean value that may be constant or compiled variable.
#[derive(Debug, Copy, Clone)]
pub enum BoolVal {
    Rt(bool),
    Cp(llvm::IntValue),
}
impl From<bool> for BoolVal {
    fn from(b: bool) -> Self {
        Self::Rt(b)
    }
}
impl From<llvm::IntValue> for BoolVal {
    fn from(i: llvm::IntValue) -> Self {
        Self::Cp(i)
    }
}
impl From<BoolVal> for Val {
    fn from(v: BoolVal) -> Val {
        match v {
            BoolVal::Rt(b) => Val::Rt(RtVal::Integer(b as LangInt)),
            BoolVal::Cp(i) => Val::Cp(CpVal::Integer(i)),
        }
    }
}
impl BoolVal {
    pub fn llvm_int_value(self) -> llvm::IntValue {
        match self {
            BoolVal::Rt(b) => llvm::const_bool(b),
            BoolVal::Cp(i) => i,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IsTerminated {
    /// No unconditional termination built yet; continue building instructions.
    Unterminated,
    /// Unconditional terminator built (e.g., `return`, `break`, etc.); don't
    /// build any more instructions.
    Terminated,
}
