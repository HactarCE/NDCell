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
use itertools::Itertools;
use std::collections::HashMap;
use std::sync::{mpsc, Arc};

mod config;
mod function;
mod loops;
mod param;

use super::builtins::{self, Expression};
use crate::ast;
use crate::data::{
    Array, CellSet, CpVal, FallibleTypeOf, LangInt, RtVal, Type, Val, VectorSet, INT_BITS,
};
use crate::errors::{AlreadyReported, Error, Fallible, Result};
use crate::exec::{Ctx, CtxTrait, Runtime};
use crate::llvm::{self, traits::*};
pub use config::CompilerConfig;
pub use function::CompiledFunction;
use loops::Loop;
pub use param::{Param, ParamType};

// TODO: consider making `vars` private and adding `fn vars(&mut self) -> &mut HashMap<_, _>`

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

    ctx: Ctx,
    /// Variable values.
    pub vars: HashMap<Arc<String>, Val>,
    /// Stack of loops containing the statement currently being built. The
    /// innermost loop is at the top of the stack (end of the list).
    loop_stack: Vec<Loop>,

    /// Parameter types for the function being built.
    param_types: Vec<Spanned<ParamType>>,
    /// List of possible runtime errors.
    runtime_errors: Vec<Error>,
}

impl CtxTrait for Compiler {
    fn ctx(&mut self) -> &mut Ctx {
        &mut self.ctx
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
        runtime: Runtime,
        config: CompilerConfig,
    ) -> std::result::Result<CompiledFunction, Vec<Error>> {
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
    /// the same thread it was created (i.e. the one that called this function)
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
                    e
                )]
            })?;

        // Make sure we have a `@compile` directive.
        let ast = compile_directive.ast;
        let (param_type_exprs, function_body) = match compile_directive.data() {
            ast::DirectiveData::Compile { param_types, body } => (param_types, body),
            _ => {
                runtime.error(internal_error_value!(
                    "cannot compile non-@compile directive"
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
                    Type::Vector(_) => todo!("vector param"),
                    Type::Array(_) => todo!("array param"),
                    Type::CellSet => todo!("cell set param"),
                    _ => Err(runtime.error(Error::cannot_compile(span))),
                }
                .map(|ok| Spanned { node: ok, span }),

                Ok(other) => Err(runtime.error(Error::expected(other.span, Type::Type))),

                Err(e) => Err(e),
            })
            .collect::<Fallible<Vec<Spanned<ParamType>>>>()
            .map_err(|_| runtime.ctx.errors.clone())?;

        if !runtime.ctx.errors.is_empty() {
            return Err(runtime.ctx.errors);
        }

        // Initialize variables in compiled code with the values from the
        // `@init` sections.
        let vars = runtime
            .vars
            .into_iter()
            .map(|(k, v)| (k, Val::Rt(v)))
            .collect();

        let mut this = Self {
            config,

            module,
            execution_engine,
            llvm_fn: None,
            builder: llvm::ctx().create_builder(),

            ctx: runtime.ctx,
            vars,
            loop_stack: vec![],

            param_types,
            runtime_errors: vec![],
        };

        // The LLVM function will take a single argument, a pointer to a struct
        // containing the real arguments and return values, if any.
        let llvm_param_types = this
            .param_types
            .iter()
            .map(|param_type| {
                this.llvm_type(&param_type.node.clone().into())
                    .expect("param type has no LLVM representation")
            })
            .collect_vec();
        let params_struct_type = llvm::ctx().struct_type(&llvm_param_types, false);
        let params_struct_ptr_type = params_struct_type
            .ptr_type(llvm::AddressSpace::Generic)
            .as_basic_type_enum();

        // Declare the LLVM function. The actual return value just signals
        // whether there was an error.
        let fn_name = Self::MAIN_FUNCTION_NAME;
        let fn_type = llvm::error_index_type().fn_type(&[params_struct_ptr_type], false);
        let fn_linkage = None;
        this.llvm_fn = Some(this.module.add_function(fn_name, fn_type, fn_linkage));

        match this.build_jit_function(ast.get_node(*function_body)) {
            Ok(ret) => Ok(ret),
            Err(AlreadyReported) => Err(this.ctx.errors),
        }
    }
    /// JIT-compiles a new function that can be called from Rust code.
    ///
    /// All parameters to the function are "in/out" parameters, so any return
    /// values must be included as parameters.
    fn build_jit_function(
        &mut self,
        function_body: ast::Stmt<'_>,
    ) -> Fallible<(llvm::JitFunction, CompiledFunction)> {
        // Build the LLVM IR for the function.
        let entry_bb = self.append_basic_block("entry");
        self.builder().position_at_end(entry_bb);
        self.build_stmt(function_body)?;
        if self.needs_terminator() {
            self.build_return_ok();
        }

        // Don't compile the JIT function if there are any errors.
        if !self.ctx.errors.is_empty() {
            return Err(AlreadyReported);
        }

        // Here we take responsibility for the inherent unsafety of compiling
        // JITted code and turning it into a raw function pointer.
        let jit_fn = unsafe { self.finish_jit_function() }?;
        let jit_fn_ptr = unsafe { jit_fn.raw_fn_ptr() };

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
            CompiledFunction::new(jit_fn_ptr, params, self.runtime_errors.clone()),
        ))
    }

    /// Finishes JIT compiling a function and returns a function pointer to
    /// executable assembly.
    unsafe fn finish_jit_function(&mut self) -> Fallible<llvm::JitFunction> {
        // Check that there are no errors in the LLVM code.
        if !self.llvm_fn().verify(true) {
            eprint!(
                "Error encountered during function compilation; dumping LLVM function to stderr"
            );
            self.llvm_fn().print_to_stderr();
            return Err(self.error(internal_error_value!("LLVM function is invalid")));
        }

        match self.llvm_fn().get_name().to_str() {
            Ok(fn_name) => self.execution_engine.get_function(fn_name).map_err(|e| {
                internal_error_value!("Failed to find JIT-compiled function {:?}: {}", fn_name, e)
            }),
            Err(e) => Err(internal_error_value!(
                "Invalid UTF-8 in LLVM function name (seriously, wtf?): {}",
                e,
            )),
        }
        .map_err(|e| self.error(e))
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
        self.runtime_errors.push(e);
        self.runtime_errors.len() - 1
    }
}

/*
 * TYPES AND VALUES
 */
impl Compiler {
    pub(crate) fn get_val_type(&mut self, v: &Spanned<Val>) -> Fallible<Spanned<Type>> {
        let span = v.span;
        match &v.node {
            Val::Rt(v) => Ok(v.ty()),
            Val::Cp(v) => Ok(v.ty()),
            Val::Unknown(Some(ty)) => Ok(ty.clone()),
            Val::Unknown(None) => Err(self.error(Error::ambiguous_variable_type(span))),
            Val::MaybeUninit => Err(self.error(Error::maybe_uninitialized_variable(span))),
            Val::Err(e) => Err(*e),
        }
        .map(|node| Spanned { node, span })
    }
    pub(crate) fn get_rt_val(&mut self, v: Spanned<Val>) -> Fallible<Spanned<RtVal>> {
        let span = v.span;
        match v.node {
            Val::Rt(v) => Ok(v),
            Val::Cp(_) => Err(self.error(Error::cannot_const_eval(span))),
            Val::Unknown(Some(ty)) => Err(self.error(Error::unknown_variable_value(span, ty))),
            Val::Unknown(None) => Err(self.error(Error::ambiguous_variable_type(span))),
            Val::MaybeUninit => Err(self.error(Error::maybe_uninitialized_variable(span))),
            Val::Err(e) => Err(e),
        }
        .map(|node| Spanned { node, span })
    }
    // TODO: probably remove this
    pub(crate) fn get_cp_val(&mut self, v: Spanned<Val>) -> Fallible<Spanned<CpVal>> {
        let span = v.span;
        match v.node {
            Val::Rt(v) => match v {
                RtVal::Integer(i) => Ok(CpVal::Integer(llvm::const_int(i))),
                RtVal::Cell(i) => Ok(CpVal::Cell(llvm::const_cell(i))),
                RtVal::Vector(v) => Ok(CpVal::Vector(llvm::VectorType::const_vector(
                    &v.iter().map(|&i| llvm::const_int(i)).collect_vec(),
                ))),
                RtVal::Array(a) => Ok(CpVal::Array()), // TODO
                RtVal::CellSet(s) => Ok(CpVal::CellSet(self.build_const_cell_set(&s))),
                _ => Err(self.error(Error::cannot_compile(span))),
            },
            Val::Cp(v) => Ok(v),
            Val::Unknown(Some(ty)) => Err(self.error(Error::unknown_variable_value(span, ty))),
            Val::Unknown(None) => Err(self.error(Error::ambiguous_variable_type(span))),
            Val::MaybeUninit => Err(self.error(Error::maybe_uninitialized_variable(span))),
            Val::Err(e) => Err(e),
        }
        .map(|node| Spanned { node, span })
    }

    /// Returns the LLVM type used for an NDCA type.
    pub fn llvm_type(&self, ty: &Type) -> Option<llvm::BasicTypeEnum> {
        match ty {
            Type::Integer => Some(llvm::int_type().into()),
            Type::Cell => Some(llvm::cell_type().into()),
            Type::Tag => Some(llvm::tag_type().into()),
            Type::Vector(len) => Some(llvm::vector_type((*len)?).into()),
            Type::Array(shape) => Some(self.array_type(shape.as_ref()?).into()),
            Type::CellSet => Some(self.cell_set_type().into()),
            _ => None,
        }
    }

    /// Returns the LLVM type used for cell arrays.
    pub fn array_type(&self, shape: &VectorSet) -> llvm::BasicTypeEnum {
        todo!("array type")
    }
    /// Returns the LLVM type used for cell sets.
    pub fn cell_set_type(&self) -> llvm::VectorType {
        todo!("cell set type, depends on max cell state ID")
    }

    /// Builds instructions to construct a vector with a constant value.
    pub fn build_const_tag(&mut self, t: &str) -> llvm::VectorValue {
        let b = self.builder();
        todo!("build const tag")
    }
    /// Builds instructions to construct a cell array with a constant value.
    pub fn build_const_array(&mut self, a: &Array) -> () {
        let b = self.builder();
        todo!("array type")
    }
    /// Builds instructions to construct a cell set with a constant value.
    pub fn build_const_cell_set(&mut self, s: &CellSet) -> llvm::VectorValue {
        let b = self.builder();
        todo!("cell set type")
    }

    /// Returns the value inside if given a `Constant` value; otherwise returns
    /// an error stating the value must be a compile-time constant.
    fn as_const(&self, v: Spanned<CpVal>) -> Result<RtVal> {
        match v.node {
            _ => Err(Error::cannot_compile(v.span)),
        }
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
    /// Returns the value inside if given an `Array` value or subtype of one;
    /// otherwise returns a type error.
    pub fn as_array(&mut self, v: &Spanned<CpVal>) -> Result<()> {
        match &v.node {
            CpVal::Array() => Ok(()),
            _ => Err(Error::type_error(v.span, Type::Array(None), &v.ty())),
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
    /// boolean to the width of an integer.
    pub fn build_convert_to_bool(&mut self, value: Spanned<Val>) -> Fallible<llvm::IntValue> {
        let cp_val = self.get_cp_val(value)?;
        self.build_convert_cp_val_to_bool(&cp_val)
    }
    /// Builds instructions to cast a value to a boolean and zero-extend that
    /// boolean to the width of an integer.
    pub fn build_convert_cp_val_to_bool(
        &mut self,
        value: &Spanned<CpVal>,
    ) -> Fallible<llvm::IntValue> {
        use llvm::IntPredicate::NE;

        let bool_result = match value.node {
            CpVal::Integer(i) | CpVal::Cell(i) => {
                self.build_any_cmp(NE, i, i.same_type_const_zero())?
            }
            CpVal::Vector(v) => self.build_any_cmp(NE, v, v.same_type_const_zero())?,
            CpVal::Array() => todo!("convert array to bool"),
            CpVal::CellSet(_) => todo!("convert cell set to bool"),
        };

        let b = self.builder();
        Ok(b.build_int_z_extend(bool_result, llvm::int_type(), "zext_bool"))
    }
}

/*
 * LOW-LEVEL UTILITIES
 */
impl Compiler {
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
    pub fn current_block(&mut self) -> llvm::BasicBlock {
        self.builder()
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
    pub fn append_unreachable_basic_block(&mut self) {
        let bb = self.append_basic_block("unreachableBlock");
        self.builder().position_at_end(bb);
    }
    /// Appends a new basic block with a phi node to the end of the current
    /// function and returns the new [`llvm::BasicBlock`] and
    /// [`llvm::PhiValue`].
    ///
    /// The instruction builder is placed at the end of the basic block it was
    /// on before calling this function (so if it's already at the end of a
    /// basic block, it will stay there).
    pub fn append_basic_block_with_phi(
        &mut self,
        bb_name: &str,
        ty: impl BasicType<'static>,
        phi_name: &str,
    ) -> (llvm::BasicBlock, llvm::PhiValue) {
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
        condition_value: llvm::IntValue,
        build_if_true: impl FnOnce(&mut Self) -> Fallible<V>,
        build_if_false: impl FnOnce(&mut Self) -> Fallible<V>,
    ) -> Fallible<V> {
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
        let if_true_needs_terminator = self.needs_terminator();
        if self.needs_terminator() {
            self.builder().build_unconditional_branch(merge_bb);
        }

        // Build the instructions to execute if false.
        self.builder().position_at_end(if_false_bb);
        let value_if_false = build_if_false(self)?;
        let if_false_end_bb = self.current_block();
        let if_false_needs_terminator = self.needs_terminator();
        if self.needs_terminator() {
            self.builder().build_unconditional_branch(merge_bb);
        }

        // Merge values if the closures provided any.
        self.builder().position_at_end(merge_bb);
        let ret = match (if_true_needs_terminator, if_false_needs_terminator) {
            (true, false) => value_if_true,
            (false, true) => value_if_false,
            _ => PhiMergeable::merge(
                value_if_true,
                value_if_false,
                if_true_end_bb,
                if_false_end_bb,
                self,
            ),
        };
        Ok(ret)
    }

    /// Builds a loop.
    ///
    /// `build_contents()` starts at the header and must end with a branch to
    /// either the prelatch or exit. `build_prelatch()` starts at the prelatch
    /// and must NOT end with a jump.
    pub fn build_loop(
        &mut self,
        build_contents: impl FnOnce(&mut Self, Loop) -> Fallible<()>,
        build_prelatch: impl FnOnce(&mut Self, Loop) -> Fallible<()>,
    ) -> Fallible<()> {
        let preheader = self.append_basic_block("preheader");
        let header = self.append_basic_block("header");
        let prelatch = self.append_basic_block("prelatch");
        let latch = self.append_basic_block("latch");
        let exit = self.append_basic_block("exit");

        // TODO: bröther may I have some `lööp`s
        let the_loop = Loop {
            preheader,
            header,
            prelatch,
            latch,
            exit,
        };
        self.loop_stack.push(the_loop);

        self.builder().build_unconditional_branch(preheader);

        // Build preheader.
        self.builder().position_at_end(preheader);
        self.builder().build_unconditional_branch(header);

        // Build header and loop contents.
        self.builder().position_at_end(header);
        build_contents(self, the_loop)?;
        // `build_contents()` branches for us.

        // Build prelatch.
        self.builder().position_at_end(prelatch);
        build_prelatch(self, the_loop)?;
        self.builder().build_unconditional_branch(latch);

        // Build latch.
        self.builder().position_at_end(latch);
        self.builder().build_unconditional_branch(header);

        // Build exit.
        self.builder().position_at_end(exit);
        Ok(())
    }

    /// Builds an unconditional jump to the end of the inside of the loop.
    pub fn build_loop_continue(&mut self, l: Loop) {
        self.builder().build_unconditional_branch(l.prelatch);
    }
    /// Builds an unconditional jump to immediately after the loop.
    pub fn build_loop_break(&mut self, l: Loop) {
        self.builder().build_unconditional_branch(l.exit);
    }

    /// Returns an LLVM intrinsic given its name and function signature.
    pub fn get_llvm_intrinisic(
        &mut self,
        name: &str,
        fn_type: llvm::FunctionType,
    ) -> Fallible<llvm::FunctionValue> {
        match self.module.get_function(name) {
            Some(fn_value) => {
                if fn_value.get_type() == fn_type {
                    Ok(fn_value)
                } else {
                    Err(self.error(internal_error_value!(
                        "Requested multiple LLVM intrinsics with same name but different type signatures",
                    )))
                }
            }
            None => Ok(self.module.add_function(name, fn_type, None)),
        }
    }

    /// Builds instructions to return with no error.
    pub fn build_return_ok(&mut self) {
        self.build_return_err(llvm::MAX_ERROR_INDEX as usize);
    }
    /// Builds instructions to return an error.
    pub fn build_return_err(&mut self, error_index: usize) {
        let llvm_return_value = llvm::error_index_type().const_int(error_index as u64, false);
        self.builder().build_return(Some(&llvm_return_value));
    }

    pub fn build_return_err_if(
        &mut self,
        condition_value: llvm::IntValue,
        error_index: usize,
    ) -> Fallible<()> {
        self.build_conditional(
            condition_value,
            |c| Ok(c.build_return_err(error_index)),
            |_| Ok(()),
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
        name: &str,
        lhs: M,
        rhs: M,
    ) -> Fallible<llvm::BasicValueEnum> {
        let arg_type = lhs.as_basic_value_enum().get_type();

        // LLVM has intrinsics that perform some math with overflow checks.
        // First, get the name of the intrinsic we want to use (e.g.
        // "llvm.sadd.with.overflow.i64" for signed addition on i64).
        let intrinsic_name = format!(
            "llvm.{}.with.overflow.{}",
            name,
            llvm::intrinsic_type_name(arg_type),
        );
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

        let error_index = self.add_runtime_error(Error::integer_overflow(error_span));

        // Return an error if there was overflow.
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
    ) -> Fallible<llvm::BasicValueEnum> {
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
    ) -> Fallible<llvm::BasicValueEnum> {
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
    /// (i.e. it is safe to perform division).
    fn build_int_div_checks<M: llvm::IntMathValue>(
        &mut self,
        error_span: Span,
        lhs: M,
        rhs: M,
    ) -> Fallible<()> {
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
    ) -> Fallible<llvm::BasicValueEnum> {
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
                    .collect::<Fallible<Vec<_>>>()?;
                let ret = llvm::VectorType::const_vector(&results);
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
    ) -> Fallible<llvm::IntValue> {
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
        self.build_conditional(
            exp_eq_zero,
            |_| Ok(one),
            |c| {
                c.build_loop(
                    |c, l| {
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

                        let new_acc = c.build_conditional(
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
                        c.build_conditional(exp_gt_1, |_| Ok(()), |c| Ok(c.build_loop_break(l)))?;

                        let new_exp = c
                            .builder()
                            .build_right_shift(old_exp, one, true, "new_exp")
                            .as_basic_value_enum();
                        let new_base = c
                            .build_checked_int_arithmetic(error_span, "smul", old_base, old_base)?
                            .as_basic_value_enum();

                        base_phi.add_incoming(&[(&base, l.preheader), (&new_base, l.latch)]);
                        exp_phi.add_incoming(&[(&exp, l.preheader), (&new_exp, l.latch)]);
                        acc_phi.add_incoming(&[(&acc, l.preheader), (&new_acc, l.latch)]);

                        c.build_loop_continue(l);

                        Ok(())
                    },
                    |_, _| Ok(()),
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
    ) -> Fallible<llvm::BasicValueEnum> {
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
    ) -> Fallible<llvm::BasicValueEnum> {
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
    ) -> Fallible<llvm::BasicValueEnum> {
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
    ) -> Fallible<()> {
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
    ) -> Fallible<llvm::IntValue> {
        match value {
            llvm::BasicValueEnum::ArrayValue(_) => unimplemented!("cannot reduce ArrayValue"),
            llvm::BasicValueEnum::FloatValue(_) => unimplemented!("cannot reduce FloatValue"),
            llvm::BasicValueEnum::IntValue(i) => Ok(i),
            llvm::BasicValueEnum::PointerValue(_) => unimplemented!("cannot reduce PointerValue"),
            llvm::BasicValueEnum::StructValue(_) => unimplemented!("cannot reduce StructValue"),
            llvm::BasicValueEnum::VectorValue(v) => {
                let fn_type = v
                    .get_type()
                    .get_element_type()
                    .fn_type(&[value.get_type()], false);
                let reduce_fn = self.get_llvm_intrinisic(
                    &format!(
                        "llvm.experimental.vector.reduce.{}.{}",
                        op,
                        // Get name of input type.
                        llvm::intrinsic_type_name(value.get_type()),
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

    /// Build an integer/vector comparison that return true if any element-wise
    /// comparison is true.
    pub fn build_any_cmp<M: llvm::IntMathValue>(
        &mut self,
        predicate: llvm::IntPredicate,
        lhs: M,
        rhs: M,
    ) -> Fallible<llvm::IntValue> {
        let cmp_result = self.builder().build_int_compare(predicate, lhs, rhs, "");
        self.build_reduce("or", cmp_result.as_basic_value_enum())
    }
}

/*
 * HIGH-LEVEL CONSTRUCTS
 */
impl Compiler {
    /// Builds instructions to get a pointer to a JIT function argument.
    fn build_get_arg_ptr(
        &mut self,
        idx: Spanned<u32>,
    ) -> Fallible<(ParamType, llvm::PointerValue)> {
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
            .ok_or_else(|| self.error(Error::custom(idx.span, "compiled arg index out of range")))
    }

    /// Builds instructions to fetch a JIT function argument value.
    pub fn build_load_arg(&mut self, idx: Spanned<u32>) -> Fallible<CpVal> {
        let (arg_ty, arg_ptr) = self.build_get_arg_ptr(idx)?;

        let arg_value = self
            .builder()
            .build_load(arg_ptr, &format!("arg_{}", idx.node));
        Ok(match arg_ty {
            ParamType::Integer => CpVal::Integer(arg_value.into_int_value()),
            ParamType::Cell => CpVal::Cell(arg_value.into_int_value()),
            ParamType::Vector(_) => CpVal::Vector(arg_value.into_vector_value()),
        })
    }
    /// Builds instructions to set a JIT function argument value.
    pub fn build_store_arg(
        &mut self,
        idx: Spanned<u32>,
        new_arg_value: Spanned<Val>,
    ) -> Fallible<()> {
        let (arg_ty, arg_ptr) = self.build_get_arg_ptr(idx)?;

        // Typecheck.
        let expected_type = Type::from(arg_ty);
        let got_type = new_arg_value.fallible_ty()?.map_err(|e| self.error(e))?;
        if got_type != expected_type {
            return Err(self.error(Error::type_error(
                new_arg_value.span,
                expected_type,
                &got_type,
            )));
        }

        let new_arg_llvm_value = self.get_cp_val(new_arg_value)?.llvm_value();
        self.builder().build_store(arg_ptr, new_arg_llvm_value);
        Ok(())
    }

    /// Builds instructions to execute a statement.
    pub fn build_stmt(&mut self, stmt: ast::Stmt<'_>) -> Fallible<()> {
        let ast = stmt.ast;
        match stmt.data() {
            ast::StmtData::Block(stmt_ids) => {
                for &stmt_id in stmt_ids {
                    // If there is an error while building a statement, keep
                    // going to see if there are more errors to report.
                    let _ = self.build_stmt(ast.get_node(stmt_id));
                }
            }

            ast::StmtData::Assign { lhs, op, rhs } => {
                let lhs = ast.get_node(*lhs);
                let rhs = ast.get_node(*rhs);
                let new_value = self.build_expr(rhs)?;

                let lhs_expression = Box::<dyn builtins::Expression>::from(lhs);
                lhs_expression.compile_assign(self, lhs.span(), *op, new_value)?;
            }

            ast::StmtData::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                todo!("compile if/else");
                // let condition = ast.get_node(*condition);
                // if self.eval_expr(condition)?.to_bool()? {
                //     if_true.map_or(Ok(Flow::Proceed), |id| self.exec_stmt(ast.get_node(id)))
                // } else {
                //     if_false.map_or(Ok(Flow::Proceed), |id| self.exec_stmt(ast.get_node(id)))
                // }
            }

            ast::StmtData::Assert { condition, msg } => {
                todo!("compile assert");
                // let condition = ast.get_node(*condition);
                // if self.eval_expr(condition)?.to_bool()? {
                //     Err(match msg {
                //         Some(msg) => Error::assertion_failed_with_msg(stmt.span(), msg),
                //         None => Error::assertion_failed(stmt.span()),
                //     }
                //     .into())
                // } else {
                //     Ok(Flow::Proceed)
                // }
            }
            ast::StmtData::Error { msg } => {
                todo!("compile user error");
                // Err(match msg {
                //     Some(msg) => Error::user_error_with_msg(stmt.span(), msg),
                //     None => Error::user_error(stmt.span()),
                // }
                // .into())
            }

            ast::StmtData::Break => todo!("compile break"),
            ast::StmtData::Continue => todo!("compile continue"),
            ast::StmtData::ForLoop {
                iter_var,
                iter_expr: iter_expr_id,
                block,
            } => {
                let iter_expr = ast.get_node(*iter_expr_id);
                todo!("compile for loop");
                // for it in self.eval_expr(iter_expr)?.iterate()? {
                //     // TODO: when #[feature(hash_raw_entry)] stabalizes, use
                //     // that here to avoid the extra `Arc::clone()` (and consider
                //     // changing `vars` to a `HashMap<String, CpVal>`)
                //     self.vars.insert(Arc::clone(&iter_var), it.node);
                //     match self.exec_stmt(ast.get_node(*block))? {
                //         Flow::Proceed | Flow::Continue(_) => (),
                //         Flow::Break(_) => break,
                //         flow => return Ok(flow),
                //     }
                // }
                // Ok(Flow::Proceed)
            }

            ast::StmtData::Become(expr_id) => {
                todo!("compile become");
                // let expr = ast.get_node(*expr_id);
                // Ok(Flow::Become(stmt.span(), self.eval_expr(expr)?))
            }
            ast::StmtData::Remain => todo!("compile remain"),
            ast::StmtData::Return(None) => todo!("compile return"),
            ast::StmtData::Return(Some(expr_id)) => {
                let expr = ast.get_node(*expr_id);
                todo!("compile return");
                // Ok(Flow::Return(stmt.span(), Some(self.eval_expr(expr)?)))
            }
        }

        Ok(())
    }

    /// Builds instructions to evaluate an expression.
    pub fn build_expr(&mut self, expr: ast::Expr<'_>) -> Fallible<Spanned<Val>> {
        let span = expr.span();
        let expression = Box::<dyn Expression>::from(expr);
        expression
            .compile(self, span)
            .map(|v| Spanned { node: v, span })
    }
    /// Builds instructions to evaluate several expressions in order.
    pub fn build_expr_list(&mut self, exprs: &[ast::Expr<'_>]) -> Fallible<Vec<Spanned<Val>>> {
        exprs.iter().map(|expr| self.build_expr(*expr)).collect()
    }
}

/*

//! JIT compiler for the language.
//!
//! Most of the logic of compiling individual statements and functions is
//! present in methods on the AST nodes themselves, but this module manages all
//! the setup and teardown required.
//!
//! When we compile a function, we don't actually know how many arguments it
//! takes or what its return type is, so we can't encode this in Rust's type
//! system. Instead we create an array containing pointers to all of the inputs
//! to the function and pass a pointer to that array as the first argument, then
//! create a variable for the output of the function and pass a pointer to that
//! as the second argument. The actual return value of the function is just an
//! integer to indicate any error.
//!
//! Note that the compiled function is able to mutate its arguments, which may
//! be useful for debugging or other capabilities in the future.

use itertools::Itertools;
use std::collections::HashMap;
use std::sync::Arc;
use thread_local::ThreadLocal;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, IntType, VectorType};
use inkwell::values::{
    BasicValueEnum, FunctionValue, IntMathValue, IntValue, PhiValue, PointerValue, StructValue,
    VectorValue,
};
use inkwell::{AddressSpace, IntPredicate};

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
use crate::types::{CellStateFilter, LangInt, TypeClass, CELL_STATE_BITS, INT_BITS};
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
        let config = CompilerConfig::default();
        let module = get_ctx().create_module(MODULE_NAME);
        let execution_engine = module
            .create_jit_execution_engine(config.optimization_level)
            .map_err(|e| internal_error_value!("Error creating JIT execution engine: {:?}", e))?;
        Ok(Self {
            module,
            execution_engine,
            function: None,
            config,

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
        arg_names: &[Arc<String>],
        var_types: &HashMap<String, Type>,
    ) -> LangResult<()> {
        // Determine the LLVM function type (signature).
        let llvm_return_type = types::get(&return_type)?;
        let llvm_arg_types = arg_names
            .iter()
            .map(|name| &var_types[&**name])
            .map(types::get)
            .collect::<LangResult<Vec<_>>>()?;
        let fn_type = llvm_return_type.fn_type(&llvm_arg_types, false);

        // Construct the FunctionInProgress.
        self.function = Some(FunctionInProgress {
            llvm_fn: self.module.add_function(name, fn_type, None),
            builder: get_ctx().create_builder(),

            return_type,
            return_value_ptr: None,

            arg_names: arg_names.to_vec(),
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
        arg_names: &[Arc<String>],
        var_types: &HashMap<String, Type>,
    ) -> LangResult<()> {
        // Determine the LLVM function type (signature).
        // The first parameter is a pointer to an array containing pointers to
        // all of the arguments.
        let args_array_ptr_type = get_ctx()
            .i8_type()
            .ptr_type(AddressSpace::Generic)
            .array_type(arg_names.len() as u32)
            .ptr_type(AddressSpace::Generic)
            .as_basic_type_enum();
        // The second parameter is a pointer to hold the return value.
        let return_ptr_type = types::get(&return_type)?
            .ptr_type(AddressSpace::Generic)
            .as_basic_type_enum();
        // The actual LLVM return value just signals whether there was an error.
        let fn_arg_types = &[args_array_ptr_type, return_ptr_type];
        let fn_type = self.llvm_return_type().fn_type(fn_arg_types, false);

        // Construct the FunctionInProgress.
        self.function = Some(FunctionInProgress {
            llvm_fn: self.module.add_function(name, fn_type, None),
            builder: get_ctx().create_builder(),

            return_type,
            return_value_ptr: None,

            arg_names: arg_names.to_vec(),
            vars_by_name: HashMap::new(),
        });
        let entry_bb = self.append_basic_block("entry");
        self.builder().position_at_end(entry_bb);

        // Get pointers to the LLVM arguments.
        let params = self.llvm_fn().get_params();
        assert_eq!(2, params.len());
        let args_ptr = params[0].into_pointer_value();
        self.function_mut().return_value_ptr = Some(params[1].into_pointer_value());

        // Get pointers to arguments and add them to the HashMap of all
        // variables.
        for (i, arg_name) in arg_names.iter().enumerate() {
            let arg_type = var_types[&**arg_name].clone();
            // Index the array.
            let arg_ptr_ptr = unsafe {
                self.builder().build_in_bounds_gep(
                    args_ptr,
                    &[const_int(0), const_uint(i as u64)],
                    &format!("arg{}_ptr_ptr", i),
                )
            };
            let arg_ptr = self
                .builder()
                .build_load(arg_ptr_ptr, &format!("arg{}_ptr_untyped", i))
                .into_pointer_value();
            // Cast the pointer type.
            let arg_ptr = self.builder().build_pointer_cast(
                arg_ptr,
                types::get(&arg_type)?.ptr_type(AddressSpace::Generic),
                &format!("arg{}_ptr", i),
            );
            self.function_mut().vars_by_name.insert(
                String::clone(arg_name),
                Variable {
                    name: String::clone(arg_name),
                    ty: arg_type,
                    is_arg: true,
                    ptr: arg_ptr,
                },
            );
        }

        // Allocate and initialize all the other variables and add them to the
        // HashMap of all variables.
        for (var_name, ty) in var_types {
            if !self.function_mut().vars_by_name.contains_key(var_name) {
                let var_value = self.alloca_and_init_var(var_name.clone(), ty.clone())?;
                self.function_mut()
                    .vars_by_name
                    .insert(var_name.clone(), var_value);
            }
        }

        Ok(())
    }
    /// Allocate space on the stack for the given variable and initialize it to
    /// a default value.
    fn alloca_and_init_var(&mut self, name: String, ty: Type) -> LangResult<Variable> {
        // Allocate space.
        let ptr = self.builder().build_alloca(types::get(&ty)?, &name);
        // Initialize to a default value.
        let default_value = self.default_var_value(&ty)?.into_basic_value()?;
        self.builder().build_store(ptr, default_value);
        Ok(Variable {
            name,
            ty,
            ptr,
            is_arg: false,
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
        let if_true_needs_terminator = self.needs_terminator();
        if self.needs_terminator() {
            self.builder().build_unconditional_branch(merge_bb);
        }

        // Build the instructions to execute if false.
        self.builder().position_at_end(if_false_bb);
        let value_if_false = build_if_false(self)?;
        let if_false_end_bb = self.current_block();
        let if_false_needs_terminator = self.needs_terminator();
        if self.needs_terminator() {
            self.builder().build_unconditional_branch(merge_bb);
        }

        // Merge values if the closures provided any.
        self.builder().position_at_end(merge_bb);
        let ret = match (if_true_needs_terminator, if_false_needs_terminator) {
            (true, false) => value_if_true,
            (false, true) => value_if_false,
            _ => PhiMergeable::merge(
                value_if_true,
                value_if_false,
                if_true_end_bb,
                if_false_end_bb,
                self,
            ),
        };
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

    /// Builds instructions to fetch a value from a variable.
    pub fn build_var_load(&mut self, var_name: &str) -> LangResult<Value> {
        let var = self
            .vars()
            .get(var_name)
            .ok_or_else(|| internal_error_value!("Failed to get variable pointer"))?;
        let var_ty = var.ty.clone();
        let var_ptr = var.ptr;
        let var_value = self.builder().build_load(var_ptr, var_name);
        Ok(Value::from_basic_value(&var_ty, var_value))
    }
    /// Builds instructions to store a value in a variable.
    pub fn build_var_store(&mut self, var_name: &str, value: &Value) -> LangResult<()> {
        let var = self
            .vars()
            .get(var_name)
            .ok_or_else(|| internal_error_value!("Failed to get variable pointer"))?;
        if var.ty != value.ty() {
            uncaught_type_error!()
        }
        let var_ptr = var.ptr;
        self.builder()
            .build_store(var_ptr, value.into_basic_value()?);
        Ok(())
    }

    /// Builds instructions to return a value.
    pub fn build_return_ok(&mut self, value: Value) -> LangResult<()> {
        let ptr = self.function().return_value_ptr.unwrap();
        self.builder().build_store(ptr, value.into_basic_value()?);
        let llvm_return_value = self.llvm_return_type().const_int(u64::MAX, true);
        self.builder().build_return(Some(&llvm_return_value));
        Ok(())
    }
    /// Builds instructions to return an error.
    pub fn build_return_err(&mut self, error_index: usize) {
        let llvm_return_value = self.llvm_return_type().const_int(error_index as u64, false);
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
            _ => internal_error!("Cannot convert {} to {}", value.ty(), TypeClass::Vector),
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
            _ => internal_error!("Cannot convert {} to {}", value.ty(), TypeClass::Rectangle),
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
                TypeClass::CellStateFilter,
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
        on_err: impl Fn(&mut Self) -> LangResult<()>,
    ) -> LangResult<IntValue<'static>> {
        let ndim = pattern.shape.ndim();
        let bounds = pattern.shape.bounds();
        let mask = pattern.shape.flat_mask();

        // Cast to the correct number of dimensions.
        let pos = self.build_vector_cast(Value::Vector(pos), ndim)?;

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
                    .build_int_compare(IntPredicate::SGT, pos, max, "tooHigh");
            let is_too_high = self.build_reduce("or", too_high_vec.into())?;
            // Combine with OR.
            is_out_of_bounds = self
                .builder()
                .build_or(is_too_low, is_too_high, "outOfBounds");
        }

        // Now branch.
        self.build_conditional(
            is_out_of_bounds,
            |c| {
                on_err(c)?;
                Ok(types::cell_state().const_zero().into())
            },
            |c| {
                // Check that the position is inside the mask.
                let is_included_by_mask: IntValue<'static>;
                if pattern.shape.is_rect() {
                    // This position can't be excluded by the mask if the mask includes
                    // everything.
                    is_included_by_mask = llvm_true();
                } else {
                    // Make a flat array for the mask and store it on the stack.
                    let mask_array = get_ctx().bool_type().const_array(
                        &mask
                            .iter()
                            .map(|&x| get_ctx().bool_type().const_int(x as u64, false))
                            .collect_vec(),
                    );
                    let mask_array_ptr =
                        c.builder().build_alloca(mask_array.get_type(), "maskArray");
                    c.builder().build_store(mask_array_ptr, mask_array);
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
                    let strides_vector_value = VectorType::const_vector(
                        &strides.iter().map(|&x| const_uint(x)).collect_vec(),
                    );
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
                    let array_offset_from_origin_vec = c.builder().build_int_nuw_mul(
                        pos,
                        strides_vector_value,
                        "maskOffsetFromOrigin",
                    );
                    let array_offset_from_origin =
                        c.build_reduce("add", array_offset_from_origin_vec.into())?;
                    // Now add the index of the origin to get the final array index.
                    let array_idx = c.builder().build_int_nsw_add(
                        origin_idx,
                        array_offset_from_origin,
                        "maskIndex",
                    );
                    // Finally, index into the array.
                    let mask_element_ptr = unsafe {
                        c.builder().build_gep(
                            mask_array_ptr,
                            &[const_int(0), array_idx],
                            "maskElementPtr",
                        )
                    };
                    is_included_by_mask = c
                        .builder()
                        .build_load(mask_element_ptr, "excludedByMask")
                        .into_int_value();
                }

                // Now branch.
                c.build_conditional(
                    is_included_by_mask,
                    |c| {
                        c.build_get_pattern_cell_state_unchecked(pattern, pos)
                            .map(BasicValueEnum::from)
                    },
                    |c| {
                        on_err(c)?;
                        Ok(types::cell_state().const_zero().into())
                    },
                )
            },
        )
        .map(BasicValueEnum::into_int_value)
    }
    /// Builds an unchecked read of a cell state patternand returns the value.
    /// Do not use this unless you are absolutely sure that the given position
    /// is within the cell state pattern.
    pub fn build_get_pattern_cell_state_unchecked(
        &mut self,
        pattern: &PatternValue,
        pos: VectorValue<'static>,
    ) -> LangResult<IntValue<'static>> {
        let ndim = pattern.shape.ndim();

        // Cast to the correct number of dimensions.
        let pos = self.build_vector_cast(Value::Vector(pos), ndim)?;

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
                .build_gep(cells_ptr, &[const_int(0), ptr_offset], "cellPtr")
        };
        // And finally load from that pointer.
        Ok(self
            .builder()
            .build_load(cell_ptr, "cellState")
            .into_int_value())
    }

    /* MISCELLANY */

    /// Constructs a Value from a ConstValue.
    pub fn value_from_const(&mut self, const_value: ConstValue) -> LangResult<Value> {
        match const_value {
            ConstValue::Void => Ok(Value::Void),
            ConstValue::Int(i) => Ok(Value::Int(const_int(i))),
            ConstValue::CellState(i) => Ok(Value::CellState(
                types::cell_state().const_int(i as u64, false),
            )),
            ConstValue::Vector(values) => Ok(Value::Vector(VectorType::const_vector(
                &values.iter().map(|&i| const_int(i)).collect_vec(),
            ))),
            ConstValue::Pattern(p) => Ok(Value::Pattern({
                let cell_type = types::cell_state();
                let cells = self.module.add_global(
                    cell_type.array_type(p.cells.len() as u32),
                    None,
                    "constPatternCells",
                );
                cells.set_initializer(&BasicValueEnum::from(
                    cell_type.const_array(
                        &p.cells
                            .iter()
                            .map(|&x| cell_type.const_int(x as u64, false))
                            .collect_vec(),
                    ),
                ));
                let cells_start_ptr = cells.as_pointer_value();
                let offset = p.shape.flatten_idx_unchecked(&vec![0; p.ndim()]);
                // Note that this address might be outside the bounds of the
                // cells array (if the pattern does not include the origin).
                let cells_origin_ptr = unsafe {
                    self.builder().build_gep(
                        cells_start_ptr,
                        &[const_int(offset as i64)],
                        "ptrToPatternOrigin",
                    )
                };
                let strides_vector = self
                    .value_from_const(ConstValue::Vector(
                        p.shape
                            .strides()
                            .into_iter()
                            .map(|x| x as LangInt)
                            .collect(),
                    ))?
                    .into_basic_value()?;
                let cell_state_lut_id = get_ctx()
                    .i8_type()
                    .const_int(p.lut.unwrap_or_default() as u64, false);
                let struct_values = [
                    cells_origin_ptr.into(),
                    strides_vector,
                    cell_state_lut_id.into(),
                ];
                PatternValue {
                    value: types::pattern(p.ndim(), p.lut.is_some()).const_named_struct(
                        if p.lut.is_some() {
                            &struct_values
                        } else {
                            &struct_values[..2]
                        },
                    ),
                    shape: p.shape.clone(),
                }
            })),
            ConstValue::IntRange { start, end, step } => {
                Ok(Value::IntRange(VectorType::const_vector(&[
                    const_int(start),
                    const_int(end),
                    const_int(step),
                ])))
            }
            ConstValue::Rectangle(start, end) => Ok({
                assert_eq!(start.len(), end.len(), "Rect dimension mismatch");
                let ndim = start.len();
                let start = self
                    .value_from_const(ConstValue::Vector(start))?
                    .into_basic_value()
                    .unwrap();
                let end = self
                    .value_from_const(ConstValue::Vector(end))?
                    .into_basic_value()
                    .unwrap();
                Value::Rectangle(types::rectangle(ndim).const_named_struct(&[start, end]))
            }),
            ConstValue::CellStateFilter(f) => Ok(Value::CellStateFilter(
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
            )),
            ConstValue::Stencil(_) => internal_error!(NO_RUNTIME_REPRESENTATION),
        }
    }
    /// Returns the default value for variables of the given type, panicking if
    /// the given type has no LLVM representation.
    pub fn default_var_value(&mut self, ty: &Type) -> LangResult<Value> {
        self.value_from_const(ConstValue::default(ty)?)
    }

    /// Returns the LLVM type actually returned from this function (as opposed
    /// to the type semantically returned).
    pub fn llvm_return_type(&self) -> IntType<'static> {
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

    /// Return type of this function.
    return_type: Type,
    /// Pointer to the place to put the return value.
    return_value_ptr: Option<PointerValue<'static>>,

    /// Argument names.
    arg_names: Vec<Arc<String>>,
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
}
*/

/// Trait implemented for `()` and [`llvm::BasicValueEnum`].
pub trait PhiMergeable: Sized {
    fn merge(
        self,
        other: Self,
        self_bb: llvm::BasicBlock,
        other_bb: llvm::BasicBlock,
        compiler: &mut Compiler,
    ) -> Self;
}
impl PhiMergeable for () {
    fn merge(
        self,
        _other: (),
        _self_bb: llvm::BasicBlock,
        _other_bb: llvm::BasicBlock,
        _compiler: &mut Compiler,
    ) -> () {
        ()
    }
}
impl PhiMergeable for llvm::BasicValueEnum {
    fn merge(
        self,
        other: Self,
        self_bb: llvm::BasicBlock,
        other_bb: llvm::BasicBlock,
        compiler: &mut Compiler,
    ) -> Self {
        let phi = compiler
            .builder()
            .build_phi(self.get_type(), "mergeValuesEndIf");
        phi.add_incoming(&[(&self, self_bb), (&other, other_bb)]);
        phi.as_basic_value()
    }
}
macro_rules! impl_phi_mergeable_for_basic_value_types {
    ( $($method:ident() -> $type:ty),+ $(,)? ) => {
        $(
            impl PhiMergeable for $type {
                fn merge(
                    self,
                    other: Self,
                    self_bb: llvm::BasicBlock,
                    other_bb: llvm::BasicBlock,
                    compiler: &mut Compiler,
                ) -> Self {
                    PhiMergeable::merge(
                        self.as_basic_value_enum(),
                        other.as_basic_value_enum(),
                        self_bb,
                        other_bb,
                        compiler,
                    ).$method()
                }
            }
        )+
    };
}
impl_phi_mergeable_for_basic_value_types!(
    into_int_value() -> llvm::IntValue,
);
