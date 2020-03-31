use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;
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

pub struct CodeGen<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Result<Self, Box<dyn Error>> {
        let module = ctx.create_module("sum");
        let builder = ctx.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
        Ok(CodeGen {
            ctx,
            module,
            builder,
            execution_engine,
        })
    }

    pub fn jit_compile_transition_fn(
        &self,
        statements: &ast::StatementBlock,
    ) -> LangResult<JitFunction<TransitionFunction>> {
        unimplemented!()
    }

    fn jit_compile_expr(&self, expression: Spanned<ast::Expr>) -> LangResult<BasicValueEnum> {
        unimplemented!()
    }

    // pub fn jit_compile_sum(&self) -> Option<JitFunction<SumFunc>> {
    //     let i64_type = self.ctx.i64_type();
    //     let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
    //     let function = self.module.add_function("sum", fn_type, None);
    //     let basic_block = self.ctx.append_basic_block(function, "entry");

    //     let bigi_type = self.ctx.custom_width_int_type(64 * 3);

    //     self.builder.position_at_end(basic_block);

    //     let x = function
    //         .get_nth_param(0)?
    //         .into_int_value()
    //         .const_s_extend(bigi_type);
    //     let y = function
    //         .get_nth_param(1)?
    //         .into_int_value()
    //         .const_s_extend(bigi_type);
    //     let z = function
    //         .get_nth_param(2)?
    //         .into_int_value()
    //         .const_s_extend(bigi_type);

    //     let sum = self.builder.build_int_add(x, y, "sum");
    //     let sum = self.builder.build_int_add(sum, z, "sum");

    //     self.builder.build_return(Some(&sum));

    //     unsafe { self.execution_engine.get_function("sum").ok() }
    // }

    fn int_type(&self) -> BasicTypeEnum {
        self.ctx.custom_width_int_type(INT_BITS).into()
    }
    fn cell_state_type(&self) -> BasicTypeEnum {
        self.ctx.custom_width_int_type(CELL_STATE_BITS).into()
    }
}
