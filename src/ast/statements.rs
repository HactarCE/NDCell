use super::super::compiler::*;
use super::super::errors::*;
use super::super::{Span, Type};
use super::{ExprRef, StatementRef, UserFunction};
use LangErrorMsg::{CannotAssignTypeToVariable, InternalError, TypeError};

pub type StatementBlock = Vec<StatementRef>;

pub trait Statement: std::fmt::Debug {
    fn span(&self) -> Span;
    fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<()>;
}

#[derive(Debug)]
pub struct SetVar {
    span: Span,
    var_name: String,
    value_expr: ExprRef,
}
impl SetVar {
    pub fn try_new(
        span: Span,
        userfunc: &mut UserFunction,
        var_name: String,
        value_expr: ExprRef,
    ) -> LangResult<Self> {
        let value_expr_span = userfunc[value_expr].span();
        // Check that the result of the expression can be stored in a variable.
        let expr_type = userfunc[value_expr].return_type();
        if !expr_type.has_runtime_representation() {
            Err(CannotAssignTypeToVariable(expr_type).with_span(value_expr_span))?;
        }
        // Check that the type of the result of the expression matches the type
        // of the variable.
        let got = expr_type;
        let expected = userfunc.get_or_create_var(&var_name, got);
        if expected != got {
            Err(TypeError { expected, got }.with_span(value_expr_span))?;
        }
        Ok(Self {
            span,
            var_name,
            value_expr,
        })
    }
}
impl Statement for SetVar {
    fn span(&self) -> Span {
        self.span
    }
    fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<()> {
        let var_ptr = *compiler
            .vars()
            .get(&self.var_name)
            .ok_or_else(|| InternalError("Invalid variable index".into()))?;
        let value = userfunc
            .compile_expr(compiler, self.value_expr)?
            .into_basic_value()?;
        compiler.builder().build_store(var_ptr, value);
        Ok(())
    }
}

#[derive(Debug)]
pub struct If {
    span: Span,
    cond_expr: ExprRef,
    if_true: StatementBlock,
    if_false: StatementBlock,
}
impl If {
    pub fn try_new(
        span: Span,
        userfunc: &mut UserFunction,
        cond_expr: ExprRef,
        if_true: StatementBlock,
        if_false: StatementBlock,
    ) -> LangResult<Self> {
        let expected = Type::Int;
        let got = userfunc[cond_expr].return_type();
        if expected != got {
            let cond_expr_span = userfunc[cond_expr].span();
            Err(TypeError { expected, got }.with_span(cond_expr_span))?;
        }
        Ok(Self {
            span,
            cond_expr,
            if_true,
            if_false,
        })
    }
}
impl Statement for If {
    fn span(&self) -> Span {
        self.span
    }
    fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<()> {
        // Evaluate the condition.
        let condition_value = userfunc[self.cond_expr]
            .compile(compiler, userfunc)?
            .as_int()?;

        // Build the destination blocks.
        let if_true_bb = compiler.append_basic_block("ifTrue");
        let if_false_bb = compiler.append_basic_block("ifFalse");
        let merge_bb = compiler.append_basic_block("endIf");

        // Build the switch instruction. We use a switch instead of
        // conditional_branch because a conditional_branch would require us to
        // convert to a 1-bit value, which is unnecessary.
        compiler.builder().build_switch(
            condition_value,
            if_true_bb,
            &[(condition_value.get_type().const_zero(), if_false_bb)],
        );

        // Build the instructions to execute if true.
        compiler.builder().position_at_end(if_true_bb);
        for &statement in &self.if_true {
            userfunc.compile_statement(compiler, statement)?;
        }
        if compiler.needs_terminator() {
            compiler.builder().build_unconditional_branch(merge_bb);
        }

        // Build the instructions to execute if false.
        compiler.builder().position_at_end(if_false_bb);
        for &statement in &self.if_false {
            userfunc.compile_statement(compiler, statement)?;
        }
        if compiler.needs_terminator() {
            compiler.builder().build_unconditional_branch(merge_bb);
        }
        compiler.builder().position_at_end(merge_bb);
        Ok(())
    }
}

#[derive(Debug)]
pub struct Return {
    span: Span,
    ret_expr: ExprRef,
}
impl Return {
    pub fn try_new(span: Span, userfunc: &mut UserFunction, ret_expr: ExprRef) -> LangResult<Self> {
        // Check that the expression matches the expected return type.
        let expected = userfunc.return_type();
        let got = userfunc[ret_expr].return_type();
        if expected != got {
            Err(TypeError { expected, got }.with_span(span))?;
        }
        Ok(Self { span, ret_expr })
    }
}
impl Statement for Return {
    fn span(&self) -> Span {
        self.span
    }
    fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<()> {
        let return_value = userfunc
            .compile_expr(compiler, self.ret_expr)?
            .into_basic_value()?
            .into_int_value();
        compiler.build_return_cell_state(return_value);
        Ok(())
    }
}
