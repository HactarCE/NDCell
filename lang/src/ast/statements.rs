use super::{ErrorPointRef, ExprRef, StatementRef, UserFunction};
use crate::compiler::*;
use crate::errors::*;
use crate::{Span, Type};
use LangErrorMsg::{
    AssertionFailed, CannotAssignTypeToVariable, InternalError, TypeError, UserError,
};

/// List of statements, executed one after another.
pub type StatementBlock = Vec<StatementRef>;

/// Statement node in the AST.
pub trait Statement: std::fmt::Debug {
    /// Returns the span of this statement in the original source code.
    fn span(&self) -> Span;
    /// Compiles this statement.
    fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<()>;
}

/// Assertion, such as `assert 2 + 2 == 4`.
#[derive(Debug)]
pub struct Assert {
    /// Span of this statement in the original source code.
    span: Span,
    /// Expression to test.
    expr: ExprRef,
    /// Error to throw if the expression is falsey.
    error: ErrorPointRef,
}
impl Assert {
    /// Constructs a new assertion statement that checks the result of the given
    /// expression and errors if it is nonzero.
    ///
    /// This method checks the type of the expression.
    pub fn try_new(
        span: Span,
        userfunc: &mut UserFunction,
        expr: ExprRef,
        msg: Option<String>,
    ) -> LangResult<Self> {
        let expr_span = userfunc[expr].span();
        let expected = Type::Int;
        let got = userfunc[expr].return_type();
        if expected != got {
            Err(TypeError { expected, got }.with_span(expr_span))?;
        }
        let error = userfunc.add_error_point(AssertionFailed(msg).with_span(expr_span));
        Ok(Self { span, expr, error })
    }
}
impl Statement for Assert {
    fn span(&self) -> Span {
        self.span
    }
    fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<()> {
        let assert_value = userfunc.compile_expr(compiler, self.expr)?.as_int()?;
        compiler.build_conditional(assert_value, |_| Ok(()), |c| Ok(self.error.compile(c)))?;
        Ok(())
    }
}

/// Error statement, such as `error "this is bad"`.
#[derive(Debug)]
pub struct Error {
    /// Span of this statement in the original source code.
    span: Span,
    /// Error to throw.
    error: ErrorPointRef,
}
impl Error {
    /// Constructs a new error statement that throws an error unconditionally.
    pub fn try_new(
        span: Span,
        userfunc: &mut UserFunction,
        msg: Option<String>,
    ) -> LangResult<Self> {
        let error = userfunc.add_error_point(UserError(msg).with_span(span));
        Ok(Self { span, error })
    }
}
impl Statement for Error {
    fn span(&self) -> Span {
        self.span
    }
    fn compile(&self, compiler: &mut Compiler, _userfunc: &UserFunction) -> LangResult<()> {
        self.error.compile(compiler);
        Ok(())
    }
}

/// Variable assignment statement, such as `set x = 3`.
#[derive(Debug)]
pub struct SetVar {
    /// Span of this statement in the original source code.
    span: Span,
    /// Name of the variable to assign to.
    var_name: String,
    /// Expression to assign.
    value_expr: ExprRef,
}
impl SetVar {
    /// Constructs a new variable assignment statement that assigns the result
    /// of the given expression to the variable with the given name.
    ///
    /// This method creates a new variable if one does not already exist, and
    /// checks the types of the variable and expression.
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
        let var_ptr = compiler
            .vars()
            .get(&self.var_name)
            .ok_or_else(|| InternalError("Invalid variable index".into()))?
            .ptr;
        let value = userfunc
            .compile_expr(compiler, self.value_expr)?
            .into_basic_value()?;
        compiler.builder().build_store(var_ptr, value);
        Ok(())
    }
}

/// A conditional statement, such as `if x == 3 { ... } else { ... }`.
#[derive(Debug)]
pub struct If {
    /// Span of this statement in the original source code.
    span: Span,
    /// Expression to branch based on.
    cond_expr: ExprRef,
    /// Block of statement to evaluate if the condition is truthy.
    if_true: StatementBlock,
    /// Block of statements to evaluate if the condition if falsey.
    if_false: StatementBlock,
}
impl If {
    /// Constructs a new conditional statement branches to either of the given
    /// blocks depending on whether the result of the given expression is truthy
    /// (nonzero) or falsey (zero).
    ///
    /// This method checks the type of the condition expression.
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
        let condition_value = userfunc[self.cond_expr]
            .compile(compiler, userfunc)?
            .as_int()?;
        compiler.build_conditional(
            condition_value,
            |c| userfunc.compile_statement_block(c, &self.if_true),
            |c| userfunc.compile_statement_block(c, &self.if_false),
        )?;
        Ok(())
    }
}

/// A return statement, such as `return 3` or `become #live`.
#[derive(Debug)]
pub struct Return {
    /// Span of this statement in the original source code.
    span: Span,
    /// Expression to return.
    ret_expr: ExprRef,
}
impl Return {
    /// Constructs a new statement that returns the result of the given
    /// expression from the user function.
    ///
    /// This method checks the type of the expression to return.
    pub fn try_new(span: Span, userfunc: &mut UserFunction, ret_expr: ExprRef) -> LangResult<Self> {
        // Check that the expression matches the expected return type.
        let expected = userfunc.return_type();
        let got = userfunc[ret_expr].return_type();
        if expected != got {
            Err(TypeError { expected, got }.with_span(userfunc[ret_expr].span()))?;
        }
        Ok(Self { span, ret_expr })
    }
}
impl Statement for Return {
    fn span(&self) -> Span {
        self.span
    }
    fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<()> {
        let return_value = userfunc.compile_expr(compiler, self.ret_expr)?;
        compiler.build_return_ok(return_value)?;
        Ok(())
    }
}
