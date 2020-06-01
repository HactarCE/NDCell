use super::{ErrorPointRef, ExprRef, StatementRef, UserFunction};
use crate::compiler::*;
use crate::errors::*;
use crate::{Span, Type};
use LangErrorMsg::{AssertionFailed, CannotAssignTypeToVariable, UserError};

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
    assert_expr: ExprRef,
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
        assert_expr: ExprRef,
        msg: Option<String>,
    ) -> LangResult<Self> {
        userfunc[assert_expr].spanned_type().check_eq(Type::Int)?;
        let error =
            userfunc.add_error_point(AssertionFailed(msg).with_span(userfunc[assert_expr].span()));
        Ok(Self {
            span,
            assert_expr,
            error,
        })
    }
}
impl Statement for Assert {
    fn span(&self) -> Span {
        self.span
    }
    fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<()> {
        let assert_value = userfunc
            .compile_expr(compiler, self.assert_expr)?
            .as_int()?;
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
    /// Expression to assign to (left-hand side).
    destination_expr: ExprRef,
    /// Expression to assign from (right-hand side).
    source_expr: ExprRef,
}
impl SetVar {
    /// Constructs a new variable assignment statement that assigns the result
    /// of the given expression to the variable with the given name.
    ///
    /// This method also the types of the source and destination expressions to
    /// ensure that they match.
    pub fn try_new(
        span: Span,
        userfunc: &mut UserFunction,
        destination_expr: ExprRef,
        source_expr: ExprRef,
    ) -> LangResult<Self> {
        // Check that the result of the source expression can be stored in a
        // variable at all.
        let source_expr_type = userfunc[source_expr].return_type();
        if !source_expr_type.has_runtime_representation() {
            Err(CannotAssignTypeToVariable(source_expr_type)
                .with_span(userfunc[source_expr].span()))?;
        }
        // Check the types of the source and destinations.
        let expected = userfunc[destination_expr].assign_type(userfunc)?;
        userfunc[source_expr].spanned_type().check_eq(expected)?;
        Ok(Self {
            span,
            destination_expr,
            source_expr,
        })
    }
}
impl Statement for SetVar {
    fn span(&self) -> Span {
        self.span
    }
    fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<()> {
        let value = userfunc.compile_expr(compiler, self.source_expr)?;
        userfunc[self.destination_expr].compile_assign(compiler, userfunc, value)?;
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
        userfunc[cond_expr].spanned_type().check_eq(Type::Int)?;
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
        userfunc[ret_expr]
            .spanned_type()
            .check_eq(userfunc.return_type())?;
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
