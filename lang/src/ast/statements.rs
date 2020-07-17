use super::{ErrorPointRef, ExprRef, StatementRef, UserFunction, UserFunctionKind};
use crate::compiler::Compiler;
use crate::errors::*;
use crate::parser;
use crate::{Span, Spanned};
use LangErrorMsg::{
    AssertionFailed, BecomeInHelperFunction, CannotAssignTypeToVariable, NotInLoop,
    ReturnInTransitionFunction, UserError,
};

/// List of statements, executed one after another.
pub type StatementBlock = Vec<StatementRef>;

pub fn from_parse_tree(
    userfunc: &mut UserFunction,
    parse_tree: &Spanned<parser::Statement>,
) -> LangResult<Box<dyn Statement>> {
    let span = parse_tree.span;
    match &parse_tree.inner {
        // Assertion
        parser::Statement::Assert { expr, msg } => {
            let expr = userfunc.build_expression_ast(&expr)?;
            let msg = msg
                .as_ref()
                .map(|string_lit| (*string_lit.inner.contents).to_owned());
            Ok(Box::new(Assert::try_new(span, userfunc, expr, msg)?))
        }
        // Error
        parser::Statement::Error { msg } => {
            let msg = msg
                .as_ref()
                .map(|string_lit| (*string_lit.inner.contents).to_owned());
            Ok(Box::new(Error::try_new(span, userfunc, msg)?))
        }
        // Variable assignment statement
        parser::Statement::SetVar {
            var_expr,
            assign_op,
            value_expr,
        } => {
            // Handle assignments with operators (e.g. `x += 3`).
            let value_expr = match assign_op.op() {
                Some(op) => userfunc.build_expression_ast(&Spanned {
                    span,
                    inner: parser::Expr::BinaryOp {
                        lhs: Box::new(var_expr.clone()),
                        op,
                        rhs: Box::new(value_expr.clone()),
                    },
                })?,
                None => userfunc.build_expression_ast(&value_expr)?,
            };
            // Register a new variable if necessary.
            if let parser::Expr::Ident(var_name) = &var_expr.inner {
                userfunc.get_or_create_var(var_name, userfunc[value_expr].ret_type().clone());
            }
            // Construct the statement.
            let var_expr = userfunc.build_expression_ast(var_expr)?;
            Ok(Box::new(SetVar::try_new(
                span, userfunc, var_expr, value_expr,
            )?))
        }
        // If statement
        parser::Statement::If {
            cond_expr,
            if_true,
            if_false,
        } => {
            let cond_expr = userfunc.build_expression_ast(cond_expr)?;
            let if_true = userfunc.build_statement_block_ast(if_true)?;
            let if_false = userfunc.build_statement_block_ast(if_false)?;
            Ok(Box::new(If::try_new(
                span, userfunc, cond_expr, if_true, if_false,
            )?))
        }
        // For loop
        parser::Statement::ForLoop {
            var_expr,
            iter_expr,
            block,
        } => {
            let iter_expr = userfunc.build_expression_ast(iter_expr)?;
            // Register a new variable if necessary.
            if let parser::Expr::Ident(var_name) = &var_expr.inner {
                userfunc.get_or_create_var(
                    var_name,
                    userfunc[iter_expr]
                        .spanned_ret_type()
                        .typecheck_can_iterate()?,
                );
            }
            let var_expr = userfunc.build_expression_ast(var_expr)?;
            let block = userfunc.build_statement_block_ast(block)?;
            Ok(Box::new(ForLoop::try_new(
                span, userfunc, var_expr, iter_expr, block,
            )?))
        }
        // Break statement
        parser::Statement::Break => Ok(Box::new(Break::new(span))),
        // Continue statement
        parser::Statement::Continue => Ok(Box::new(Continue::new(span))),
        // Become statement (In a transition function, `become` should be used, not `return`.)
        parser::Statement::Become(ret_expr) => match userfunc.kind() {
            UserFunctionKind::Helper(_) => Err(BecomeInHelperFunction.with_span(span)),
            UserFunctionKind::Transition => {
                let ret_expr = userfunc.build_expression_ast(ret_expr)?;
                Ok(Box::new(Return::try_new(span, userfunc, ret_expr)?))
            }
        },
        // Return statement (In a helper function, `return` should be used, not `become`.)
        parser::Statement::Return(ret_expr) => match userfunc.kind() {
            UserFunctionKind::Helper(_) => {
                let ret_expr = userfunc.build_expression_ast(ret_expr)?;
                Ok(Box::new(Return::try_new(span, userfunc, ret_expr)?))
            }
            UserFunctionKind::Transition => Err(ReturnInTransitionFunction.with_span(span))?,
        },
    }
}

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
        typecheck!(userfunc[assert_expr].spanned_ret_type(), Int)?;
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
        compiler.build_new_unreachable_bb();
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
    /// This method checks the types of the source and destination expressions
    /// to ensure that they match.
    pub fn try_new(
        span: Span,
        userfunc: &mut UserFunction,
        destination_expr: ExprRef,
        source_expr: ExprRef,
    ) -> LangResult<Self> {
        // Check that the result of the source expression can be stored in a
        // variable at all.
        let source_expr_type = userfunc[source_expr].ret_type().clone();
        if !source_expr_type.has_runtime_representation() {
            Err(CannotAssignTypeToVariable(source_expr_type)
                .with_span(userfunc[source_expr].span()))?;
        }
        // Check the types of the source and destinations.
        let expected = userfunc[destination_expr].assign_type(userfunc)?;
        userfunc[source_expr]
            .spanned_ret_type()
            .typecheck(expected)?;
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
        userfunc[self.destination_expr].compile_assign(compiler, value, userfunc)?;
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
    /// Block of statements to execute if the condition is truthy.
    if_true: StatementBlock,
    /// Block of statements to execute if the condition if falsey.
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
        typecheck!(userfunc[cond_expr].spanned_ret_type(), Int)?;
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

/// A for loop, such as `for x in 1..10 { ... }`
#[derive(Debug)]
pub struct ForLoop {
    /// Span of this statement in the original source code.
    span: Span,
    /// Expression to assign the iteration value to.
    var_expr: ExprRef,
    /// Expression to iterate over.
    iter_expr: ExprRef,
    /// Block of statements to execute on each iteration.
    block: StatementBlock,
}
impl ForLoop {
    /// Constructs a new for loop.
    ///
    /// This method checks the types of the variable and iteration expressions.
    pub fn try_new(
        span: Span,
        userfunc: &mut UserFunction,
        var_expr: ExprRef,
        iter_expr: ExprRef,
        block: StatementBlock,
    ) -> LangResult<Self> {
        // Check types.
        let var_type = userfunc[var_expr].spanned_assign_type(userfunc)?;
        let iteration_type = userfunc[iter_expr]
            .spanned_ret_type()
            .typecheck_can_iterate()?;
        var_type.typecheck(iteration_type)?;

        Ok(Self {
            span,
            var_expr,
            iter_expr,
            block,
        })
    }
}
impl Statement for ForLoop {
    fn span(&self) -> Span {
        self.span
    }
    fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<()> {
        let iterator = userfunc.compile_expr(compiler, self.iter_expr)?;
        compiler.build_value_iter(iterator, |c, iteration_value| {
            // Assign iteration variable.
            userfunc[self.var_expr].compile_assign(c, iteration_value, userfunc)?;
            // Compile statements.
            userfunc.compile_statement_block(c, &self.block)?;
            Ok(())
        })?;
        Ok(())
    }
}

/// A break statement.
#[derive(Debug)]
pub struct Break {
    /// Span of this statement in the original source code.
    span: Span,
}
impl Break {
    /// Constructs a new statement that breaks out of the innermost loop.
    pub fn new(span: Span) -> Self {
        Self { span }
    }
}
impl Statement for Break {
    fn span(&self) -> Span {
        self.span
    }
    fn compile(&self, compiler: &mut Compiler, _userfunc: &UserFunction) -> LangResult<()> {
        compiler
            .build_jump_to_loop_exit()
            .map_err(|_| NotInLoop.with_span(self.span))?;
        compiler.build_new_unreachable_bb();
        Ok(())
    }
}

/// A break statement.
#[derive(Debug)]
pub struct Continue {
    /// Span of this statement in the original source code.
    span: Span,
}
impl Continue {
    /// Constructs a new statement that continues to the next iteration of the
    /// innermost loop.
    pub fn new(span: Span) -> Self {
        Self { span }
    }
}
impl Statement for Continue {
    fn span(&self) -> Span {
        self.span
    }
    fn compile(&self, compiler: &mut Compiler, _userfunc: &UserFunction) -> LangResult<()> {
        compiler
            .build_jump_to_loop_entry()
            .map_err(|_| NotInLoop.with_span(self.span))?;
        compiler.build_new_unreachable_bb();
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
            .spanned_ret_type()
            .typecheck(userfunc.kind().return_type())?;
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
        compiler.build_new_unreachable_bb();
        Ok(())
    }
}
