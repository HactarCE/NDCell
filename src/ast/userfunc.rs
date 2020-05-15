use std::collections::HashMap;
use std::ops::Index;
use std::rc::Rc;

use super::super::compiler::{Compiler, Value};
use super::super::errors::*;
use super::super::functions;
use super::super::lexer::{OperatorToken, PunctuationToken};
use super::super::parser;
use super::super::{ConstValue, Span, Spanned, Type};
use super::statements;
use super::{Args, Expr, Function, RuleMeta, Statement, StatementBlock};
use LangErrorMsg::{
    BecomeInHelperFunction, Expected, ExpectedGot, InternalError, ReturnInTransitionFunction,
    UseOfUninitializedVariable,
};

#[derive(Debug, Default)]
pub struct UserFunction {
    rule_meta: Rc<RuleMeta>,
    return_type: Type,
    is_transition_function: bool,
    statements: Vec<Box<dyn Statement>>,
    expressions: Vec<Expr>,
    error_points: Vec<LangError>,
    variables: HashMap<String, Type>,
}
impl UserFunction {
    pub fn rule_meta(&self) -> &Rc<RuleMeta> {
        &self.rule_meta
    }

    pub fn new_transition_function(rule_meta: Rc<RuleMeta>) -> Self {
        Self {
            is_transition_function: true,
            ..Self::new_helper_function(rule_meta, Type::CellState)
        }
    }
    pub fn new_helper_function(rule_meta: Rc<RuleMeta>, return_type: Type) -> Self {
        Self {
            rule_meta,
            return_type,
            is_transition_function: false,
            statements: vec![],
            expressions: vec![],
            variables: HashMap::new(),
            error_points: vec![],
        }
    }

    pub fn return_type(&self) -> Type {
        self.return_type
    }

    pub fn try_get_var(&self, span: Span, var_name: &str) -> LangResult<Type> {
        self.variables
            .get(var_name)
            .copied()
            .ok_or_else(|| UseOfUninitializedVariable.with_span(span))
    }
    pub fn get_or_create_var(&mut self, var_name: &str, new_ty: Type) -> Type {
        if let Some(existing_type) = self.variables.get(var_name) {
            *existing_type
        } else {
            self.variables.insert(var_name.to_owned(), new_ty);
            new_ty
        }
    }

    pub fn build_statement_block_ast(
        &mut self,
        parser_statements: &parser::StatementBlock,
    ) -> LangResult<StatementBlock> {
        let mut block = vec![];
        for parser_statement in parser_statements {
            let span = parser_statement.span;

            let new_statement: Box<dyn Statement> = match &parser_statement.inner {
                parser::Statement::SetVar {
                    var_expr,
                    assign_op,
                    value_expr,
                } => {
                    if let parser::Expr::Ident(var_name) = &var_expr.inner {
                        // Handle assignments with operators (e.g. `x += 3`).
                        let value_expr = match assign_op.op() {
                            Some(op) => self.build_expression_ast(&Spanned {
                                span,
                                inner: parser::Expr::BinaryOp {
                                    lhs: Box::new(Spanned {
                                        span: var_expr.span,
                                        inner: parser::Expr::Ident(var_name.to_owned()),
                                    }),
                                    op: op,
                                    rhs: Box::new(value_expr.clone()),
                                },
                            })?,
                            None => self.build_expression_ast(&value_expr)?,
                        };
                        Box::new(statements::SetVar::try_new(
                            span,
                            self,
                            var_name.to_owned(),
                            value_expr,
                        )?)
                    } else {
                        Err(Expected("variable name").with_span(parser_statement.span))?
                    }
                }

                parser::Statement::If {
                    cond_expr,
                    if_true,
                    if_false,
                } => {
                    let cond_expr = self.build_expression_ast(cond_expr)?;
                    let if_true = self.build_statement_block_ast(if_true)?;
                    let if_false = self.build_statement_block_ast(if_false)?;
                    Box::new(statements::If::try_new(
                        span, self, cond_expr, if_true, if_false,
                    )?)
                }

                // In a transition function, `become` should be used, not `return`.
                parser::Statement::Become(ret_expr) => {
                    if self.is_transition_function {
                        let ret_expr = self.build_expression_ast(ret_expr)?;
                        Box::new(statements::Return::try_new(span, self, ret_expr)?)
                    } else {
                        Err(BecomeInHelperFunction.with_span(span))?
                    }
                }

                // In a helper function, `return` should be used, not `become`.
                parser::Statement::Return(ret_expr) => {
                    if self.is_transition_function {
                        Err(ReturnInTransitionFunction.with_span(span))?
                    } else {
                        let ret_expr = self.build_expression_ast(ret_expr)?;
                        Box::new(statements::Return::try_new(span, self, ret_expr)?)
                    }
                }
            };

            block.push(self.add_statement(new_statement));
        }
        Ok(block)
    }
    pub fn build_expression_ast(
        &mut self,
        parser_expr: &Spanned<parser::Expr>,
    ) -> LangResult<ExprRef> {
        let span = parser_expr.span;
        let args: Args;
        let function: Box<dyn Function>;

        match &parser_expr.inner {
            parser::Expr::Int(i) => {
                args = Args::none();
                function = Box::new(functions::literals::Int(*i));
            }

            parser::Expr::Ident(s) => {
                args = Args::none();
                function = Box::new(functions::misc::GetVar::try_new(self, span, s.to_owned())?);
            }

            parser::Expr::Group { start_token, inner } => {
                use PunctuationToken::*;
                match start_token {
                    LParen => return self.build_expression_ast(inner),
                    LBracket => todo!("Construct vector"),
                    _ => return Err(InternalError("Invalid group".into()).with_span(span)),
                }
            }

            parser::Expr::List(_) => {
                return Err(ExpectedGot {
                    expected: "expression",
                    got: "comma-separated list",
                }
                .with_span(span))
            }

            parser::Expr::UnaryOp { op, operand } => match op {
                OperatorToken::Minus => {
                    args = Args::from(vec![self.build_expression_ast(operand)?]);
                    function = Box::new(functions::math::NegInt::try_new(self, span)?);
                }
                OperatorToken::Tag => {
                    args = Args::from(vec![self.build_expression_ast(operand)?]);
                    function = Box::new(functions::convert::IntToCellState::try_new(self, span)?);
                }
                _ => return Err(InternalError("Invalid unary operator".into()).with_span(span)),
            },

            parser::Expr::BinaryOp { lhs, op, rhs } => match op {
                OperatorToken::Plus
                | OperatorToken::Minus
                | OperatorToken::Asterisk
                | OperatorToken::Slash
                | OperatorToken::Percent
                | OperatorToken::DoubleAsterisk
                | OperatorToken::DoubleLessThan
                | OperatorToken::DoubleGreaterThan
                | OperatorToken::TripleGreaterThan
                | OperatorToken::Ampersand
                | OperatorToken::Pipe => {
                    args = Args::from(vec![
                        self.build_expression_ast(lhs)?,
                        self.build_expression_ast(rhs)?,
                    ]);
                    function = Box::new(functions::math::BinaryIntOp::try_new(self, span, *op)?);
                }
                OperatorToken::Dot => todo!("Method call"),
                OperatorToken::DotDot => todo!("Range"),
                _ => return Err(InternalError("Invalid binary operator".into()).with_span(span)),
            },

            parser::Expr::Cmp { exprs, cmps } => {
                args = Args::from(
                    exprs
                        .iter()
                        .map(|e| self.build_expression_ast(e))
                        .collect::<LangResult<Vec<_>>>()?,
                );
                function = Box::new(functions::cmp::Cmp::try_new(self, &args, cmps.clone())?);
            }
        };

        let expr = Expr::try_new(span, self, function, args)?;
        Ok(self.add_expr(expr))
    }

    fn add_statement(&mut self, statement: Box<dyn Statement>) -> StatementRef {
        let idx = self.statements.len();
        self.statements.push(statement);
        StatementRef(idx)
    }
    fn add_expr(&mut self, expr: Expr) -> ExprRef {
        let idx = self.expressions.len();
        self.expressions.push(expr);
        ExprRef(idx)
    }
    pub fn add_error_point(&mut self, error: LangError) -> ErrorPointRef {
        let idx = self.error_points.len();
        self.error_points.push(error.clone());
        ErrorPointRef { idx, error }
    }

    pub fn compile_statement(
        &self,
        compiler: &mut Compiler,
        statement: StatementRef,
    ) -> LangResult<()> {
        self[statement].compile(compiler, self)
    }
    pub fn compile_expr(&self, compiler: &mut Compiler, expr: ExprRef) -> LangResult<Value> {
        self[expr].compile(compiler, self)
    }
    pub fn const_eval_expr(&self, expr: ExprRef) -> LangResult<ConstValue> {
        self[expr].const_eval(self)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprRef(usize);
impl Index<ExprRef> for UserFunction {
    type Output = Expr;
    fn index(&self, expr_ref: ExprRef) -> &Expr {
        &self.expressions[expr_ref.0]
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct StatementRef(usize);
impl Index<StatementRef> for UserFunction {
    type Output = Box<dyn Statement>;
    fn index(&self, statement_ref: StatementRef) -> &Box<dyn Statement> {
        &self.statements[statement_ref.0]
    }
}

#[derive(Debug, Clone)]
pub struct ErrorPointRef {
    idx: usize,
    error: LangError,
}
impl ErrorPointRef {
    pub fn compile(&self, compiler: &mut Compiler) {
        compiler.build_return_error(self.idx);
    }
    pub fn error(&self) -> LangError {
        self.error.clone()
    }
    pub fn err<T>(&self) -> LangResult<T> {
        Err(self.error())
    }
}
impl PartialEq for ErrorPointRef {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}
impl Eq for ErrorPointRef {}
impl Index<ErrorPointRef> for UserFunction {
    type Output = LangError;
    fn index(&self, error_point_ref: ErrorPointRef) -> &LangError {
        &self.error_points[error_point_ref.idx]
    }
}
