use std::collections::HashMap;
use std::ops::Index;
use std::rc::Rc;

use super::{statements, Args, Expr, RuleMeta, Statement, StatementBlock};
use crate::compiler::{CompiledFunction, Compiler, Value};
use crate::errors::*;
use crate::functions;
use crate::parser;
use crate::{ConstValue, Span, Spanned, Type};
use LangErrorMsg::{
    BecomeInHelperFunction, CannotIndexType, Expected, FunctionLookupError,
    ReturnInTransitionFunction, Unimplemented, UseOfUninitializedVariable,
};

/// A user-defined function node in the AST.
#[derive(Debug, Default)]
pub struct UserFunction {
    /// Metadata of the rule that this user function is part of.
    rule_meta: Rc<RuleMeta>,
    /// Name of this function.
    name: String,
    /// Whether this is the transition function, as opposed to a helper function
    /// (determines whether `become`/`remain` or `return` is accepted).
    is_transition_function: bool,

    /// Top-level statement block, consisting of StatementRefs to self.statements.
    top_level_statements: StatementBlock,
    /// List of every statement AST node.
    statements: Vec<Box<dyn Statement>>,
    /// List of every expression AST node.
    expressions: Vec<Expr>,
    /// List of every possible runtime error.
    error_points: Vec<LangError>,

    /// HashMap of variable types (including arguments), indexed by name.
    variables: HashMap<String, Type>,
    /// List of variable names for arguments.
    arg_names: Vec<String>,
    /// Return type of this function.
    return_type: Type,
}
impl UserFunction {
    /// Constructs a new transition function.
    pub fn new_transition_function(rule_meta: Rc<RuleMeta>) -> Self {
        Self {
            is_transition_function: true,
            // TODO: take arguments in the transition function
            // TODO: reserved word for transition function?
            ..Self::new_helper_function(rule_meta, "transition".to_owned(), vec![], Type::CellState)
        }
    }
    /// Constructs a new helper function that returns the given type.
    pub fn new_helper_function(
        rule_meta: Rc<RuleMeta>,
        name: String,
        args: Vec<(String, Type)>,
        return_type: Type,
    ) -> Self {
        let mut variables = HashMap::new();
        let mut arg_names = vec![];
        for (name, ty) in args {
            variables.insert(name.clone(), ty);
            arg_names.push(name);
        }
        Self {
            rule_meta,
            name,
            is_transition_function: false,

            top_level_statements: vec![],
            statements: vec![],
            expressions: vec![],
            error_points: vec![],

            arg_names,
            variables,
            return_type,
        }
    }
    pub fn build_helper_function(
        rule_meta: &Rc<RuleMeta>,
        helper_func: parser::HelperFunc,
    ) -> LangResult<Self> {
        let mut ret = Self::new_helper_function(
            rule_meta.clone(),
            helper_func.name.inner,
            helper_func
                .args
                .into_iter()
                .map(|arg| (arg.inner.1.inner, arg.inner.0.inner.resolve(rule_meta.ndim)))
                .collect(),
            helper_func.return_type.inner.resolve(rule_meta.ndim),
        );
        ret.build_top_level_statement_block_ast(&helper_func.body.inner)?;
        Ok(ret)
    }

    /// Returns the metadata associated with the rule that this function is a
    /// member of.
    pub fn rule_meta(&self) -> &Rc<RuleMeta> {
        &self.rule_meta
    }
    /// Returns the name of this function.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the names of the arguments to this function.
    pub fn arg_names(&self) -> &[String] {
        &self.arg_names
    }
    /// Returns the return type of this function.
    pub fn return_type(&self) -> Type {
        self.return_type
    }

    /// Returns the type of an existing variable with the given name, or an
    /// Err(UseOfUninitializedVariable) if it does not exist.
    pub fn try_get_var(&self, span: Span, var_name: &str) -> LangResult<Type> {
        self.variables
            .get(var_name)
            .copied()
            .ok_or_else(|| UseOfUninitializedVariable.with_span(span))
    }
    /// Returns the type of the variable with the given name, creating it with
    /// the given type if it does not already exist.
    pub fn get_or_create_var(&mut self, var_name: &str, new_ty: Type) -> Type {
        if let Some(existing_type) = self.variables.get(var_name) {
            *existing_type
        } else {
            self.variables.insert(var_name.to_owned(), new_ty);
            new_ty
        }
    }

    /// Constructs AST nodes for statements in a block from a parse tree and
    /// adds those AST nodes to a list of top-level statements (i.e. statements
    /// that are not inside a loop or conditional block).
    pub fn build_top_level_statement_block_ast(
        &mut self,
        parser_statements: &parser::StatementBlock,
    ) -> LangResult<()> {
        self.top_level_statements = self.build_statement_block_ast(parser_statements)?;
        Ok(())
    }
    /// Constructs AST nodes for statements in a block from a parse tree.
    pub fn build_statement_block_ast(
        &mut self,
        parser_statements: &parser::StatementBlock,
    ) -> LangResult<StatementBlock> {
        let mut block = vec![];
        for parser_statement in parser_statements {
            let span = parser_statement.span;

            let new_statement: Box<dyn Statement> = match &parser_statement.inner {
                // Assertion
                parser::Statement::Assert { expr, msg } => {
                    let expr = self.build_expression_ast(&expr)?;
                    let msg = msg
                        .as_ref()
                        .map(|string_lit| string_lit.inner.contents.to_owned());
                    Box::new(statements::Assert::try_new(span, self, expr, msg)?)
                }
                // Error
                parser::Statement::Error { msg } => {
                    let msg = msg
                        .as_ref()
                        .map(|string_lit| string_lit.inner.contents.to_owned());
                    Box::new(statements::Error::try_new(span, self, msg)?)
                }
                // Variable assignment statement
                parser::Statement::SetVar {
                    var_expr,
                    assign_op,
                    value_expr,
                } => {
                    // Handle assignments with operators (e.g. `x += 3`).
                    let value_expr = match assign_op.op() {
                        Some(op) => self.build_expression_ast(&Spanned {
                            span,
                            inner: parser::Expr::BinaryOp {
                                lhs: Box::new(var_expr.clone()),
                                op,
                                rhs: Box::new(value_expr.clone()),
                            },
                        })?,
                        None => self.build_expression_ast(&value_expr)?,
                    };
                    // Register a new variable if necessary.
                    if let parser::Expr::Ident(var_name) = &var_expr.inner {
                        self.get_or_create_var(var_name, self[value_expr].return_type());
                    }
                    // Construct the statement.
                    let var_expr = self.build_expression_ast(var_expr)?;
                    Box::new(statements::SetVar::try_new(
                        span, self, var_expr, value_expr,
                    )?)
                }
                // If statement
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
                // Become statement (In a transition function, `become` should be used, not `return`.)
                parser::Statement::Become(ret_expr) => {
                    if self.is_transition_function {
                        let ret_expr = self.build_expression_ast(ret_expr)?;
                        Box::new(statements::Return::try_new(span, self, ret_expr)?)
                    } else {
                        Err(BecomeInHelperFunction.with_span(span))?
                    }
                }
                // Return statement (In a helper function, `return` should be used, not `become`.)
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
    /// Constructs the AST nodes for a list of expressions.
    pub fn build_expression_list_ast(
        &mut self,
        parser_exprs: &[Spanned<parser::Expr>],
    ) -> LangResult<Vec<ExprRef>> {
        parser_exprs
            .iter()
            .map(|e| self.build_expression_ast(e))
            .collect()
    }
    /// Constructs an AST node for an expression from a parse tree.
    pub fn build_expression_ast(
        &mut self,
        parser_expr: &Spanned<parser::Expr>,
    ) -> LangResult<ExprRef> {
        let span = parser_expr.span;
        let args: Args;
        let func: functions::FuncConstructor;

        match &parser_expr.inner {
            // Integer literal
            parser::Expr::Int(i) => {
                args = Args::none();
                func = functions::literals::Int::with_value(*i);
            }
            // Identifier (variable)
            parser::Expr::Ident(s) => {
                args = Args::none();
                func = Box::new(functions::misc::GetVar::with_name(s.to_owned()));
            }
            // String literal
            parser::Expr::String(_) => {
                args = Args::none();
                func = Err(Unimplemented)?;
            }
            // Vector literal
            parser::Expr::Vector(exprs) => {
                args = Args::from(
                    exprs
                        .iter()
                        .map(|expr| self.build_expression_ast(expr))
                        .collect::<LangResult<Vec<_>>>()?,
                );
                func = Box::new(functions::vectors::Build::construct);
            }
            // Parenthetical group
            parser::Expr::ParenExpr(expr) => return self.build_expression_ast(expr),
            // Unary operator
            parser::Expr::UnaryOp { op, operand } => {
                let arg = self.build_expression_ast(operand)?;
                args = Args::from(vec![arg]);
                func = functions::lookup_unary_operator(*op, self[arg].return_type(), span)?;
            }
            // Binary mathematical/bitwise operator
            parser::Expr::BinaryOp { lhs, op, rhs } => {
                let lhs = self.build_expression_ast(lhs)?;
                let rhs = self.build_expression_ast(rhs)?;
                args = Args::from(vec![lhs, rhs]);
                func = functions::lookup_binary_operator(
                    self[lhs].return_type(),
                    *op,
                    self[rhs].return_type(),
                    span,
                )?;
            }
            // Logical NOT
            parser::Expr::LogicalNot(expr) => {
                args = Args::from(vec![self.build_expression_ast(expr)?]);
                func = Box::new(functions::logic::LogicalNot::construct);
            }
            // Binary logical operator
            parser::Expr::LogicalOr { lhs, rhs }
            | parser::Expr::LogicalXor { lhs, rhs }
            | parser::Expr::LogicalAnd { lhs, rhs } => {
                let lhs = self.build_expression_ast(lhs)?;
                let rhs = self.build_expression_ast(rhs)?;
                args = Args::from(vec![lhs, rhs]);
                func = functions::logic::LogicalBinaryOp::with_op(match &parser_expr.inner {
                    parser::Expr::LogicalOr { .. } => functions::logic::LogicalBinOpType::Or,
                    parser::Expr::LogicalXor { .. } => functions::logic::LogicalBinOpType::Xor,
                    parser::Expr::LogicalAnd { .. } => functions::logic::LogicalBinOpType::And,
                    _ => unreachable!(),
                })
            }
            // Comparison
            parser::Expr::Cmp { exprs, cmps } => {
                args = Args::from(self.build_expression_list_ast(exprs)?);
                func = functions::cmp::Cmp::with_comparisons(cmps.clone());
            }
            // Attribute access
            parser::Expr::GetAttr { object, attribute } => {
                args = Args::from(vec![self.build_expression_ast(object)?]);
                func = functions::lookup_method(self[args[0]].return_type(), &attribute.inner)
                    .ok_or_else(|| FunctionLookupError.with_span(attribute.span))?;
            }
            // Function call
            parser::Expr::FnCall {
                func: func_expr,
                args: arg_exprs,
            } => {
                let mut args_list = self.build_expression_list_ast(arg_exprs)?;
                func = match &func_expr.inner {
                    parser::Expr::Ident(func_name) => {
                        if self
                            .rule_meta()
                            .helper_function_signatures
                            .contains_key(func_name)
                        {
                            // User-defined function
                            functions::misc::CallUserFn::with_name(func_name.to_owned())
                        } else {
                            // Built-in function
                            functions::lookup_function(func_name)
                                .ok_or_else(|| FunctionLookupError.with_span(func_expr.span))?
                        }
                    }
                    parser::Expr::GetAttr {
                        object,
                        attribute: method_name,
                    } => {
                        args_list.insert(0, self.build_expression_ast(object)?);
                        // Built-in method
                        let ty = self[args_list[0]].return_type();
                        functions::lookup_method(ty, &method_name.inner)
                            .ok_or_else(|| FunctionLookupError.with_span(method_name.span))?
                    }
                    _ => Err(Expected("function name").with_span(func_expr.span))?,
                };
                args = Args::from(args_list);
            }
            parser::Expr::Index {
                object,
                args: index_args,
            } => {
                let mut args_list = vec![self.build_expression_ast(object)?];
                args_list.extend(self.build_expression_list_ast(index_args)?);
                args = Args::from(args_list);
                func = match self[args[0]].return_type() {
                    Type::Vector(_) => functions::vectors::Access::with_component_idx(None),
                    ty => Err(CannotIndexType(ty))?,
                }
            }
        };

        let expr = Expr::try_new(self, span, args, func)?;
        Ok(self.add_expr(expr))
    }

    /// Adds a statement AST node to this user function, and returns a
    /// StatementRef representing it.
    fn add_statement(&mut self, statement: Box<dyn Statement>) -> StatementRef {
        let idx = self.statements.len();
        self.statements.push(statement);
        StatementRef(idx)
    }
    /// Adds an expression AST node to this user function, and returns an
    /// ExprRef representing it.
    fn add_expr(&mut self, expr: Expr) -> ExprRef {
        let idx = self.expressions.len();
        self.expressions.push(expr);
        ExprRef(idx)
    }
    /// Adds an error point to this user function, and returns an ErrorPointRef
    /// representing it.
    pub fn add_error_point(&mut self, error: LangError) -> ErrorPointRef {
        let idx = self.error_points.len();
        self.error_points.push(error.clone());
        ErrorPointRef { idx, error }
    }

    /// JIT compiles this function and returns an executable function.
    pub fn compile(&self, compiler: &mut Compiler) -> LangResult<CompiledFunction> {
        compiler.begin_extern_function(
            &self.name,
            self.return_type(),
            &self.arg_names,
            &self.variables,
        )?;

        // Compile the statements.
        self.compile_statement_block(compiler, &self.top_level_statements)?;

        if compiler.needs_terminator() {
            // If necessary, add an implicit `return #0` at the end of the
            // transition function. TODO: change this to `remain` once that's
            // implemented, and handle other types as well.
            let default_return_value = compiler.get_default_var_value(self.return_type()).unwrap();
            compiler.build_return_ok(default_return_value)?;
        }
        CompiledFunction::try_new(
            self.rule_meta.source_code.clone(),
            self.error_points.clone(),
            compiler,
        )
    }

    /// Compiles a block of statements into LLVM IR, stopping if a terminator
    /// (e.g. `return`) is reached.
    pub fn compile_statement_block(
        &self,
        compiler: &mut Compiler,
        block: &StatementBlock,
    ) -> LangResult<()> {
        for &statement in block {
            if !compiler.needs_terminator() {
                // The function has already returned.
                return Ok(());
            }
            self.compile_statement(compiler, statement)?;
        }
        Ok(())
    }
    /// Compiles a statement into LLVM IR by calling Statement::compile().
    pub fn compile_statement(
        &self,
        compiler: &mut Compiler,
        statement: StatementRef,
    ) -> LangResult<()> {
        self[statement].compile(compiler, self)
    }
    /// Compiles an expression into LLVM IR by calling Expr::compile().
    pub fn compile_expr(&self, compiler: &mut Compiler, expr: ExprRef) -> LangResult<Value> {
        self[expr].compile(compiler, self)
    }
    /// Evaluates an expression as a constant, returning an
    /// Err(CannotEvalAsConst) if the expression cannot be evaluated at compile
    /// time.
    pub fn const_eval_expr(&self, expr: ExprRef) -> LangResult<ConstValue> {
        self[expr].const_eval(self)
    }
}

/// A newtype of usize that refers to an expression AST node of a user function.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprRef(usize);
impl Index<ExprRef> for UserFunction {
    type Output = Expr;
    fn index(&self, expr_ref: ExprRef) -> &Expr {
        &self.expressions[expr_ref.0]
    }
}

/// A newtype of usize that refers to a statement AST node of a user function.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct StatementRef(usize);
impl Index<StatementRef> for UserFunction {
    type Output = Box<dyn Statement>;
    fn index(&self, statement_ref: StatementRef) -> &Box<dyn Statement> {
        &self.statements[statement_ref.0]
    }
}

/// A reference to an error point of a user function (a possible runtime error).
#[derive(Debug, Clone)]
pub struct ErrorPointRef {
    idx: usize,
    error: LangError,
}
impl ErrorPointRef {
    /// Compiles LLVM IR that returns this error.
    pub fn compile(&self, compiler: &mut Compiler) {
        compiler.build_return_err(self.idx);
    }
    /// Returns the LangError that this refers to.
    pub fn error(&self) -> LangError {
        self.error.clone()
    }
    /// Returns a LangResult::Err of this error.
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
