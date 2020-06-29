use std::collections::HashMap;
use std::ops::Index;
use std::rc::Rc;

use super::{expressions, statements, Expr, RuleMeta, Statement, StatementBlock};
use crate::compiler::{CompiledFunction, Compiler, Value};
use crate::errors::*;
use crate::parser;
use crate::{ConstValue, Span, Spanned, Type};
use LangErrorMsg::{InternalError, UseOfUninitializedVariable};

#[derive(Debug, Clone, PartialEq, Eq)]
/// Kind of user function (including return type, if there is one).
pub enum UserFunctionKind {
    /// Helper function returning a specific type.
    ///
    /// `return` statements are only allowed in helper functions.
    Helper(Type),
    /// Transition function, which returns a cell state.
    ///
    /// `become` and `remain` statements are only allowed in transition functions.
    Transition,
}
impl Default for UserFunctionKind {
    fn default() -> Self {
        Self::Helper(Type::default())
    }
}
impl UserFunctionKind {
    /// Returns the type that this user function returns.
    pub fn return_type(&self) -> Type {
        match self {
            Self::Helper(ty) => ty.clone(),
            Self::Transition => Type::CellState,
        }
    }
}

/// A user-defined function node in the AST.
#[derive(Debug, Default)]
pub struct UserFunction {
    /// Metadata of the rule that this user function is part of.
    rule_meta: Rc<RuleMeta>,
    /// Name of this function.
    name: String,
    /// Kind of this function (including return type).
    kind: UserFunctionKind,

    /// HashMap of variable types (including arguments), indexed by name.
    variables: HashMap<String, Type>,
    /// List of variable names for arguments.
    arg_names: Vec<String>,

    /// Top-level statement block, consisting of StatementRefs to self.statements.
    top_level_statements: StatementBlock,
    /// List of every statement AST node.
    statements: Vec<Box<dyn Statement>>,
    /// List of every expression AST node.
    expressions: Vec<Expr>,
    /// List of every possible runtime error.
    error_points: Vec<LangError>,
}
impl UserFunction {
    /// Constructs a new transition function.
    pub fn new_transition_function(rule_meta: Rc<RuleMeta>) -> Self {
        Self {
            kind: UserFunctionKind::Transition,
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
            kind: UserFunctionKind::Helper(return_type),

            arg_names,
            variables,

            top_level_statements: vec![],
            statements: vec![],
            expressions: vec![],
            error_points: vec![
                // Error index 0 is reserved for internal errors.
                InternalError("Something went wrong at runtime, which is really bad".into())
                    .without_span(),
            ],
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
    /// Returns the kind of this function.
    pub fn kind(&self) -> &UserFunctionKind {
        &self.kind
    }

    /// Returns the type of an existing variable with the given name, or an
    /// Err(UseOfUninitializedVariable) if it does not exist.
    pub fn try_get_var(&self, span: Span, var_name: &str) -> LangResult<&Type> {
        self.variables
            .get(var_name)
            .ok_or_else(|| UseOfUninitializedVariable.with_span(span))
    }
    /// Returns the type of the variable with the given name, creating it with
    /// the given type if it does not already exist.
    pub fn get_or_create_var(&mut self, var_name: &str, new_ty: Type) -> Type {
        if let Some(existing_type) = self.variables.get(var_name) {
            existing_type.clone()
        } else {
            self.variables.insert(var_name.to_owned(), new_ty.clone());
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
        parser_statements
            .iter()
            .map(|parser_statement| {
                statements::from_parse_tree(self, parser_statement)
                    .map(|statement| self.add_statement(statement))
            })
            .collect()
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
        expressions::from_parse_tree(self, parser_expr).map(|expr| self.add_expr(expr))
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
    pub fn compile(&self, mut compiler: Compiler) -> LangResult<CompiledFunction> {
        compiler.begin_extern_function(
            &self.name,
            self.kind().return_type(),
            &self.arg_names,
            &self.variables,
        )?;

        // Compile the statements.
        self.compile_statement_block(&mut compiler, &self.top_level_statements)?;

        if compiler.needs_terminator() {
            // If necessary, add an implicit `return #0` at the end of the
            // transition function. TODO: change this to `remain` once that's
            // implemented, and handle other types as well.
            let default_return_value = compiler
                .get_default_var_value(&self.kind().return_type())
                .unwrap();
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
