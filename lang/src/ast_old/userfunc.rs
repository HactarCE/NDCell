use std::collections::HashMap;
use std::ops::Index;
use std::sync::{mpsc, Arc};

use super::{expression, statement, Expr, Statement, StatementBlock};
use crate::compiler::{const_int, CompiledFunction, Compiler, Value};
use crate::errors::*;
use crate::parser;
use crate::{ConstValue, RuleMeta, Span, Spanned, Type};
use ErrorKind::{InternalError, UseOfUninitializedVariable};

/// A user-defined function node in the AST.
#[derive(Debug, Default)]
pub struct UserFunction {
    /// Metadata of the rule that this user function is part of.
    rule_meta: Arc<RuleMeta>,
    /// Name of this function.
    name: Arc<String>,
    /// Kind of this function (including return type).
    kind: UserFunctionKind,

    /// HashMap of variable types (including arguments), indexed by name.
    variables: HashMap<String, Type>,
    /// List of variable names for arguments.
    arg_names: Vec<Arc<String>>,

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
    pub fn new_transition_function(rule_meta: Arc<RuleMeta>) -> Self {
        let nbhd_type = Type::Pattern {
            shape: rule_meta.nbhd_shape.clone(),
            has_lut: rule_meta.has_cell_state_luts,
        };
        let mut ret = Self::new_helper_function(
            rule_meta,
            Arc::new("transition".to_owned()),
            vec![(Arc::new("neighborhood".to_owned()), nbhd_type.clone())],
            Type::CellState,
        );
        ret.kind = UserFunctionKind::Transition;
        ret.variables.insert("nbhd".to_string(), nbhd_type);
        ret.variables.insert("this".to_string(), Type::CellState);
        ret
    }
    /// Constructs a new helper function that returns the given type.
    pub fn new_helper_function(
        rule_meta: Arc<RuleMeta>,
        name: Arc<String>,
        args: Vec<(Arc<String>, Type)>,
        return_type: Type,
    ) -> Self {
        let mut variables = HashMap::new();
        let mut arg_names = vec![];
        for (name, ty) in args {
            variables.insert(String::clone(&*name), ty);
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
        rule_meta: &Arc<RuleMeta>,
        helper_func: parser::HelperFunc,
    ) -> LangResult<Self> {
        let mut ret = Self::new_helper_function(
            rule_meta.clone(),
            helper_func.name.inner,
            helper_func
                .params
                .into_iter()
                .map(|arg| (arg.inner.1.inner, arg.inner.0.inner.resolve(rule_meta)))
                .collect(),
            helper_func.return_type.inner.resolve(rule_meta),
        );
        ret.build_top_level_statement_block_ast(&helper_func.body.inner)?;
        Ok(ret)
    }

    /// Returns the metadata associated with the rule that this function is a
    /// member of.
    pub fn rule_meta(&self) -> &Arc<RuleMeta> {
        &self.rule_meta
    }
    /// Returns the name of this function.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the names of the parameters of this function.
    pub fn arg_names(&self) -> &[Arc<String>] {
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
        if var_name == crate::THROWAWAY_VARIABLE {
            return Type::Void;
        }
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
                statement::from_parse_tree(self, parser_statement)
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
        expression::from_parse_tree(self, parser_expr).map(|expr| self.add_expr(expr))
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
    ///
    /// **This function blocks** until all instances of the CompiledFunction
    /// have been dropped.
    pub fn compile(
        &self,
        mpsc_sender: &mpsc::Sender<LangResult<CompiledFunction>>,
        mut compiler: Compiler,
    ) -> LangResult<()> {
        compiler.begin_extern_function(
            &self.name,
            self.kind().return_type(),
            &self.arg_names,
            &self.variables,
        )?;

        // Assign variables "nbhd" and "this" if this is a transition function.
        match self.kind() {
            UserFunctionKind::Helper(_) => (),
            UserFunctionKind::Transition => {
                let nbhd_value = compiler.build_var_load("neighborhood")?;
                compiler.build_var_store("nbhd", &nbhd_value)?;
                if self
                    .rule_meta()
                    .nbhd_shape
                    .contains_pos(&vec![0; self.rule_meta().ndim])
                {
                    let origin =
                        compiler.build_construct_vector(&vec![const_int(0); self.rule_meta().ndim]);
                    let this_value = compiler.build_get_pattern_cell_state_unchecked(
                        &nbhd_value.as_pattern()?,
                        origin,
                    )?;
                    compiler.build_var_store("this", &Value::CellState(this_value))?;
                }
            }
        }

        // Compile the statements.
        self.compile_statement_block(&mut compiler, &self.top_level_statements)?;

        let default_return_value = match self.kind() {
            UserFunctionKind::Helper(_) => {
                // Implicitly return a default value at the end of a helper
                // function.
                compiler.default_var_value(&self.kind().return_type())?
            }
            UserFunctionKind::Transition => {
                // Implicitly return `this` at the end of the transition
                // function.
                compiler.build_var_load("this")?
            }
        };
        compiler.build_return_ok(default_return_value)?;

        CompiledFunction::send_new(
            mpsc_sender,
            self.rule_meta.clone(),
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

/// Kind of user function (including return type, if there is one).
#[derive(Debug, Clone, PartialEq, Eq)]
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
