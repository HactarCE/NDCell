use std::collections::HashMap;

mod value;
pub use value::Value;

use super::types::{LangCellState, LangInt, Type};
use super::{ast, errors::*, Spanned, CELL_STATE_COUNT};
use LangErrorMsg::{
    CellStateOutOfRange, DivideByZero, IntegerOverflow, InternalError, Unimplemented,
};

pub enum ExecuteResult {
    Continue,
    Return(Option<Value>),
}
impl ExecuteResult {
    pub fn return_value(self) -> Option<Value> {
        match self {
            Self::Continue => None,
            Self::Return(value) => value,
        }
    }
}

#[derive(Debug)]
pub struct State {
    /// List of instructions to execute. Branching instructions (If, ForLoop,
    /// WhileLoop, etc.) are flattened before execution by replacing their body
    /// with a single Goto statement and moving the instructions that were there
    /// to the end of this instruction list.
    pub instructions: ast::StatementBlock,
    /// Index of instruction to execute next.
    pub instruction_pointer: usize,
    /// Variables.
    pub vars: HashMap<String, Value>,
    pub function_type: ast::FunctionType,
}
impl State {
    pub fn new(mut function: ast::Function) -> LangResult<Self> {
        ast::flatten_block(&mut function.statements);
        let mut vars = HashMap::new();
        for (var_name, var_type) in function.vars {
            vars.insert(
                var_name,
                Value::from_type(var_type).ok_or_else(|| {
                    InternalError("Invalid variable type not caught by type checker".into())
                        .without_span()
                })?,
            );
        }
        Ok(Self {
            instructions: function.statements,
            instruction_pointer: 0,
            vars,
            function_type: function.fn_type,
        })
    }

    pub fn step(&mut self) -> LangResult<ExecuteResult> {
        use ast::Statement::*;
        if let Some(instruction) = self.instructions.get(self.instruction_pointer) {
            match &instruction.inner {
                SetVar {
                    var_name,
                    value_expr,
                } => {
                    if self.vars[&var_name.inner].get_type() != value_expr.get_type() {
                        Err(InternalError(
                            "Invalid variable assignment not caught by type checker".into(),
                        ))?
                    }
                    let var_value = match value_expr {
                        ast::Expr::Int(e) => Value::Int(self.eval_int_expr(e)?.inner),
                        ast::Expr::CellState(e) => {
                            Value::CellState(self.eval_cell_state_expr(e)?.inner)
                        }
                    };
                    *self.vars.get_mut(&var_name.inner).unwrap() = var_value;
                }
                If {
                    cond_expr,
                    if_true,
                    if_false,
                } => {
                    // Evaluate the condition.
                    let condition: bool = self.eval_int_expr(cond_expr)?.inner != 0;
                    // Decide which block to execute.
                    let block = if condition { if_true } else { if_false };
                    // Jump there.
                    Self::goto_block(&mut self.instruction_pointer, block)?;
                }
                Return(return_expr) => match self.function_type {
                    ast::FunctionType::Transition => {
                        if Type::CellState != return_expr.get_type() {
                            Err(InternalError(
                                "Invalid return statement not caught by type checker".into(),
                            )
                            .without_span())?;
                        }
                        let return_value = Value::CellState(
                            self.eval_cell_state_expr(return_expr.cell_state_ref()?)?
                                .inner
                                .into(),
                        );
                        return Ok(ExecuteResult::Return(Some(return_value)));
                    }
                    ast::FunctionType::Helper(_) => Err(Unimplemented.with_span(return_expr))?,
                },
                End => return Ok(ExecuteResult::Return(None)),
                Goto(idx) => self.instruction_pointer = *idx,
            }
            self.instruction_pointer += 1;
            Ok(ExecuteResult::Continue)
        } else {
            Ok(ExecuteResult::Return(None))
        }
    }

    fn goto_block(instruction_pointer: &mut usize, block: &ast::StatementBlock) -> LangResult<()> {
        match block.get(0).map(|s| &s.inner) {
            Some(ast::Statement::Goto(idx)) => *instruction_pointer = *idx,
            None => (),
            _ => Err(
                InternalError("Block in interpreter did not contain GOTO statement".into())
                    .without_span(),
            )?,
        }
        Ok(())
    }

    pub fn eval_int_expr(
        &self,
        expression: &Spanned<ast::IntExpr>,
    ) -> LangResult<Spanned<LangInt>> {
        let span = expression.span;
        use ast::IntExpr::*;
        Ok(Spanned {
            span,
            inner: match &expression.inner {
                FnCall(_) => Err(Unimplemented.with_span(span))?,
                Var(var_name) => self.vars[var_name].int()?,
                Literal(i) => *i,
                Op { lhs, op, rhs } => {
                    let lhs = self.eval_int_expr(&lhs)?.inner;
                    let rhs = self.eval_int_expr(&rhs)?.inner;
                    match op {
                        ast::Op::Math(math_op) => {
                            use ast::MathOp::*;
                            // Check for division by zero.
                            if (*math_op == Div || *math_op == Rem) && rhs == 0 {
                                Err(DivideByZero.with_span(span))?;
                            }
                            // Do the operation, checking for overflow.
                            match math_op {
                                Add => lhs.checked_add(rhs),
                                Sub => lhs.checked_sub(rhs),
                                Mul => lhs.checked_mul(rhs),
                                Div => lhs.checked_div(rhs),
                                Rem => lhs.checked_rem(rhs),
                                Exp => Err(Unimplemented.with_span(span))?,
                            }
                            .ok_or_else(|| IntegerOverflow.with_span(span))?
                        }
                        _ => Err(Unimplemented.with_span(span))?,
                    }
                }
                Neg(x) => self
                    .eval_int_expr(x)?
                    .inner
                    .checked_neg()
                    .ok_or_else(|| IntegerOverflow.with_span(span))?,
                CmpInt(cmp_expr) => {
                    self.eval_multi_comparison_any(cmp_expr, Self::eval_int_expr)?
                }
                CmpCellState(cmp_expr) => {
                    self.eval_multi_comparison_eq(cmp_expr, Self::eval_cell_state_expr)?
                }
            },
        })
    }

    pub fn eval_cell_state_expr(
        &self,
        expression: &Spanned<ast::CellStateExpr>,
    ) -> LangResult<Spanned<LangCellState>> {
        let span = expression.span;
        use ast::CellStateExpr::*;
        Ok(Spanned {
            span,
            inner: match &expression.inner {
                FnCall(_) => Err(Unimplemented.with_span(span))?,
                Var(var_name) => self.vars[var_name].cell_state()?,
                FromId(id_expr) => {
                    // Convert integer to cell state, checking whether it is
                    // within range.
                    let id_value = self.eval_int_expr(id_expr)?.inner;
                    if id_value >= 0 && (id_value as usize) < CELL_STATE_COUNT {
                        id_value as LangCellState
                    } else {
                        Err(CellStateOutOfRange.with_span(span))?
                    }
                }
            },
        })
    }

    fn eval_multi_comparison_eq<ExprType, ValueType: PartialEq<ValueType>>(
        &self,
        cmp_expr: &ast::CmpExpr<ExprType, ast::EqCmp>,
        eval_expr_fn: impl FnMut(&Self, &Spanned<ExprType>) -> LangResult<Spanned<ValueType>>,
    ) -> LangResult<LangInt> {
        self.eval_multi_comparison_generic(cmp_expr, eval_expr_fn, |_, cmp, x, y| match cmp {
            ast::EqCmp::Eql => x == y,
            ast::EqCmp::Neq => x != y,
        })
    }
    fn eval_multi_comparison_any<ExprType, ValueType: PartialOrd<ValueType>>(
        &self,
        cmp_expr: &ast::CmpExpr<ExprType, ast::Cmp>,
        eval_expr_fn: impl FnMut(&Self, &Spanned<ExprType>) -> LangResult<Spanned<ValueType>>,
    ) -> LangResult<LangInt> {
        self.eval_multi_comparison_generic(cmp_expr, eval_expr_fn, |_, cmp, x, y| match cmp {
            ast::Cmp::Eql => x == y,
            ast::Cmp::Neq => x != y,
            ast::Cmp::Lt => x < y,
            ast::Cmp::Gt => x > y,
            ast::Cmp::Lte => x <= y,
            ast::Cmp::Gte => x >= y,
        })
    }
    fn eval_multi_comparison_generic<ExprType, ValueType, CmpType: Copy>(
        &self,
        cmp_expr: &ast::CmpExpr<ExprType, CmpType>,
        mut eval_expr_fn: impl FnMut(&Self, &Spanned<ExprType>) -> LangResult<Spanned<ValueType>>,
        mut eval_compare_fn: impl FnMut(&Self, CmpType, &ValueType, &ValueType) -> bool,
    ) -> LangResult<LangInt> {
        let expr1 = &cmp_expr.initial;
        let mut lhs = eval_expr_fn(self, expr1)?.inner;
        for (comparison, expr2) in &cmp_expr.comparisons {
            let rhs = eval_expr_fn(self, expr2)?.inner;
            let compare_result = eval_compare_fn(self, *comparison, &lhs, &rhs);
            // If the condition is false, return 0 immediately. If it is true,
            // continue on to check the next condition.
            if !compare_result {
                return Ok(0);
            }
            // The current RHS will be the next condition's LHS.
            lhs = rhs;
        }
        // Every condition was true, so return 1.
        Ok(1)
    }
}
