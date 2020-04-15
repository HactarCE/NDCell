use std::collections::HashMap;

mod value;
pub use value::Value;

use super::types::LangCellState;
use super::{ast, errors::*, Span, Spanned, CELL_STATE_COUNT};
use LangErrorMsg::{
    CellStateOutOfRange, ComparisonError, DivideByZero, Expected, IntegerOverflow, InternalError,
    TypeError, UseOfUninitializedVariable,
};

pub enum ExecuteResult {
    Continue,
    Return(Value),
}
impl ExecuteResult {
    pub fn return_value(self) -> Option<Value> {
        match self {
            Self::Continue => None,
            Self::Return(value) => Some(value),
        }
    }
}

#[derive(Debug)]
pub struct State {
    /// List of instructions to execute. As branching instructions (If, ForLoop,
    /// WhileLoop, etc.) are encountered, they are flattened by replacing their
    /// body with a single Goto statement and their instructions are copied to
    /// the end of this instruction list.
    pub instructions: ast::StatementBlock,
    /// Index of instruction to execute next.
    pub instruction_pointer: usize,
    /// Variables.
    pub vars: HashMap<String, Value>,
}
impl State {
    pub fn new(mut instructions: ast::StatementBlock) -> Self {
        ast::flatten_block(&mut instructions);
        Self {
            instructions,
            instruction_pointer: 0,
            vars: HashMap::default(),
        }
    }

    pub fn step(&mut self) -> LangResult<ExecuteResult> {
        use ast::Statement::*;
        if let Some(instruction) = self.instructions.get(self.instruction_pointer) {
            println!("exec {}", instruction.inner);
            match &instruction.inner {
                SetVar(var_expr, expr) => {
                    if let ast::Expr::Var(var_name) = &var_expr.inner {
                        let new_value = self.eval(expr)?.inner;
                        if let Some(old_value) = self.vars.get_mut(var_name) {
                            let old_type = old_value.get_type();
                            let new_type = new_value.get_type();
                            if old_type == new_type {
                                *old_value = new_value;
                            } else {
                                Err(TypeError {
                                    expected: old_type,
                                    got: new_type,
                                })?;
                            }
                        } else {
                            self.vars.insert(var_name.clone(), new_value);
                        }
                    } else {
                        Err(Expected("variable").with_span(var_expr))?;
                    }
                }
                If(expr, if_true, maybe_if_false) => {
                    if self.eval(expr)?.as_int()? != 0 {
                        self.instruction_pointer = Self::get_goto_idx_of_block(if_true)?;
                    } else if let Some(if_false) = maybe_if_false {
                        self.instruction_pointer = Self::get_goto_idx_of_block(if_false)?;
                    }
                }
                Become(expr) | Return(expr) => {
                    return Ok(ExecuteResult::Return(self.eval(expr)?.inner))
                }
                End => return Ok(ExecuteResult::Return(Value::Null)),
                Goto(idx) => self.instruction_pointer = *idx,
            }
            self.instruction_pointer += 1;
            Ok(ExecuteResult::Continue)
        } else {
            Ok(ExecuteResult::Return(Value::Null))
        }
    }

    fn get_goto_idx_of_block(block: &ast::StatementBlock) -> LangResult<usize> {
        if let Some(ast::Statement::Goto(idx)) = block.get(0).map(|s| &s.inner) {
            Ok(*idx)
        } else {
            Err(
                InternalError("Block in interpreter did not contain GOTO statement".into())
                    .without_span(),
            )
        }
    }

    pub fn eval(&self, expression: &Spanned<ast::Expr>) -> LangResult<Spanned<Value>> {
        use ast::Expr::*;
        let span = expression.span;
        Ok(Spanned {
            span,
            inner: match &expression.inner {
                Int(i) => Value::Int(*i),
                Tag(expr) => Value::CellState({
                    // Convert integer to cell state, checking whether it is
                    // within range.
                    let x = self.eval(expr)?.as_int()?;
                    if x >= 0 && (x as usize) < CELL_STATE_COUNT {
                        x as LangCellState
                    } else {
                        Err(CellStateOutOfRange.with_span(span))?
                    }
                }),
                Neg(expr) => Value::Int(
                    self.eval(expr)?
                        .as_int()?
                        .checked_neg()
                        .ok_or_else(|| IntegerOverflow.with_span(span))?,
                ),
                Op(expr1, op, expr2) => {
                    let lhs = self.eval(expr1)?.as_int()?;
                    let rhs = self.eval(expr2)?.as_int()?;
                    use ast::Op::*;
                    Value::Int({
                        // Check for division by zero.
                        if (*op == Div || *op == Rem) && rhs == 0 {
                            Err(DivideByZero.with_span(span))?;
                        }
                        // Do the operation, checking for overflow.
                        match op {
                            Add => lhs.checked_add(rhs),
                            Sub => lhs.checked_sub(rhs),
                            Mul => lhs.checked_mul(rhs),
                            Div => lhs.checked_div(rhs),
                            Rem => lhs.checked_rem(rhs),
                        }
                        .ok_or_else(|| IntegerOverflow.with_span(span))?
                    })
                }
                Comparison(expr1, comparisons) => {
                    let mut result = true;
                    let mut lhs = self.eval(expr1)?;
                    for (comparison, expr2) in comparisons {
                        let rhs = self.eval(expr2)?;
                        if !Self::compare(*comparison, &lhs, &rhs)? {
                            result = false;
                        }
                        // The current RHS will be the next condition's LHS.
                        lhs = rhs;
                    }
                    Value::Int(if result { 1 } else { 0 })
                }
                Var(name) => self
                    .vars
                    .get(name)
                    .ok_or_else(|| UseOfUninitializedVariable.with_span(span))?
                    .clone(),
            },
        })
    }
    fn compare(
        comparison: ast::Comparison,
        lhs: &Spanned<Value>,
        rhs: &Spanned<Value>,
    ) -> LangResult<bool> {
        use ast::Comparison::*;
        use Value::*;
        Ok(match (&lhs.inner, &rhs.inner, comparison) {
            // Integer comparison
            (Int(a), Int(b), Equal) => a == b,
            (Int(a), Int(b), NotEqual) => a != b,
            (Int(a), Int(b), LessThan) => a < b,
            (Int(a), Int(b), GreaterThan) => a > b,
            (Int(a), Int(b), LessThanOrEqual) => a <= b,
            (Int(a), Int(b), GreaterThanOrEqual) => a >= b,
            // Cell state comparison
            (CellState(a), CellState(b), Equal) => a == b,
            (CellState(a), CellState(b), NotEqual) => a != b,
            // Error
            _ => Err(ComparisonError {
                cmp_sym: comparison.get_symbol(),
                lhs: lhs.inner.get_type(),
                rhs: rhs.inner.get_type(),
            }
            .with_span(Span::merge(lhs, rhs)))?,
        })
    }
}
