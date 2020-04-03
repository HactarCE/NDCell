use std::collections::HashMap;

mod value;
pub use value::Value;

use super::types::LangCellState;
use super::{ast, errors::*, Spanned, CELL_STATE_COUNT};
use LangErrorMsg::{
    CellStateOutOfRange, IntegerOverflowDuringNegation, IntegerOverflowDuringSubtraction,
    UseOfUninitializedVariable,
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
            match &instruction.inner {
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
                    if x > 0 && (x as usize) < CELL_STATE_COUNT {
                        x as LangCellState
                    } else {
                        Err(CellStateOutOfRange.with_span(span))?
                    }
                }),
                Neg(expr) => Value::Int(-self.eval(expr)?.as_int()?),
                Add(expr1, expr2) => {
                    // Add two integers, checking for overflow.
                    let lhs = self.eval(expr1)?.as_int()?;
                    let rhs = self.eval(expr2)?.as_int()?;
                    Value::Int(
                        lhs.checked_add(rhs)
                            .ok_or_else(|| IntegerOverflowDuringNegation.with_span(span))?,
                    )
                }
                Sub(expr1, expr2) => {
                    // Subtract two integers, checking for overflow.
                    let lhs = self.eval(expr1)?.as_int()?;
                    let rhs = self.eval(expr2)?.as_int()?;
                    Value::Int(
                        lhs.checked_sub(rhs)
                            .ok_or_else(|| IntegerOverflowDuringSubtraction.with_span(span))?,
                    )
                }
                Comparison(expr1, comparisons) => unimplemented!(),
                Var(name) => self
                    .vars
                    .get(name)
                    .ok_or_else(|| UseOfUninitializedVariable.with_span(span))?
                    .clone(),
            },
        })
    }
}
