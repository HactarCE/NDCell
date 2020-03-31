use std::collections::HashMap;

mod value;
pub use value::{Type, Value};

use super::{ast, errors::*, Spanned};

pub enum ExecuteResult {
    Continue,
    Return(Value),
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
    pub fn new(instructions: ast::StatementBlock) -> Self {
        Self {
            instructions,
            instruction_pointer: 0,
            vars: HashMap::default(),
        }
    }
    fn step(&mut self) -> LangResult<ExecuteResult> {
        use ast::Statement::*;
        match &self.instructions[self.instruction_pointer].inner {
            Become(expr) | Return(expr) => {
                return Ok(ExecuteResult::Return(self.eval(expr)?.inner))
            }
            End => return Ok(ExecuteResult::Return(Value::Null)),
            Goto(idx) => self.instruction_pointer = *idx,
        }
        self.instruction_pointer += 1;
        return Ok(ExecuteResult::Continue);
    }
    fn eval(&self, expression: &Spanned<ast::Expr>) -> LangResult<Spanned<Value>> {
        use ast::Expr::*;
        let span = expression.span;
        Ok(Spanned {
            span,
            inner: match &expression.inner {
                Int(i) => Value::Int(*i),
                Tag(expr) => Value::CellState(self.eval(expr)?.as_int()?),
                Neg(expr) => Value::Int(-self.eval(expr)?.as_int()?),
                Add(expr1, expr2) => {
                    Value::Int(self.eval(expr1)?.as_int()? + self.eval(expr2)?.as_int()?)
                }
                Sub(expr1, expr2) => {
                    Value::Int(self.eval(expr1)?.as_int()? - self.eval(expr2)?.as_int()?)
                }
                Var(name) => self
                    .vars
                    .get(name)
                    .ok_or_else(|| {
                        lang_error(
                            span,
                            format!("Tried to access uninitialized variable '{}'", name),
                        )
                    })?
                    .clone(),
            },
        })
    }
}
