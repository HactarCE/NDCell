use std::fmt;

use super::span::Spanned;
use super::vars::VarMapping;

pub type Instructions = Vec<Spanned<Instruction>>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Instruction {
    /// Pop a value off the stack and assign it to a variable.
    VarAssign(usize),
    /// Fetch a variable value and push it onto the stack.
    VarFetch(usize),
    /// Push a constant integer onto the stack.
    PushInt(i64),
    /// Pop an integer off the stack and get the state with that value.
    GetStateFromInt,
    /// Add two integers; pop two integers a and b off the stack and push b + a
    /// onto the stack.
    AddInt,
    /// Subtract two integers; pop two integers a and b off the stack and push b
    /// - a onto the stack.
    SubInt,
    /// Negate an integer; pop an integer a off the stack and push -a onto the
    /// stack.
    NegInt,
    /// Pop a value off the stack, use that as the next cell state, and halt
    /// execution.
    Become,
    /// Pop a value off the stack and return it.
    Return,
}

#[derive(Debug)]
pub struct Function {
    pub instructions: Instructions,
    pub vars: VarMapping,
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "    Variables [")?;
        for id in 0..self.vars.len() {
            writeln!(
                f,
                "  {:<7} {:<15?} {}",
                id,
                self.vars.get_type(id),
                self.vars.get_name(id)
            )?;
        }
        writeln!(f, "    ]")?;
        writeln!(f, "    Instructions [")?;
        for instruction in &self.instructions {
            writeln!(
                f,
                "      {:<6}  {:?}",
                instruction.span.end, instruction.inner
            )?;
        }
        write!(f, "    ]")?;
        Ok(())
    }
}
