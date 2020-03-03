use super::span::Spanned;

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
    /// Negate an integer; pop an integer a off the stack and push -a onto the stack.
    NegInt,
    /// Pop a value off the stack and use that as the return value.
    Become,
    /// Pop a value off the stack and return it.
    Return,
}
