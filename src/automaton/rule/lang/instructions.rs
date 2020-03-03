use super::vars::Type;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Instruction {
    VarAssign(Type, usize),
    VarFetch(Type, usize),
    PushInt(i32),
    AddInt,
}
