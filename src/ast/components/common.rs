use std::convert::TryFrom;

use super::super::tokens::ComparisonToken;

pub const DISPLAY_INDENT: usize = 2;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Op {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Remainder
    Rem,
}
impl Op {
    pub fn get_symbol(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Rem => "%",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum EqCmp {
    /// == Equal
    Eql,
    /// != Not equal
    Neq,
}
impl EqCmp {
    pub fn get_symbol(self) -> &'static str {
        match self {
            Self::Eql => "==",
            Self::Neq => "!=",
        }
    }
}
impl TryFrom<Cmp> for EqCmp {
    type Error = ();
    fn try_from(cmp: Cmp) -> Result<Self, ()> {
        match cmp {
            Cmp::Eql => Ok(Self::Eql),
            Cmp::Neq => Ok(Self::Neq),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Cmp {
    /// == Equal
    Eql,
    /// != Not equal
    Neq,
    /// <  Less than
    Lt,
    /// >  Greater than
    Gt,
    /// <= Less than or equal
    Lte,
    /// >= Greater than or equal
    Gte,
}
impl Cmp {
    pub fn get_symbol(self) -> &'static str {
        match self {
            Self::Eql => "==",
            Self::Neq => "!=",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Lte => "<=",
            Self::Gte => ">=",
        }
    }
}
impl From<EqCmp> for Cmp {
    fn from(eq_cmp: EqCmp) -> Self {
        match eq_cmp {
            EqCmp::Eql => Self::Eql,
            EqCmp::Neq => Self::Neq,
        }
    }
}
impl From<ComparisonToken> for Cmp {
    fn from(token_class: ComparisonToken) -> Self {
        use ComparisonToken::*;
        match token_class {
            Eql => Self::Eql,
            Neq => Self::Neq,
            Lt => Self::Lt,
            Gt => Self::Gt,
            Lte => Self::Lte,
            Gte => Self::Gte,
        }
    }
}
