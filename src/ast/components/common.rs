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
