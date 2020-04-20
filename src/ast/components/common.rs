use std::convert::TryFrom;
use std::fmt;
use std::str::FromStr;

/// A binary (two-input) operator.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Op {
    /// Mathematical operator.
    Math(MathOp),
    /// Bitshift operator.
    Shift(ShiftOp),
    /// Bitwise operator.
    Bitwise(BitOp),
    /// Logical operator.
    Logic(LogicOp),
}
impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Math(op) => write!(f, "{}", op),
            Self::Shift(op) => write!(f, "{}", op),
            Self::Bitwise(op) => write!(f, "{}", op),
            Self::Logic(op) => write!(f, "{}", op),
        }
    }
}
impl FromStr for Op {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        if let Ok(math_op) = s.parse() {
            Ok(Self::Math(math_op))
        } else if let Ok(shift_op) = s.parse() {
            Ok(Self::Shift(shift_op))
        } else if let Ok(bitwise_op) = s.parse() {
            Ok(Self::Bitwise(bitwise_op))
        } else if let Ok(logic_op) = s.parse() {
            Ok(Self::Logic(logic_op))
        } else {
            Err(())
        }
    }
}

enum_with_str_repr! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum MathOp {
        /// Addition.
        Add = "+",
        /// Subtraction.
        Sub = "-",
        /// Multiplication.
        Mul = "*",
        /// Division.
        Div = "/",
        /// Remainder.
        Rem = "%",
        /// Exponentiation.
        Exp = "**",
    }

    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum ShiftOp {
        /// Shift left.
        Shl = "<<",
        /// Shift right (arithmetic/signed).
        Shr = ">>",
        /// Shift right (logical/unsigned).
        Srl = ">>>",
    }

   #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum BitOp {
        /// Bitwise AND.
        And = "&",
        /// Bitwise OR.
        Or = "|",
        /// Bitwise XOR.
        Xor = "^",
    }

    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum LogicOp {
        /// Logical AND.
        And = "and",
        /// Logical OR.
        Or = "or",
        /// Logical XOR.
        Xor = "xor",
    }

    /// A comparison operator.
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum Cmp {
        /// Equal.
        Eql = "==",
        /// Not equal.
        Neq = "!=",
        /// Less than.
        Lt = "<",
        /// Greater than.
        Gt = ">",
        /// Less than or equal.
        Lte = "<=",
        /// Greater than or equal.
        Gte = ">=",
    }

    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum EqCmp {
        /// Equal.
        Eql = "==",
        /// Not equal.
        Neq = "!=",
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
