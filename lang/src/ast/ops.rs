use std::convert::TryFrom;
use std::fmt;

use crate::lexer::{Keyword, Token};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AssignOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    Shl,
    ShrSigned,
    ShrUnsigned,

    And,
    Or,
    Xor,
}
impl TryFrom<Token> for Option<AssignOp> {
    type Error = ();

    fn try_from(token: Token) -> std::result::Result<Self, Self::Error> {
        match token {
            Token::Assign => Ok(None),

            Token::AssignPlus => Ok(Some(AssignOp::Add)),
            Token::AssignMinus => Ok(Some(AssignOp::Sub)),
            Token::AssignAsterisk => Ok(Some(AssignOp::Mul)),
            Token::AssignSlash => Ok(Some(AssignOp::Div)),
            Token::AssignPercent => Ok(Some(AssignOp::Mod)),
            Token::AssignDoubleAsterisk => Ok(Some(AssignOp::Pow)),
            Token::AssignDoubleLessThan => Ok(Some(AssignOp::Shl)),
            Token::AssignDoubleGreaterThan => Ok(Some(AssignOp::ShrSigned)),
            Token::AssignTripleGreaterThan => Ok(Some(AssignOp::ShrUnsigned)),
            Token::AssignAmpersand => Ok(Some(AssignOp::And)),
            Token::AssignPipe => Ok(Some(AssignOp::Or)),
            Token::AssignCaret => Ok(Some(AssignOp::Xor)),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    Shl,
    ShrSigned,
    ShrUnsigned,

    And,
    Or,
    Xor,

    LogicalAnd,
    LogicalOr,
    LogicalXor,

    Range,

    Is,
}
impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::Pow => write!(f, "**"),

            Self::Shl => write!(f, "<<"),
            Self::ShrSigned => write!(f, ">>"),
            Self::ShrUnsigned => write!(f, ">>>"),

            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
            Self::Xor => write!(f, "^"),

            Self::LogicalAnd => write!(f, "and"),
            Self::LogicalOr => write!(f, "or"),
            Self::LogicalXor => write!(f, "xor"),

            Self::Range => write!(f, ".."),

            Self::Is => write!(f, "is"),
        }
    }
}
impl From<AssignOp> for BinaryOp {
    fn from(op: AssignOp) -> Self {
        match op {
            AssignOp::Add => BinaryOp::Add,
            AssignOp::Sub => BinaryOp::Sub,
            AssignOp::Mul => BinaryOp::Mul,
            AssignOp::Div => BinaryOp::Div,
            AssignOp::Mod => BinaryOp::Mod,
            AssignOp::Pow => BinaryOp::Pow,

            AssignOp::Shl => BinaryOp::Shl,
            AssignOp::ShrSigned => BinaryOp::ShrSigned,
            AssignOp::ShrUnsigned => BinaryOp::ShrUnsigned,

            AssignOp::And => BinaryOp::And,
            AssignOp::Or => BinaryOp::Or,
            AssignOp::Xor => BinaryOp::Xor,
        }
    }
}
impl TryFrom<Token> for BinaryOp {
    type Error = ();

    fn try_from(token: Token) -> std::result::Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(Self::Add),
            Token::Minus => Ok(Self::Sub),
            Token::Asterisk => Ok(Self::Mul),
            Token::Slash => Ok(Self::Div),
            Token::Percent => Ok(Self::Mod),
            Token::DoubleAsterisk => Ok(Self::Pow),
            Token::DoubleLessThan => Ok(Self::Shl),
            Token::DoubleGreaterThan => Ok(Self::ShrSigned),
            Token::TripleGreaterThan => Ok(Self::ShrUnsigned),
            Token::Ampersand => Ok(Self::And),
            Token::Pipe => Ok(Self::Or),
            Token::Caret => Ok(Self::Xor),

            Token::DotDot => Ok(Self::Range),

            Token::Keyword(Keyword::Or) => Ok(Self::LogicalOr),
            Token::Keyword(Keyword::Xor) => Ok(Self::LogicalXor),
            Token::Keyword(Keyword::And) => Ok(Self::LogicalAnd),
            Token::Keyword(Keyword::Is) => Ok(Self::Is),

            _ => Err(()),
        }
    }
}
impl BinaryOp {
    pub fn function_name(self) -> &'static str {
        match self {
            BinaryOp::Add => "'+'",
            BinaryOp::Sub => "'-'",
            BinaryOp::Mul => "'*'",
            BinaryOp::Div => "'/'",
            BinaryOp::Mod => "'%'",
            BinaryOp::Pow => "'**'",
            BinaryOp::Shl => "'<<'",
            BinaryOp::ShrSigned => "'>>'",
            BinaryOp::ShrUnsigned => "'>>>'",
            BinaryOp::And => "'&'",
            BinaryOp::Or => "'|'",
            BinaryOp::Xor => "'^'",
            BinaryOp::LogicalAnd => "'and'",
            BinaryOp::LogicalOr => "'or'",
            BinaryOp::LogicalXor => "'xor'",
            BinaryOp::Range => "'..'",
            BinaryOp::Is => "'is'",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PrefixOp {
    Pos,
    Neg,

    BitwiseNot,
    LogicalNot,

    IntToCell,
}
impl fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrefixOp::Pos => write!(f, "+"),
            PrefixOp::Neg => write!(f, "-"),

            PrefixOp::BitwiseNot => write!(f, "~"),
            PrefixOp::LogicalNot => write!(f, "not"),

            PrefixOp::IntToCell => write!(f, "#"),
        }
    }
}
impl TryFrom<Token> for PrefixOp {
    type Error = ();

    fn try_from(token: Token) -> std::result::Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(Self::Pos),
            Token::Minus => Ok(Self::Neg),

            Token::Tilde => Ok(Self::BitwiseNot),
            Token::Keyword(Keyword::Not) => Ok(Self::LogicalNot),

            Token::Octothorpe => Ok(Self::IntToCell),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CompareOp {
    /// Equal `==`.
    Eql,
    /// Not equal `!=`.
    Neq,
    /// Less than `<`.
    Lt,
    /// Greater than `>`.
    Gt,
    /// Less than or equal `<=`.
    Lte,
    /// Greater than or equal `>=`.
    Gte,
}
impl fmt::Display for CompareOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompareOp::Eql => write!(f, "=="),
            CompareOp::Neq => write!(f, "!="),
            CompareOp::Lt => write!(f, "<"),
            CompareOp::Gt => write!(f, ">"),
            CompareOp::Lte => write!(f, "<="),
            CompareOp::Gte => write!(f, ">="),
        }
    }
}
impl TryFrom<Token> for CompareOp {
    type Error = ();

    fn try_from(token: Token) -> std::result::Result<Self, Self::Error> {
        match token {
            Token::Eql => Ok(Self::Eql),
            Token::Neq => Ok(Self::Neq),
            Token::Lt => Ok(Self::Lt),
            Token::Gt => Ok(Self::Gt),
            Token::Lte => Ok(Self::Lte),
            Token::Gte => Ok(Self::Gte),
            _ => Err(()),
        }
    }
}
impl CompareOp {
    pub fn eval<L, R>(self, lhs: L, rhs: R) -> bool
    where
        L: PartialOrd<R>,
    {
        match self {
            Self::Eql => lhs == rhs,
            Self::Neq => lhs != rhs,
            Self::Lt => lhs < rhs,
            Self::Gt => lhs > rhs,
            Self::Lte => lhs <= rhs,
            Self::Gte => lhs >= rhs,
        }
    }
}
