use std::convert::TryFrom;

use crate::lexer::{Keyword, Token};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AssignOp {
    NoOp,

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
impl TryFrom<Token> for AssignOp {
    type Error = ();

    fn try_from(token: Token) -> std::result::Result<Self, Self::Error> {
        match token {
            Token::Assign => Ok(Self::NoOp),

            Token::AssignPlus => Ok(Self::Add),
            Token::AssignMinus => Ok(Self::Sub),
            Token::AssignAsterisk => Ok(Self::Mul),
            Token::AssignSlash => Ok(Self::Div),
            Token::AssignPercent => Ok(Self::Mod),
            Token::AssignDoubleAsterisk => Ok(Self::Pow),
            Token::AssignDoubleLessThan => Ok(Self::Shl),
            Token::AssignDoubleGreaterThan => Ok(Self::ShrSigned),
            Token::AssignTripleGreaterThan => Ok(Self::ShrUnsigned),
            Token::AssignAmpersand => Ok(Self::And),
            Token::AssignPipe => Ok(Self::Or),
            Token::AssignCaret => Ok(Self::Xor),

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
impl From<AssignOp> for Option<BinaryOp> {
    fn from(op: AssignOp) -> Self {
        match op {
            AssignOp::NoOp => None,

            AssignOp::Add => Some(BinaryOp::Add),
            AssignOp::Sub => Some(BinaryOp::Sub),
            AssignOp::Mul => Some(BinaryOp::Mul),
            AssignOp::Div => Some(BinaryOp::Div),
            AssignOp::Mod => Some(BinaryOp::Mod),
            AssignOp::Pow => Some(BinaryOp::Pow),

            AssignOp::Shl => Some(BinaryOp::Shl),
            AssignOp::ShrSigned => Some(BinaryOp::ShrSigned),
            AssignOp::ShrUnsigned => Some(BinaryOp::ShrUnsigned),

            AssignOp::And => Some(BinaryOp::And),
            AssignOp::Or => Some(BinaryOp::Or),
            AssignOp::Xor => Some(BinaryOp::Xor),
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
