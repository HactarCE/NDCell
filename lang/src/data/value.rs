use codemap::{Span, Spanned};
use itertools::Itertools;

use super::{CpVal, GetType, RtVal, Type};
use crate::errors::{Error, Result};
use crate::llvm;

/// Well-defined value that may be constant or compiled variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Val {
    /// Compile-time constant value.
    Rt(RtVal),
    /// Compile-time variable value.
    Cp(CpVal),
}
impl From<RtVal> for Val {
    fn from(v: RtVal) -> Self {
        Self::Rt(v)
    }
}
impl From<CpVal> for Val {
    fn from(v: CpVal) -> Self {
        Self::Cp(v)
    }
}
impl GetType for Val {
    fn ty(&self) -> Type {
        match self {
            Val::Rt(v) => v.ty(),
            Val::Cp(v) => v.ty(),
        }
    }
}
impl Val {
    /// Returns whether this is an `RtVal`.
    pub fn is_rt_val(&self) -> bool {
        matches!(self, Self::Rt(_))
    }

    /// Returns the value inside if this is an `RtVal`; otherwise returns
    /// `None`.
    pub fn rt_val(self) -> Option<RtVal> {
        match self {
            Self::Rt(v) => Some(v),
            _ => None,
        }
    }
    /// Returns the value inside if this is an `CpVal`; otherwise returns
    /// `None`.
    pub fn cp_val(self) -> Option<CpVal> {
        match self {
            Self::Cp(v) => Some(v),
            _ => None,
        }
    }

    /// Returns a reference to the value inside if this is an `RtVal`; otherwise
    /// returns `None`.
    pub fn as_rt_val(&self) -> Option<&RtVal> {
        match self {
            Val::Rt(v) => Some(&v),
            _ => None,
        }
    }
    /// Returns a reference to the value inside if this is an `CpVal`; otherwise
    /// returns `None`.
    pub fn as_cp_val(&self) -> Option<&CpVal> {
        match self {
            Val::Cp(v) => Some(&v),
            _ => None,
        }
    }
}

/// Well-defined value that may be constant or compiled variable and has a span.
#[derive(Debug, Clone)]
pub enum SpannedVal {
    /// Compile-time constant value.
    Rt(Spanned<RtVal>),
    /// Compile-time variable value.
    Cp(Spanned<CpVal>),
}
impl From<Spanned<Val>> for SpannedVal {
    fn from(v: Spanned<Val>) -> Self {
        let span = v.span;
        match v.node {
            Val::Rt(node) => Self::Rt(Spanned { span, node }),
            Val::Cp(node) => Self::Cp(Spanned { span, node }),
        }
    }
}

pub trait SpannedValExt {
    fn try_rt_val(&self) -> Result<Spanned<RtVal>>;
}
impl SpannedValExt for Spanned<Val> {
    fn try_rt_val(&self) -> Result<Spanned<RtVal>> {
        match &self.node {
            Val::Rt(v) => {
                let span = self.span;
                let node = v.clone();
                Ok(Spanned { span, node })
            }
            Val::Cp(_) => Err(Error::must_be_constant(self.span)),
        }
    }
}
