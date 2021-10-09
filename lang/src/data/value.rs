use codemap::Spanned;

use super::CpVal;
use super::RtVal;
use super::Type;
use crate::errors::{Error, Result};

/// Value that may be constant and/or compiled variable.
#[derive(Debug, Clone)]
pub enum Val {
    /// Runtime value or compile-time constant.
    Rt(RtVal),
    /// Compile-time variable value.
    Cp(CpVal),
    /// Unknown value with potentially-known type.
    Unknown(Option<Type>),
    /// Possibly uninitialized.
    MaybeUninit,
    /// Depends on some expression that encountered an error that has already
    /// been reported.
    Error,
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
impl Val {
    /// Returns whether this is an `RtVal`.
    pub fn is_rt_val(&self) -> bool {
        matches!(self, Self::Rt(_))
    }

    /// Returns the value inside if this is an `RtVal`; otherwise returns
    /// `None`.
    pub fn rt_val(self) -> Option<RtVal> {
        if let Self::Rt(v) = self {
            Some(v)
        } else {
            None
        }
    }
    /// Returns the value inside if this is an `CpVal`; otherwise returns
    /// `None`.
    pub fn cp_val(self) -> Option<CpVal> {
        if let Self::Cp(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// NDCA value that may have a defined type.
pub trait TryGetType {
    /// Returns the type of the value, if it definitely has one.
    fn try_get_type(&self) -> Result<Type>;
}
impl TryGetType for Spanned<Val> {
    fn try_get_type(&self) -> Result<Type> {
        match &self.node {
            Val::Rt(v) => Ok(v.ty()),
            Val::Cp(v) => Ok(v.ty()),
            Val::Unknown(Some(ty)) => Ok(ty.clone()),
            Val::Unknown(None) => Err(Error::ambiguous_variable_type(self.span)),
            Val::MaybeUninit => Err(Error::maybe_uninitialized_variable(self.span)),
            Val::Error => Err(Error::AlreadyReported),
        }
    }
}
impl TryGetType for RtVal {
    fn try_get_type(&self) -> Result<Type> {
        Ok(self.ty())
    }
}
impl TryGetType for CpVal {
    fn try_get_type(&self) -> Result<Type> {
        Ok(self.ty())
    }
}
impl<T: TryGetType> TryGetType for Spanned<T> {
    fn try_get_type(&self) -> Result<Type> {
        self.node.try_get_type()
    }
}
