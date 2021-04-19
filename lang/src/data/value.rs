use codemap::Spanned;

use super::CpVal;
use super::RtVal;
use super::Type;
use crate::errors::{AlreadyReported, Error, Fallible, Result};

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
    Err(AlreadyReported),
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

/// NDCA value that may have a defined type.
pub trait FallibleTypeOf {
    /// Returns the type of the value, if it definitely has one.
    fn fallible_ty(&self) -> Fallible<Result<Type>>;
}
impl FallibleTypeOf for Spanned<Val> {
    fn fallible_ty(&self) -> Fallible<Result<Type>> {
        match &self.node {
            Val::Rt(v) => Ok(Ok(v.ty())),
            Val::Cp(v) => Ok(Ok(v.ty())),
            Val::Unknown(Some(ty)) => Ok(Ok(ty.clone())),
            Val::Unknown(None) => Ok(Err(Error::ambiguous_variable_type(self.span))),
            Val::MaybeUninit => Ok(Err(Error::maybe_uninitialized_variable(self.span))),
            Val::Err(AlreadyReported) => Err(AlreadyReported),
        }
    }
}
impl FallibleTypeOf for RtVal {
    fn fallible_ty(&self) -> Fallible<Result<Type>> {
        Ok(Ok(self.ty()))
    }
}
impl FallibleTypeOf for CpVal {
    fn fallible_ty(&self) -> Fallible<Result<Type>> {
        Ok(Ok(self.ty()))
    }
}
