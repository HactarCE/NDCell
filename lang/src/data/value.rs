use super::CpVal;
use super::RtVal;
use super::Type;
use crate::errors::{AlreadyReported, Fallible};

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
