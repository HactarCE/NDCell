use codemap::{Span, Spanned};
use itertools::Itertools;

use super::{CpVal, GetType, RtVal, Type};
use crate::errors::{Error, Result};

pub type VarResult = std::result::Result<Val, VarError>;

/// Variable value.
///
/// TODO: these fields probably shouldn't be public; move into compiler/mod.rs,
/// since that's the only file that needs to see them.
#[derive(Debug, Clone)]
pub struct Var {
    /// Variable value.
    pub value: VarResult,
    /// Initial variable type (if still relevant).
    pub initial_type: Option<Type>,
    /// All assignments to this variable that contribute to its current value.
    pub assign_spans: Vec<(Span, Type)>,
}
impl Default for Var {
    fn default() -> Self {
        Self {
            value: Err(VarError::Undefined),
            initial_type: None,
            assign_spans: vec![],
        }
    }
}
impl Var {
    pub fn try_ty(&self) -> Option<Type> {
        match &self.value {
            Ok(v) => Some(v.ty()),
            Err(VarError::NonConstValue(ty)) => Some(ty.clone()),
            Err(_) => None,
        }
    }
    pub fn get_value(&mut self, span: Span) -> Result<Val> {
        self.value.clone().map_err(|e| match e {
            VarError::Undefined => Error::uninitialized_variable(span),
            VarError::MaybeUninit => Error::maybe_uninitialized_variable(span),
            VarError::NonConstValue(ty) => {
                let mut e = Error::unknown_variable_value(span, &ty);
                for (span, _ty) in &self.assign_spans {
                    e = e.with_note(*span, format!("assigned a value"));
                }
                e
            }
            VarError::AmbiguousType => {
                let mut e = match &self.initial_type {
                    Some(ty) => Error::ambiguous_variable_type_with_initial(span, &ty),
                    None => Error::ambiguous_variable_type(span),
                };
                for (span, ty) in &self.assign_spans {
                    e = e.with_note(*span, format!("assigned a value of type {}", ty));
                }
                e
            }
            VarError::AlreadyReported => Error::AlreadyReported,
        })
    }
}

/// Reason for a variable not having a well-defined value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarError {
    /// Variable has never been initialized.
    Undefined,
    /// Variable might not be initialized.
    MaybeUninit,
    /// Cannot determine value of variable, and values of this type must be
    /// compile-time constants.
    NonConstValue(Type),
    /// Cannot determine type of variable.
    AmbiguousType,
    /// Error already reported.
    AlreadyReported,
}
impl VarError {
    /// Merges two errors, returning the most severe.
    pub fn merge(self, other: Self) -> Self {
        use VarError::*;

        if self == other {
            return self;
        }

        match (self, other) {
            (AlreadyReported, _) | (_, AlreadyReported) => AlreadyReported,
            (MaybeUninit, _) | (_, MaybeUninit) => MaybeUninit,
            (Undefined, _) | (_, Undefined) => MaybeUninit,
            (AmbiguousType, _) | (_, AmbiguousType) => AmbiguousType,
            (NonConstValue(_), NonConstValue(_)) => AmbiguousType, // types not equal
        }
    }
}

/// Well-defined value that may be constant or compiled variable.
#[derive(Debug, Clone)]
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
