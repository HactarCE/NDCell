use codemap::{Span, Spanned};
use itertools::Itertools;

use crate::data::{CpVal, GetType, RtVal, Type, Val};
use crate::errors::{Error, Result};

pub type VarResult = std::result::Result<Val, VarError>;

/// Variable value.
#[derive(Debug, Clone)]
pub struct Var {
    /// Variable value.
    pub(super) value: VarResult,
    /// Initial variable type (if still relevant).
    pub(super) initial_type: Option<Type>,
    /// All assignments to this variable that contribute to its current value.
    pub(super) assign_spans: Vec<(Span, Type)>,
    /// If this variable holds a placeholder value from the top of a loop, the
    /// index of that loop from the bottom of the loop stack.
    pub(super) loop_placeholder_index: Option<usize>,
}
impl Default for Var {
    fn default() -> Self {
        Self {
            value: Err(VarError::Undefined),
            initial_type: None,
            assign_spans: vec![],
            loop_placeholder_index: None,
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
