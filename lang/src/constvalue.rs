//! Values used by the interpreter for NDCA.

use std::convert::TryInto;

use crate::errors::*;
use crate::types::{LangCellState, LangInt, Type};
use LangErrorMsg::InternalError;

/// InternalError reported when a variable is used improperly and it was not
/// caught by the type checker.
const INTERNAL_VAR_USE_ERROR: LangError = InternalError(std::borrow::Cow::Borrowed(
    "Invalid variable use not caught by type checker",
))
.without_span();

/// Constant value of any type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstValue {
    /// Integer
    Int(LangInt),
    /// Cell state
    CellState(LangCellState),
    /// Vector of a specific length from 1 to 256 (extra components are zero).
    Vector(Vec<LangInt>),
    // Pattern(crate::automaton::ArrayView2D<u8>),
}
impl ConstValue {
    /// Returns the type of this value.
    pub fn ty(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
            Self::Vector(values) => Type::Vector(values.len()),
            // Self::Pattern(_) => Type::Pattern,
        }
    }
    /// Constructs a default value of the given type.
    pub fn default(ty: Type) -> Option<Self> {
        match ty {
            Type::Int => Some(Self::Int(0)),
            Type::CellState => Some(Self::CellState(0)),
            Type::Vector(len) => Some(Self::Vector(vec![0; len])),
        }
    }
    /// Returns the integer value inside if this is a ConstValue::Int; otherwise a
    /// TypeError.
    pub fn as_int(self) -> LangResult<LangInt> {
        match self {
            Self::Int(i) => Ok(i),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Returns the integer value inside if this is a ConstValue::CellState;
    /// otherwise a TypeError.
    pub fn as_cell_state(self) -> LangResult<LangCellState> {
        match self {
            Self::CellState(i) => Ok(i),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Converts this value to a vector of the specified length if this is a
    /// ConstValue::Int or ConstValue::Vector; otherwise returns a TypeError.
    pub fn coerce_to_vector(self, len: usize) -> LangResult<Vec<LangInt>> {
        match self {
            Self::Int(i) => Ok(vec![i; len]),
            Self::Vector(mut v) => {
                if v.len() < len {
                    // Not long enough; extend with zeros.
                    v.extend(std::iter::repeat(0).take(len - v.len()));
                } else if v.len() > len {
                    // Too long; truncate.
                    v.truncate(len);
                }
                Ok(v)
            }
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Constructs a value of the given type from raw bytes. Panics if given an
    /// invalid value or invalid type.
    pub fn from_bytes(ty: Type, bytes: &[u8]) -> Self {
        // TODO: fuzz/test this method
        // assert!(
        //     ty.has_runtime_representation(),
        //     "Cannot construct {:?} from bytes",
        //     ty
        // );
        assert_eq!(
            ty.size_of(),
            Some(bytes.len()),
            "Wrong number of bytes to construct ConstValue; {:?} requires {:?} bytes, but got {:?}",
            ty,
            ty.size_of(),
            bytes.len()
        );
        match ty {
            Type::Int => Self::Int(LangInt::from_ne_bytes(bytes.try_into().unwrap())),
            Type::CellState => {
                Self::CellState(LangCellState::from_ne_bytes(bytes.try_into().unwrap()))
            }
            Type::Vector(_) => Self::Vector(
                bytes
                    .chunks(Type::Int.size_of().unwrap())
                    .map(TryInto::try_into)
                    .map(Result::unwrap)
                    .map(LangInt::from_ne_bytes)
                    .collect(),
            ),
        }
    }
    /// Returns raw bytes representing this value. Panics if this type has no
    /// runtime representation.
    pub fn to_bytes(&self) -> Vec<u8> {
        // TODO: fuzz/test this method
        let mut ret = vec![0; self.ty().size_of().unwrap()];
        let mut ret_slice = ret.as_mut_slice();
        self.set_bytes(&mut ret_slice);
        return ret;
    }
    /// Writes raw bytes representing this value to an array slice. Panics if
    /// this type has no runtime representation, or if the array slice is the
    /// wrong length.
    pub fn set_bytes(&self, bytes: &mut [u8]) {
        // TODO: fuzz/test this method

        // Check that this type can be represented in raw bytes.
        assert!(
            self.ty().has_runtime_representation(),
            "Cannot convert {:?} to bytes",
            self.ty()
        );
        // Check the number of bytes.
        assert_eq!(
            bytes.len(),
            self.ty().size_of().unwrap(),
            "{:?} requires {} bytes; got {}",
            self.ty(),
            self.ty().size_of().unwrap(),
            bytes.len()
        );
        match self {
            Self::Int(i) => {
                const SIZE: usize = std::mem::size_of::<LangInt>();
                let bytes: &mut [u8; SIZE] = bytes.try_into().unwrap();
                *bytes = i.to_ne_bytes()
            }
            Self::CellState(i) => {
                const SIZE: usize = std::mem::size_of::<LangCellState>();
                let bytes: &mut [u8; SIZE] = bytes.try_into().unwrap();
                *bytes = i.to_ne_bytes()
            }
            Self::Vector(values) => {
                for (chunk, &i) in bytes.chunks_mut(Type::Int.size_of().unwrap()).zip(values) {
                    Self::Int(i).set_bytes(chunk);
                }
            }
        }
    }
}
