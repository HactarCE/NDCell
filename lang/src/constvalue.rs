//! Values used by the interpreter for NDCA.

use std::convert::TryInto;

use crate::errors::*;
use crate::types::{LangCellState, LangInt, Type};

/// Constant value of any type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstValue {
    /// Integer
    Int(LangInt),
    /// Cell state
    CellState(LangCellState),
    /// Vector of a specific length from 1 to 256 (extra components are zero).
    Vector(Vec<LangInt>),
}
impl ConstValue {
    /// Returns the type of this value.
    pub fn ty(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
            Self::Vector(values) => Type::Vector(values.len()),
        }
    }
    /// Constructs a default value of the given type.
    pub fn default(ty: &Type) -> Option<Self> {
        match ty {
            Type::Int => Some(Self::Int(0)),
            Type::CellState => Some(Self::CellState(0)),
            Type::Vector(len) => Some(Self::Vector(vec![0; *len])),
            Type::Pattern(_) => None,
        }
    }
    /// Returns the integer value inside if this is a ConstValue::Int; otherwise
    /// returns an InternalError.
    pub fn as_int(self) -> LangResult<LangInt> {
        match self {
            Self::Int(i) => Ok(i),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Returns the integer value inside if this is a ConstValue::CellState;
    /// otherwise returns an InternalError.
    pub fn as_cell_state(self) -> LangResult<LangCellState> {
        match self {
            Self::CellState(i) => Ok(i),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Returns the vector value inside if this is a ConstValue::Vector;
    /// otherwise returns an InternalError.
    pub fn as_vector(self) -> LangResult<Vec<LangInt>> {
        match self {
            Self::Vector(v) => Ok(v),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Converts this value to a boolean if it can be converted; otherwise
    /// returns an InternalError.
    pub fn to_bool(self) -> LangResult<bool> {
        match self {
            Self::Int(i) => Ok(i != 0),
            Self::CellState(i) => Ok(i != 0),
            Self::Vector(v) => Ok(v.into_iter().any(|i| i != 0)),
        }
    }
    /// Converts this value to a vector of the specified length if this is a
    /// ConstValue::Int or ConstValue::Vector; otherwise returns an
    /// InternalError.
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
    pub fn from_bytes(ty: &Type, bytes: &[u8]) -> Self {
        // Check that this type can be represented in raw bytes.
        assert!(
            ty.has_runtime_representation(),
            "Cannot construct {:?} from bytes",
            ty
        );
        // Check the number of bytes.
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
            Type::Pattern(_) => todo!("construct pattern from bytes"),
        }
    }
    /// Returns raw bytes representing this value. Panics if this type has no
    /// runtime representation.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut ret = vec![0; self.ty().size_of().unwrap()];
        let mut ret_slice = ret.as_mut_slice();
        self.set_bytes(&mut ret_slice);
        return ret;
    }
    /// Writes raw bytes representing this value to an array slice. Panics if
    /// this type has no runtime representation, or if the array slice is the
    /// wrong length.
    pub fn set_bytes(&self, bytes: &mut [u8]) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constvalue_int_bytes() {
        let int_iter = vec![-10, -5, -1, 0, 1, 5, 10]
            .into_iter()
            .map(ConstValue::Int);
        let cell_state_iter = vec![0, 1, 5, 10, 254, 255]
            .into_iter()
            .map(ConstValue::CellState);
        let vector_iter = vec![
            vec![],
            vec![10],
            vec![10, -20],
            vec![-10, 20, -30],
            vec![10, -20, 30, -40],
            vec![-10, 20, -30, 40, -50, 60, -70, 80, -90, 100, -110],
        ]
        .into_iter()
        .map(ConstValue::Vector);
        for const_value in int_iter.chain(cell_state_iter).chain(vector_iter) {
            let bytes = const_value.to_bytes();
            assert_eq!(
                const_value,
                ConstValue::from_bytes(&const_value.ty(), &bytes)
            );
        }
    }
}
