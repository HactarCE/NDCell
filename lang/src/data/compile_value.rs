use codemap::Spanned;
use std::sync::Arc;

use super::{GetType, LlvmCellArray, Type, VectorSet};
use crate::errors::{Error, Result};
use crate::llvm;

/// Compile-time variable value of any type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CpVal {
    Integer(llvm::IntValue),
    Cell(llvm::IntValue),
    Vector(llvm::VectorValue),
    CellArray(LlvmCellArray),
    CellArrayMut(LlvmCellArray),
    CellSet(llvm::VectorValue),
}
impl GetType for CpVal {
    fn ty(&self) -> Type {
        match self {
            CpVal::Integer(_) => Type::Integer,
            CpVal::Cell(_) => Type::Cell,
            CpVal::Vector(v) => Type::Vector(Some(v.get_type().get_size() as usize)),
            CpVal::CellArray(a) => Type::CellArray(Some(Arc::clone(a.shape()))),
            CpVal::CellArrayMut(a) => Type::CellArrayMut(Some(Arc::clone(a.shape()))),
            CpVal::CellSet(_) => Type::CellSet,
        }
    }
}
impl CpVal {
    /// Returns the value as an [`llvm::BasicValueEnum`].
    pub(crate) fn to_basic_value(&self) -> llvm::BasicValueEnum {
        match self {
            CpVal::Integer(x) => (*x).into(),
            CpVal::Cell(x) => (*x).into(),
            CpVal::Vector(x) => (*x).into(),
            CpVal::CellArray(a) | CpVal::CellArrayMut(a) => match a.cells() {
                None => llvm::cell_ndarray_type(a.ndim()).get_undef().into(),
                Some(cells) => cells.struct_value.into(),
            },
            CpVal::CellSet(x) => (*x).into(),
        }
    }
    /// Converts a basic value to a `CpVal`.
    pub(crate) fn from_basic_value(ty: Type, v: llvm::BasicValueEnum) -> Result<CpVal> {
        fn ndarray_from_basic_value(
            shape: Arc<VectorSet>,
            v: llvm::BasicValueEnum,
        ) -> Option<llvm::NdArrayValue> {
            Some(llvm::NdArrayValue {
                bounds: shape.bounds()?,
                struct_value: v.into_struct_value(),
            })
        }

        match ty {
            Type::Integer => Ok(CpVal::Integer(v.into_int_value())),
            Type::Cell => Ok(CpVal::Cell(v.into_int_value())),
            Type::Vector(_) => Ok(CpVal::Vector(v.into_vector_value())),
            Type::CellArray(Some(shape)) => Ok(CpVal::CellArray(LlvmCellArray::new(
                Arc::clone(&shape),
                ndarray_from_basic_value(shape, v),
            ))),
            Type::CellArrayMut(Some(shape)) => Ok(CpVal::CellArrayMut(LlvmCellArray::new(
                Arc::clone(&shape),
                ndarray_from_basic_value(shape, v),
            ))),
            Type::CellSet => Ok(CpVal::CellSet(v.into_vector_value())),
            _ => internal_error!("cannot create CpVal of type {}", ty),
        }
    }
}

pub trait SpannedCompileValueExt {
    /// Returns the value inside if this is an `Integer` or subtype of one;
    /// otherwise returns a type error.
    fn as_integer(&self) -> Result<llvm::IntValue>;
    /// Returns the value inside if this is a `Cell` or subtype of one;
    /// otherwise returns a type error.
    fn as_cell(&self) -> Result<llvm::IntValue>;
    /// Returns the value inside if this is a `Vector` or subtype of one;
    /// otherwise returns a type error.
    fn as_vector(&self) -> Result<llvm::VectorValue>;
    /// Returns the value inside if this is a `CellArray` or subtype of one;
    /// otherwise returns a type error.
    fn as_cell_array(&self) -> Result<LlvmCellArray>;
    /// Returns the value inside if this is a `CellArrayMut` or a subtype of
    /// one; otherwise returns a type error.
    fn as_cell_array_mut(&self) -> Result<LlvmCellArray>;
    /// Returns the value inside if this is a `CellSet` or subtype of one;
    /// otherwise returns a type error.
    fn as_cell_set(&self) -> Result<llvm::VectorValue>;
}
impl SpannedCompileValueExt for Spanned<CpVal> {
    fn as_integer(&self) -> Result<llvm::IntValue> {
        match self.node {
            CpVal::Integer(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Integer, &self.ty())),
        }
    }
    fn as_cell(&self) -> Result<llvm::IntValue> {
        match self.node {
            CpVal::Cell(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Cell, &self.ty())),
        }
    }
    fn as_vector(&self) -> Result<llvm::VectorValue> {
        match self.node {
            CpVal::Vector(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Vector(None), &self.ty())),
        }
    }
    fn as_cell_array(&self) -> Result<LlvmCellArray> {
        match &self.node {
            CpVal::CellArray(a) | CpVal::CellArrayMut(a) => Ok(a.clone()),
            _ => Err(Error::type_error(
                self.span,
                Type::CellArray(None),
                &self.ty(),
            )),
        }
    }
    fn as_cell_array_mut(&self) -> Result<LlvmCellArray> {
        match &self.node {
            CpVal::CellArrayMut(a) => Ok(a.clone()),
            _ => Err(Error::type_error(
                self.span,
                Type::CellArrayMut(None),
                &self.ty(),
            )),
        }
    }
    fn as_cell_set(&self) -> Result<llvm::VectorValue> {
        match self.node {
            CpVal::CellSet(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::CellSet, &self.ty())),
        }
    }
}
