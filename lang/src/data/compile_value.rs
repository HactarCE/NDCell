use codemap::Spanned;
use std::sync::Arc;

use super::{LlvmCellArray, Type};
use crate::errors::{Error, Result};
use crate::llvm;

/// Compile-time variable value of any type.
#[derive(Debug, Clone)]
pub enum CpVal {
    Integer(llvm::IntValue),
    Cell(llvm::IntValue),
    Vector(llvm::VectorValue),
    CellArray(LlvmCellArray),
    CellSet(llvm::VectorValue),
}
impl CpVal {
    /// Returns the type of the value.
    pub fn ty(&self) -> Type {
        match self {
            CpVal::Integer(_) => Type::Integer,
            CpVal::Cell(_) => Type::Cell,
            CpVal::Vector(v) => Type::Vector(Some(v.get_type().get_size() as usize)),
            CpVal::CellArray(a) => Type::CellArray(Some(Arc::clone(a.shape()))),
            CpVal::CellSet(_) => Type::CellSet,
        }
    }
}

pub trait SpannedCompileValueExt {
    /// Returns the value inside if this is an `Integer` or subtype of one;
    /// otherwise returns a type error.
    fn as_integer(self) -> Result<llvm::IntValue>;
    /// Returns the value inside if this is a `Cell` or subtype of one;
    /// otherwise returns a type error.
    fn as_cell(self) -> Result<llvm::IntValue>;
    /// Returns the value inside if this is a `Vector` or subtype of one;
    /// otherwise returns a type error.
    fn as_vector(self) -> Result<llvm::VectorValue>;
    /// Returns the value inside if this is an `CellArray` or subtype of one;
    /// otherwise returns a type error.
    fn as_cell_array(self) -> Result<LlvmCellArray>;
    /// Returns the value inside if this is a `CellSet` or subtype of one;
    /// otherwise returns a type error.
    fn as_cell_set(self) -> Result<llvm::VectorValue>;
}
impl SpannedCompileValueExt for Spanned<CpVal> {
    fn as_integer(self) -> Result<llvm::IntValue> {
        match self.node {
            CpVal::Integer(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Integer, &self.ty())),
        }
    }
    fn as_cell(self) -> Result<llvm::IntValue> {
        match self.node {
            CpVal::Cell(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Cell, &self.ty())),
        }
    }
    fn as_vector(self) -> Result<llvm::VectorValue> {
        match self.node {
            CpVal::Vector(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Vector(None), &self.ty())),
        }
    }
    fn as_cell_array(self) -> Result<LlvmCellArray> {
        match self.node {
            CpVal::CellArray(a) => Ok(a),
            _ => Err(Error::type_error(
                self.span,
                Type::CellArray(None),
                &self.ty(),
            )),
        }
    }
    fn as_cell_set(self) -> Result<llvm::VectorValue> {
        match self.node {
            CpVal::CellSet(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::CellSet, &self.ty())),
        }
    }
}
