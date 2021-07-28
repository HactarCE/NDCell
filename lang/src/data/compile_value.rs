use codemap::Spanned;

use super::Type;
use crate::errors::{Error, Result};
use crate::llvm::{self, traits::*};

/// Compile-time variable value of any type.
#[derive(Debug, Clone)]
pub enum CpVal {
    Integer(llvm::IntValue),
    Cell(llvm::IntValue),
    Vector(llvm::VectorValue),
    Array(/* todo */),
    CellSet(llvm::VectorValue),
}
impl CpVal {
    /// Returns the type of the value.
    pub fn ty(&self) -> Type {
        match self {
            CpVal::Integer(_) => Type::Integer,
            CpVal::Cell(_) => Type::Cell,
            CpVal::Vector(v) => Type::Vector(Some(v.get_type().get_size() as usize)),
            CpVal::Array() => todo!("array type"),
            CpVal::CellSet(_) => Type::CellSet,
        }
    }

    pub fn llvm_value(&self) -> llvm::BasicValueEnum {
        match self {
            CpVal::Integer(v) => v.as_basic_value_enum(),
            CpVal::Cell(v) => v.as_basic_value_enum(),
            CpVal::Vector(v) => v.as_basic_value_enum(),
            CpVal::Array() => todo!("array type"),
            CpVal::CellSet(v) => v.as_basic_value_enum(),
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
    /// Returns the value inside if this is an `Array` or subtype of one;
    /// otherwise returns a type error.
    fn as_array(self) -> Result<()>;
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
    fn as_array(self) -> Result<()> {
        match self.node {
            CpVal::Array() => todo!("array type"),
            _ => Err(Error::type_error(self.span, Type::Array(None), &self.ty())),
        }
    }
    fn as_cell_set(self) -> Result<llvm::VectorValue> {
        match self.node {
            CpVal::CellSet(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::CellSet, &self.ty())),
        }
    }
}
