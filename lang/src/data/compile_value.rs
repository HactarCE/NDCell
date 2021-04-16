use crate::data::Type;
use crate::errors::{Error, Result};
use crate::llvm;
use crate::runtime;

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
}
