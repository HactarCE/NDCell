use inkwell::targets::TargetData;
use inkwell::types::{BasicTypeEnum, IntType, StructType, VectorType};
use inkwell::AddressSpace;

use super::get_ctx;
use crate::types::{CELL_STATE_BITS, INT_BITS};
use crate::{LangResult, Type};

/// Returns the LLVM type used to represent vales of the given type, or an
/// InternalError if the given type has no runtime representation.
pub fn get(ty: &Type) -> LangResult<BasicTypeEnum<'static>> {
    match ty {
        Type::Int => Ok(int().into()),
        Type::CellState => Ok(cell_state().into()),
        Type::Vector(len) => Ok(vec(*len).into()),
        Type::Pattern(shape) => Ok(pattern(shape.ndim()).into()),
        Type::IntRange => Ok(int_range().into()),
        Type::Rectangle(ndim) => Ok(rectangle(*ndim).into()),
        // _ => Err(InternalError(
        //     "Attempt to get LLVM representation of type that has none".into(),
        // )
        // .without_span()),
    }
}

/// Returns the size in bytes of the LLVM type used to represent the given type,
/// panicking if the given type has no runtime representation.
pub fn size_of(ty: &Type, target_data: &TargetData) -> usize {
    target_data
        .get_store_size(&get(ty).expect("Cannot get size of type without runtime representation"))
        as usize
}

/// Returns the LLVM type used to represent an integer.
pub fn int() -> IntType<'static> {
    get_ctx().custom_width_int_type(INT_BITS)
}

/// Returns the LLVM type used to represent a cell state.
pub fn cell_state() -> IntType<'static> {
    get_ctx().custom_width_int_type(CELL_STATE_BITS)
}

/// Returns the LLVM type used to represent a vector with the given length.
pub fn vec(ndim: usize) -> VectorType<'static> {
    int().vec_type(ndim as u32)
}

/// Returns the LLVM type used to represent a pattern with the given number of
/// dimensions.
pub fn pattern(ndim: usize) -> StructType<'static> {
    get_ctx().struct_type(
        &[
            // Pointer to the origin (0 along all axes) in an array of
            // cell states.
            cell_state().ptr_type(AddressSpace::Generic).into(),
            // Strides to increment the given axis by 1.
            vec(ndim).into(),
        ],
        false,
    )
}

/// Returns the LLVM type used to represent an integer range.
pub fn int_range() -> VectorType<'static> {
    vec(3)
}

/// Returns the LLVM type used to represent a hyperrectangle.
pub fn rectangle(ndim: usize) -> StructType<'static> {
    let vec_type = vec(ndim).into();
    get_ctx().struct_type(&[vec_type, vec_type], false)
}
