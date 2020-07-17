use inkwell::targets::TargetData;
use inkwell::types::{BasicType, BasicTypeEnum, IntType, StructType, VectorType};
use inkwell::AddressSpace;

use super::get_ctx;
use crate::errors::NO_RUNTIME_REPRESENTATION;
use crate::types::{CellStateFilter, FnSignature, CELL_STATE_BITS, INT_BITS};
use crate::{LangResult, Type};

/// Returns the LLVM type used to represent vales of the given type, or an
/// InternalError if the given type has no runtime representation.
pub fn get(ty: &Type) -> LangResult<BasicTypeEnum<'static>> {
    match ty {
        Type::Void => Ok(void().into()),
        Type::Int => Ok(int().into()),
        Type::CellState => Ok(cell_state().into()),
        Type::Vector(len) => Ok(vec(*len).into()),
        Type::Pattern { shape, has_lut } => Ok(pattern(shape.ndim(), *has_lut).into()),
        Type::IntRange => Ok(int_range().into()),
        Type::Rectangle(ndim) => Ok(rectangle(*ndim).into()),
        Type::CellStateFilter(state_count) => Ok(cell_state_filter(*state_count).into()),
        Type::Stencil => internal_error!(NO_RUNTIME_REPRESENTATION),
    }
}

/// Returns the size in bytes of the LLVM type used to represent the given type,
/// panicking if the given type has no runtime representation.
pub fn size_of(ty: &Type, target_data: &TargetData) -> usize {
    target_data
        .get_store_size(&get(ty).expect("Cannot get size of type without runtime representation"))
        as usize
}

/// Returns the LLVM type used to represent void.
pub fn void() -> StructType<'static> {
    get_ctx().struct_type(&[], false)
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
/// dimensions and optionally a cell state LUT.
pub fn pattern(ndim: usize, has_lut: bool) -> StructType<'static> {
    let field_types = [
        // Pointer to the origin (0 along all axes) in an array of
        // cell states.
        cell_state()
            .array_type(0)
            .ptr_type(AddressSpace::Generic)
            .into(),
        // Strides to increment the given axis by 1.
        vec(ndim).into(),
        // ID of cell state lookup table.
        get_ctx().i8_type().into(),
    ];
    get_ctx().struct_type(
        if has_lut {
            &field_types
        } else {
            // Exclude the cell state LUT if it is not needed.
            &field_types[..2]
        },
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

/// Returns the LLVM type used to represent a cell state filter.
pub fn cell_state_filter(state_count: usize) -> VectorType<'static> {
    // Each cell state filter is fundamentally a bit vector with length equal to
    // the number of cell states in the automaton. That bit vector is
    // represented as an LLVM vector of 64-bit integers. But if we have fewer
    // than 64 cell states, we might be able to use an even smaller integer, all
    // the way down to 8 bits.
    match state_count {
        (0..=8) => get_ctx().i8_type().vec_type(1),
        (0..=16) => get_ctx().i16_type().vec_type(1),
        (0..=32) => get_ctx().i32_type().vec_type(1),
        _ => vec(CellStateFilter::vec_len_for_state_count(state_count)),
    }
}

/// Returns the LLVM function type for the given function signature.
pub fn function(fn_signature: FnSignature) -> LangResult<inkwell::types::FunctionType<'static>> {
    let llvm_param_types = fn_signature
        .args
        .iter()
        .map(get)
        .collect::<LangResult<Vec<_>>>()?;
    let llvm_ret_type = get(&fn_signature.ret)?;
    Ok(llvm_ret_type.fn_type(&llvm_param_types, false))
}
