use inkwell::targets::TargetData;
use std::convert::TryInto;

use crate::compiler::types::size_of;
use crate::errors::NO_RUNTIME_REPRESENTATION;
use crate::types::{CellStateFilter, LangCellState, LangInt, LangUint};
use crate::{ConstValue, Type};

/// Constructs a value of the given type from raw bytes. Panics if given an
/// invalid value or invalid type.
pub fn bytes_to_value(ty: Type, bytes: &[u8], target_data: &TargetData) -> ConstValue {
    // Check that this type can be represented in raw bytes.
    assert!(
        ty.has_runtime_representation(),
        "Cannot construct {:?} from bytes",
        ty,
    );
    // Check the number of bytes.
    assert_eq!(
        size_of(&ty, target_data),
        bytes.len(),
        "Wrong number of bytes to construct ConstValue; {:?} requires {:?} bytes, but got {:?}",
        ty,
        size_of(&ty, target_data),
        bytes.len(),
    );
    match ty {
        Type::Void => ConstValue::Void,
        Type::Int => ConstValue::Int({
            const SIZE: usize = std::mem::size_of::<LangInt>();
            assert_eq!(SIZE, size_of(&ty, target_data));
            let bytes: &[u8; SIZE] = bytes.try_into().unwrap();
            LangInt::from_ne_bytes(*bytes)
        }),
        Type::CellState => ConstValue::CellState({
            const SIZE: usize = std::mem::size_of::<LangCellState>();
            assert_eq!(SIZE, size_of(&ty, target_data));
            let bytes: &[u8; SIZE] = bytes.try_into().unwrap();
            LangCellState::from_ne_bytes(*bytes)
        }),
        Type::Vector(_) => ConstValue::Vector(
            bytes
                .chunks(size_of(&Type::Int, target_data))
                .map(TryInto::try_into)
                .map(Result::unwrap)
                .map(LangInt::from_ne_bytes)
                .collect(),
        ),
        Type::Pattern(_) => todo!("construct pattern from bytes"),
        Type::IntRange => {
            let values = bytes_to_value(Type::Vector(3), bytes, target_data)
                .as_vector()
                .unwrap();
            ConstValue::IntRange {
                start: values[0],
                end: values[1],
                step: values[2],
            }
        }
        Type::Rectangle(ndim) => {
            let element_size = size_of(&Type::Vector(ndim), target_data);
            let struct_type = super::types::rectangle(ndim);

            let offset = target_data.offset_of_element(&struct_type, 0).unwrap() as usize;
            let start = bytes_to_value(
                Type::Vector(ndim),
                &bytes[offset..(offset + element_size)],
                target_data,
            )
            .as_vector()
            .unwrap();

            let offset = target_data.offset_of_element(&struct_type, 1).unwrap() as usize;
            let end = bytes_to_value(
                Type::Vector(ndim),
                &bytes[offset..(offset + element_size)],
                target_data,
            )
            .as_vector()
            .unwrap();

            ConstValue::Rectangle(start, end)
        }
        Type::CellStateFilter(state_count) => {
            let llvm_ty = super::types::cell_state_filter(state_count);
            let bits_per_element = llvm_ty.get_element_type().into_int_type().get_bit_width();
            let bytes_per_element = bits_per_element as usize / 8;
            let bits: Vec<LangUint> = bytes
                .chunks(bytes_per_element)
                .map(|chunk| match chunk.len() {
                    1 => chunk[0] as LangUint,
                    2 => u16::from_ne_bytes(chunk.try_into().unwrap()) as LangUint,
                    4 => u32::from_ne_bytes(chunk.try_into().unwrap()) as LangUint,
                    8 => u64::from_ne_bytes(chunk.try_into().unwrap()) as LangUint,
                    _ => panic!("Unknown cell state filter element type"),
                })
                .collect();
            ConstValue::CellStateFilter(CellStateFilter::from_bits(state_count, bits))
        }
        Type::Stencil => panic!(NO_RUNTIME_REPRESENTATION),
    }
}

/// Writes raw bytes representing this value to an array slice. Panics if
/// this type has no runtime representation, or if the array slice is the
/// wrong length.
pub fn value_to_bytes(value: &ConstValue, bytes: &mut [u8], target_data: &TargetData) {
    // Check that this type can be represented in raw bytes.
    assert!(
        value.ty().has_runtime_representation(),
        "Cannot convert {:?} to bytes",
        value.ty(),
    );
    // Check the number of bytes.
    assert_eq!(
        bytes.len(),
        size_of(&value.ty(), target_data),
        "{:?} requires {} bytes; got {}",
        value.ty(),
        size_of(&value.ty(), target_data),
        bytes.len(),
    );
    match value {
        ConstValue::Void => (),
        ConstValue::Int(i) => {
            const SIZE: usize = std::mem::size_of::<LangInt>();
            assert_eq!(SIZE, size_of(&Type::Int, target_data));
            let bytes: &mut [u8; SIZE] = bytes.try_into().unwrap();
            *bytes = i.to_ne_bytes();
        }
        ConstValue::CellState(i) => {
            const SIZE: usize = std::mem::size_of::<LangCellState>();
            assert_eq!(SIZE, size_of(&Type::CellState, target_data));
            let bytes: &mut [u8; SIZE] = bytes.try_into().unwrap();
            *bytes = i.to_ne_bytes();
        }
        ConstValue::Vector(values) => {
            bytes
                .chunks_mut(size_of(&Type::Int, target_data))
                .zip(values)
                .for_each(|(chunk, &i)| {
                    value_to_bytes(&ConstValue::Int(i), chunk, target_data);
                });
        }
        ConstValue::IntRange { start, end, step } => {
            value_to_bytes(
                &ConstValue::Vector(vec![*start, *end, *step]),
                bytes,
                target_data,
            );
        }
        ConstValue::Rectangle(start, end) => {
            assert_eq!(start.len(), end.len());
            let ndim = start.len();
            let element_size = size_of(&Type::Vector(ndim), target_data);
            let struct_type = super::types::rectangle(ndim);

            let offset = target_data.offset_of_element(&struct_type, 0).unwrap() as usize;
            value_to_bytes(
                &ConstValue::Vector(start.clone()),
                &mut bytes[offset..(offset + element_size)],
                target_data,
            );

            let offset = target_data.offset_of_element(&struct_type, 1).unwrap() as usize;
            value_to_bytes(
                &ConstValue::Vector(end.clone()),
                &mut bytes[offset..(offset + element_size)],
                target_data,
            );
        }
        ConstValue::CellStateFilter(f) => {
            let llvm_ty = super::types::cell_state_filter(f.state_count());
            let bits_per_element = llvm_ty.get_element_type().into_int_type().get_bit_width();
            let bytes_per_element = bits_per_element as usize / 8;
            bytes
                .chunks_mut(bytes_per_element)
                .zip(f.as_bits())
                .for_each(|(chunk, &int)| match bytes_per_element {
                    1 => chunk[0] = int as u8,
                    2 => chunk.copy_from_slice(&(int as u16).to_ne_bytes()),
                    4 => chunk.copy_from_slice(&(int as u32).to_ne_bytes()),
                    8 => chunk.copy_from_slice(&int.to_ne_bytes()),
                    _ => panic!("Unknown cell state filter element type"),
                });
        }
        ConstValue::Stencil(_) => panic!(NO_RUNTIME_REPRESENTATION),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Check that values can be converted to/from their bytes representation
    /// with no mangling (i.e. that bytes_to_value() and value_to_bytes() are
    /// consistent with each other). There is a separate test in crate::tests to
    /// verify that these agree with LLVM's representation.
    #[test]
    fn test_llvm_constvalue_conversion() {
        let ints = vec![-10, -5, -1, 0, 1, 5, 10]
            .into_iter()
            .map(ConstValue::Int);

        let cell_states = vec![0, 1, 5, 10, 254, 255]
            .into_iter()
            .map(ConstValue::CellState);

        let vecs = vec![
            vec![10],
            vec![10, -20],
            vec![-10, 20, -30],
            vec![10, -20, 30, -40],
            vec![-10, 20, -30, 40, -50, 60, -70, 80, -90, 100, -110],
        ]
        .into_iter()
        .map(ConstValue::Vector);

        let ranges = vec![ConstValue::IntRange {
            start: -25,
            end: -10,
            step: 3,
        }]
        .into_iter();

        let rectangles = vec![
            ConstValue::Rectangle(vec![10, -20, 30], vec![-40, 50, -60]),
            ConstValue::Rectangle(vec![0, 1, 2, 3], vec![-10, -2, 8, 99]),
        ]
        .into_iter();

        let cell_state_filters = (0..crate::MAX_STATE_COUNT)
            .flat_map(|state_count| {
                std::iter::repeat(state_count).zip(0..state_count as LangCellState)
            })
            .map(|(state_count, cell_state)| {
                ConstValue::CellStateFilter(CellStateFilter::single_cell_state(
                    state_count,
                    cell_state,
                ))
            });

        let compiler = crate::compiler::Compiler::new().unwrap();
        let target_data = compiler.target_data();

        for value in ints
            .chain(cell_states)
            .chain(vecs)
            .chain(ranges)
            .chain(rectangles)
            .chain(cell_state_filters)
        {
            let mut bytes = vec![0; size_of(&value.ty(), target_data)];
            value_to_bytes(&value, &mut bytes, target_data);
            assert_eq!(value, bytes_to_value(value.ty(), &bytes, target_data));
        }
    }
}
