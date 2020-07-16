use inkwell::targets::TargetData;
use inkwell::types::StructType;
use itertools::Itertools;
use std::convert::TryInto;

use crate::compiler::types::size_of;
use crate::errors::NO_RUNTIME_REPRESENTATION;
use crate::types::{CellStateFilter, LangCellState, LangInt, LangUint, INT_BYTES};
use crate::{ConstValue, Type};

/// Wrapper around a ConstValue converted to raw bytes which can be passed to a
/// JIT function.
///
/// Note that some types may be mutated if the JIT code modifies the value (e.g.
/// the backing store for a cell pattern) so this struct holds a mutable
/// reference.
pub struct ValueConvert<'a> {
    /// Type of the value.
    ty: Type,
    /// Value convert to bytes. We have to hold onto this because the bytes may
    /// contain a pointer to data owned by this value.
    value: &'a mut ConstValue,
    /// Bytes which can be passed to a JIT function. We use Vec<u64> instead of
    /// Vec<u8> to force 8-bytes alignment, which is required for some types.
    bytes: Vec<u64>,
    /// LLVM target data.
    target_data: &'a TargetData,
}
impl<'a> ValueConvert<'a> {
    /// Converts a value to raw bytes. Panics if this type has no runtime
    /// representation.
    pub fn from_value(value: &'a mut ConstValue, target_data: &'a TargetData) -> Self {
        let ty = value.ty();
        let size = size_of(&ty, target_data);
        let mut bytes = vec![0_u64; crate::utils::div_ceil(size, std::mem::size_of::<u64>())];

        // Check that this type can be represented in raw bytes.
        assert!(
            ty.has_runtime_representation(),
            "Cannot convert {:?} to bytes",
            ty,
        );

        match value {
            ConstValue::Void => (),
            ConstValue::Int(i) => i.to_ne_bytes().iter().copied().fill_u64_slice(&mut bytes),
            ConstValue::CellState(i) => i.to_ne_bytes().iter().copied().fill_u64_slice(&mut bytes),
            ConstValue::Vector(values) => values
                .iter()
                .flat_map(|i| i.to_ne_bytes().iter().copied().collect_vec())
                .fill_u64_slice(&mut bytes),
            ConstValue::IntRange { start, end, step } => std::iter::empty()
                .chain(&start.to_ne_bytes())
                .chain(&end.to_ne_bytes())
                .chain(&step.to_ne_bytes())
                .copied()
                .fill_u64_slice(&mut bytes),
            ConstValue::Rectangle(start, end) => {
                let ndim = start.len();
                let struct_type = super::types::rectangle(ndim);
                let offsets = struct_offsets(&struct_type, target_data);
                std::iter::empty()
                    .pad_using(offsets[0], |_| 0_u8)
                    .chain(
                        start
                            .iter()
                            .flat_map(|i| i.to_ne_bytes().iter().copied().collect_vec()),
                    )
                    .pad_using(offsets[1], |_| 0_u8)
                    .chain(
                        end.iter()
                            .flat_map(|i| i.to_ne_bytes().iter().copied().collect_vec()),
                    )
                    .fill_u64_slice(&mut bytes)
            }
            ConstValue::CellStateFilter(f) => {
                let llvm_ty = super::types::cell_state_filter(f.state_count());
                let bits_per_element = llvm_ty.get_element_type().into_int_type().get_bit_width();
                let bytes_per_element = bits_per_element as usize / 8;
                for (i, &bits) in f.as_bits().iter().enumerate() {
                    bytes[i] = match bytes_per_element {
                        1 => {
                            let [b0] = (bits as u8).to_ne_bytes();
                            u64::from_ne_bytes([b0, 0, 0, 0, 0, 0, 0, 0])
                        }
                        2 => {
                            let [b0, b1] = (bits as u16).to_ne_bytes();
                            u64::from_ne_bytes([b0, b1, 0, 0, 0, 0, 0, 0])
                        }
                        4 => {
                            let [b0, b1, b2, b3] = (bits as u32).to_ne_bytes();
                            u64::from_ne_bytes([b0, b1, b2, b3, 0, 0, 0, 0])
                        }
                        8 => bits,
                        _ => panic!("Unknown cell state filter element type"),
                    };
                }
            }
            ConstValue::Stencil(_) => panic!(NO_RUNTIME_REPRESENTATION),
        };

        Self {
            ty,
            value,
            bytes,
            target_data,
        }
    }

    /// Reads the value encoded by the bytes into the &mut ConstValue passed to
    /// from_value().
    ///
    /// Note that this method cannot read cell patterns (because doing so much
    /// dereference a freed pointer, which is UB) so it returns Err(_) instead.
    ///
    /// This function is marked unsafe because the bytes may contain a raw
    /// pointer, and dereferencing that pointer may be unsafe if the value was
    /// formed
    pub fn update_value_from_bytes(&mut self) -> Result<(), ()> {
        // Transmute u64 to u8.
        let (pre, bytes, post) = unsafe { self.bytes.align_to::<u8>() };
        assert!(pre.is_empty());
        assert!(post.is_empty());
        *self.value = match &self.ty {
            Type::Void => ConstValue::Void,
            Type::Int => ConstValue::Int({
                let bytes: &[u8; INT_BYTES] = bytes[..INT_BYTES].try_into().unwrap();
                LangInt::from_ne_bytes(*bytes)
            }),
            Type::CellState => ConstValue::CellState({
                const CELL_STATE_BYTES: usize = std::mem::size_of::<LangCellState>();
                let bytes: &[u8; CELL_STATE_BYTES] = bytes[..CELL_STATE_BYTES].try_into().unwrap();
                LangCellState::from_ne_bytes(*bytes)
            }),
            Type::Vector(_) => ConstValue::Vector(
                bytes
                    .chunks(INT_BYTES)
                    .map(|b| LangInt::from_ne_bytes(b.try_into().unwrap()))
                    .collect(),
            ),
            Type::IntRange => {
                let mut values = bytes
                    .chunks(INT_BYTES)
                    .map(|b| LangInt::from_ne_bytes(b.try_into().unwrap()));
                ConstValue::IntRange {
                    start: values.next().unwrap(),
                    end: values.next().unwrap(),
                    step: values.next().unwrap(),
                }
            }
            Type::Rectangle(ndim) => {
                let struct_type = super::types::rectangle(*ndim);
                let offsets = struct_offsets(&struct_type, self.target_data);
                let start = bytes[offsets[0]..]
                    .chunks(INT_BYTES)
                    .take(*ndim)
                    .map(|b| LangInt::from_ne_bytes(b.try_into().unwrap()))
                    .collect();
                let end = bytes[offsets[1]..]
                    .chunks(INT_BYTES)
                    .take(*ndim)
                    .map(|b| LangInt::from_ne_bytes(b.try_into().unwrap()))
                    .collect();
                ConstValue::Rectangle(start, end)
            }
            Type::CellStateFilter(state_count) => {
                let llvm_ty = super::types::cell_state_filter(*state_count);
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
                    .take(CellStateFilter::vec_len_for_state_count(*state_count))
                    .collect();
                ConstValue::CellStateFilter(CellStateFilter::from_bits(*state_count, bits))
            }
            Type::Stencil => panic!(NO_RUNTIME_REPRESENTATION),
        };
        Ok(())
    }

    /// Returns the raw bytes representing this value.
    pub fn bytes(&mut self) -> &mut [u64] {
        &mut self.bytes
    }
}

/// Returns a list of offsets for each element in the given struct type.
fn struct_offsets(struct_type: &StructType<'static>, target_data: &TargetData) -> Vec<usize> {
    (0..struct_type.count_fields())
        .map(|i| target_data.offset_of_element(&struct_type, i).unwrap() as usize)
        .collect()
}

/// Monkey-patch trait to fill a &mut [u64] from a u8 iterator.
trait FillU64Slice {
    /// Fill a &mut [u64] from a u8 iterator, filling extra bytes with 0.
    fn fill_u64_slice(&mut self, slice: &mut [u64]);
}
impl<T: Iterator<Item = u8>> FillU64Slice for T {
    fn fill_u64_slice(&mut self, slice: &mut [u64]) {
        const U64_SIZE: usize = std::mem::size_of::<u64>();
        for chunk in slice {
            let mut bytes = [0_u8; U64_SIZE];
            for i in 0..U64_SIZE {
                bytes[i] = self.next().unwrap_or_default();
            }
            *chunk = u64::from_ne_bytes(bytes);
        }
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

        for old_value in ints
            .chain(cell_states)
            .chain(vecs)
            .chain(ranges)
            .chain(rectangles)
            .chain(cell_state_filters)
        {
            let mut new_value = old_value.clone();
            let mut bytes = ValueConvert::from_value(&mut new_value, target_data);
            bytes.update_value_from_bytes().unwrap();
            assert_eq!(old_value, new_value);
        }
    }
}
