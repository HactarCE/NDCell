//! Parameter for a JIT-compiled function.

use inkwell::targets::TargetData;
use inkwell::types::StructType;
use itertools::Itertools;
use std::convert::TryInto;
use std::ops::Range;
use std::sync::Arc;

use ndcell_core::num::Integer;

use crate::data::{
    CellSet, LangCell, LangInt, LangUint, RtVal, Type, VectorSet, CELL_STATE_BYTES, INT_BYTES,
};
use crate::errors::Result;
use crate::llvm;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParamType {
    Integer,
    Cell,
    // Tag,
    Vector(usize),
    CellArray(Arc<VectorSet>),
    // CellSet(usize),
}
impl From<ParamType> for Type {
    fn from(ty: ParamType) -> Self {
        match ty {
            ParamType::Integer => Type::Integer,
            ParamType::Cell => Type::Cell,
            ParamType::Vector(len) => Type::Vector(Some(len)),
            ParamType::CellArray(shape) => Type::CellArray(Some(shape)),
        }
    }
}

/// Parameter to a JIT-compiled function.
#[derive(Debug, Clone)]
pub struct Param {
    /// Offset in parameter struct.
    pub offset: usize,
    /// Size in bytes in the parameter struct.
    pub size: usize,

    /// Type of value.
    pub ty: ParamType,
}
impl Param {
    /// Returns the type that this Parameter converts.
    pub fn ty(&self) -> Type {
        self.ty.clone().into()
    }
    pub fn slice_range(&self) -> Range<usize> {
        let a = self.offset;
        let b = a + self.size;
        a..b
    }

    /// Converts a value to raw bytes, or returns an error if the value is not
    /// the correct type.
    ///
    /// # Safety
    ///
    /// The raw bytes may contain a pointer to the original value and the
    /// contents may be mutated through that pointer, so a mutable reference to
    /// the value must be valid for at least as long as the bytes are
    /// referenced.
    ///
    /// To be read correctly by JIT-compiled code, `bytes` must be 8-byte
    /// aligned.
    pub fn value_to_bytes(&self, bytes: &mut [u8], value: &mut RtVal) -> Result<()> {
        // TODO: when #![feature(array_chunks)] stabalizes, use that here
        // instead of `fill_u8_slice`.
        match (&self.ty, value) {
            (ParamType::Integer, RtVal::Integer(i)) => {
                i.to_ne_bytes().iter().copied().fill_u8_slice(bytes);
            }

            (ParamType::Cell, RtVal::Cell(i)) => {
                bytes[0] = *i;
            }

            (ParamType::Vector(len), RtVal::Vector(v)) if v.len() == *len => {
                for (chunk, i) in bytes.chunks_mut(INT_BYTES).take(*len).zip(v) {
                    i.to_ne_bytes().iter().copied().fill_u8_slice(chunk);
                }
            }

            (ParamType::CellArray(shape), RtVal::CellArray(a)) if shape == a.shape() => {
                // Only store the cell array; strides are determined by the
                // shape.
                let p = a.get_origin_ptr() as usize;
                p.to_ne_bytes().iter().copied().fill_u8_slice(bytes);
            }

            (expected, got) => {
                return Err(internal_error_value!(
                    "wrong parameter type; expected {:?} but got {:?}",
                    expected,
                    got,
                ));
            }
        }

        Ok(())

        //     RtVal::CellSet(f) => {
        //         todo!("convert cell set to bytes");
        //         /*
        //         let llvm_ty = super::types::cell_state_filter(f.state_count());
        //         let bits_per_element = llvm_ty.get_element_type().into_int_type().get_bit_width();
        //         let bytes_per_element = bits_per_element as usize / 8;
        //         for (i, &bits) in f.as_bits().iter().enumerate() {
        //             self.bytes[i] = match bytes_per_element {
        //                 1 => {
        //                     let [b0] = (bits as u8).to_ne_bytes();
        //                     u64::from_ne_bytes([b0, 0, 0, 0, 0, 0, 0, 0])
        //                 }
        //                 2 => {
        //                     let [b0, b1] = (bits as u16).to_ne_bytes();
        //                     u64::from_ne_bytes([b0, b1, 0, 0, 0, 0, 0, 0])
        //                 }
        //                 4 => {
        //                     let [b0, b1, b2, b3] = (bits as u32).to_ne_bytes();
        //                     u64::from_ne_bytes([b0, b1, b2, b3, 0, 0, 0, 0])
        //                 }
        //                 8 => bits,
        //                 _ => panic!("Unknown cell state filter element type"),
        //             };
        //         }
        //         */
        //     }

        //     _ => panic!(NO_RUNTIME_REPRESENTATION),
        // };
    }
    /// Reads the value encoded by the bytes into the &mut RtVal passed to
    /// [`Param::from_value()`].
    ///
    /// Note that this method does not read cell arrays (because doing so might
    /// dereference a freed pointer, which is UB), however it will still return
    /// `Ok(())`.
    pub fn bytes_to_value(&self, bytes: &[u8], value: &mut RtVal) {
        match self.ty {
            ParamType::Integer => {
                let bytes: &[u8; INT_BYTES] = bytes[..INT_BYTES].try_into().unwrap();
                *value = RtVal::Integer(LangInt::from_ne_bytes(*bytes));
            }

            ParamType::Cell => {
                *value = RtVal::Cell(bytes[0]);
            }

            ParamType::Vector(len) => {
                *value = RtVal::Vector(
                    bytes
                        .chunks(INT_BYTES)
                        .take(len)
                        .map(|b| LangInt::from_ne_bytes(b.try_into().unwrap()))
                        .collect(),
                );
            }

            /*
            ParamType::CellSet => Ok({
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
                    .take(CellSet::vec_len_for_state_count(*state_count))
                    .collect();
                RtVal::CellSet(CellSet::from_bits(*state_count, bits))
            }),

            */
            // LLVM JIT function already wrote using the pointer
            ParamType::CellArray(_) => (),
        }
    }
}

/// Monkey-patch trait to fill a `&mut [u8]` from a u8 iterator.
trait FillU8Slice {
    /// Fill a `&mut [u8]` from a u8 iterator, filling extra bytes with 0.
    fn fill_u8_slice(&mut self, slice: &mut [u8]);
}
impl<T: Iterator<Item = u8>> FillU8Slice for T {
    fn fill_u8_slice(&mut self, slice: &mut [u8]) {
        for (i, b) in self.enumerate() {
            slice[i] = b;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Check that values can be converted to/from their bytes representation
    /// with no mangling (i.e., that [`Param::bytes_to_value()`] and
    /// [`Param::value_to_bytes()`] are consistent with each other). There is a
    /// separate test in [`crate::tests`] to verify that these agree with LLVM's
    /// representation.
    #[test]
    fn test_llvm_convert_param() {
        // Test `Integer`.
        test_llvm_convert_param_single(
            Param {
                offset: 0,
                size: INT_BYTES,
                ty: ParamType::Integer,
            },
            vec![LangInt::MIN, -10, -5, -1, 0, 1, 5, 10, LangInt::MAX]
                .into_iter()
                .map(RtVal::Integer),
        );

        // Test `Cell`.
        test_llvm_convert_param_single(
            Param {
                offset: 0,
                size: CELL_STATE_BYTES,
                ty: ParamType::Cell,
            },
            vec![0, 1, 5, 10, 254, 255].into_iter().map(RtVal::Cell),
        );

        // Test `Vector`.
        let vecs = vec![
            vec![10],
            vec![10, -20],
            vec![-10, 20, -30],
            vec![10, -20, 30, -40],
            vec![-10, 20, -30, 40, -50, 60, -70, 80, -90, 100],
        ];
        for v in vecs {
            test_llvm_convert_param_single(
                Param {
                    offset: 0,
                    size: INT_BYTES * 10,
                    ty: ParamType::Vector(v.len()),
                },
                vec![RtVal::Vector(v)],
            );
        }

        // let cell_state_filters = (0..crate::MAX_STATE_COUNT)
        //     .flat_map(|state_count| std::iter::repeat(state_count).zip(0..state_count as LangCell))
        //     .map(|(state_count, cell_state)| RtVal::CellSet(CellSet::single_cell(cell_state)));
    }

    fn test_llvm_convert_param_single(p: Param, test_values: impl IntoIterator<Item = RtVal>) {
        let mut bytes = vec![0_u8; p.size];
        for mut old_value in test_values {
            p.value_to_bytes(&mut bytes, &mut old_value).unwrap();
            let mut new_value = RtVal::Null;
            p.bytes_to_value(&bytes, &mut new_value);
            assert_eq!(old_value, new_value);
        }
    }
}
