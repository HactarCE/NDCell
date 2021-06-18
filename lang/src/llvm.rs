//! Type aliases, static refs and utility functions for low-level LLVM things.

pub use inkwell::context::Context;
pub use inkwell::targets::TargetData;
pub use inkwell::{AddressSpace, IntPredicate};
use itertools::Itertools;
use thread_local::ThreadLocal;

pub mod traits {
    pub use inkwell::execution_engine::UnsafeFunctionPointer;
    pub use inkwell::types::BasicType;
    pub use inkwell::values::BasicValue;

    pub use super::IntMathValue;
}

use crate::data::{LangCell, LangInt, CELL_STATE_BITS, INT_BITS};
pub use traits::*;

/// Raw JIT-compiled function pointer.
pub type JitFnPtr = unsafe extern "C" fn(*mut u64) -> u32;

pub type BasicBlock = inkwell::basic_block::BasicBlock<'static>;
pub type ExecutionEngine = inkwell::execution_engine::ExecutionEngine<'static>;
pub type JitFunction = inkwell::execution_engine::JitFunction<'static, JitFnPtr>;
pub type Module = inkwell::module::Module<'static>;
pub type Builder = inkwell::builder::Builder<'static>;

pub type BasicValueEnum = inkwell::values::BasicValueEnum<'static>;
pub type FunctionValue = inkwell::values::FunctionValue<'static>;
pub type IntValue = inkwell::values::IntValue<'static>;
pub type PhiValue = inkwell::values::PhiValue<'static>;
pub type PointerValue = inkwell::values::PointerValue<'static>;
pub type StructValue = inkwell::values::StructValue<'static>;
pub type VectorValue = inkwell::values::VectorValue<'static>;

pub type BasicTypeEnum = inkwell::types::BasicTypeEnum<'static>;
pub type FunctionType = inkwell::types::FunctionType<'static>;
pub type IntType = inkwell::types::IntType<'static>;
pub type PointerType = inkwell::types::PointerType<'static>;
pub type StructType = inkwell::types::StructType<'static>;
pub type VectorType = inkwell::types::VectorType<'static>;

lazy_static! {
    /// Per-thread LLVM context.
    static ref CTX: ThreadLocal<Context> = ThreadLocal::new();
}
/// Returns this thread's LLVM context.
pub fn ctx() -> &'static Context {
    &CTX.get_or(Context::create)
}

/// Returns a list of offsets for each element in the given struct type.
pub fn struct_offsets<'a>(
    struct_type: &'a StructType,
    target_data: &'a TargetData,
) -> impl 'a + Iterator<Item = usize> {
    (0..struct_type.count_fields())
        .map(move |i| target_data.offset_of_element(struct_type, i).unwrap() as usize)
}

/// Returns LLVM boolean (`i1`) [`IntValue`] of 0.
pub fn false_() -> IntValue {
    ctx().bool_type().const_zero()
}
/// Returns LLVM boolean (`i1`) [`IntValue`] of 1.
pub fn true_() -> IntValue {
    ctx().bool_type().const_all_ones()
}

/// Returns the LLVM type used for booleans.
pub fn bool_type() -> IntType {
    ctx().bool_type()
}
/// Returns the LLVM type used for error indices.
pub fn error_index_type() -> IntType {
    ctx().i32_type()
}
/// Maximum error index, which is a sentinel value indicating no error.
pub const MAX_ERROR_INDEX: u32 = u32::MAX;

/// Returns the LLVM type used for integers.
pub fn int_type() -> IntType {
    ctx().custom_width_int_type(INT_BITS)
}
/// Returns the LLVM type used for cell states.
pub fn cell_type() -> IntType {
    ctx().custom_width_int_type(CELL_STATE_BITS)
}
/// Returns the LLVM type used for tags.
pub fn tag_type() -> IntType {
    todo!("tag type")
}
/// Returns the LLVM type used for vectors of a specific length.
pub fn vector_type(len: usize) -> VectorType {
    int_type().vec_type(len as u32)
}

/// Returns a constant boolean (`i1`) LLVM value.
pub fn const_bool(b: bool) -> IntValue {
    match b {
        false => false_(),
        true => true_(),
    }
}
/// Returns a constant integer LLVM value.
pub fn const_int(i: LangInt) -> IntValue {
    int_type().const_int(i as u64, true)
}
/// Returns a constant cell state LLVM value.
pub fn const_cell(i: LangCell) -> IntValue {
    cell_type().const_int(i as u64, false)
}
/// Returns a constant vector LLVM value.
pub fn const_vector(v: impl IntoIterator<Item = LangInt>) -> VectorValue {
    VectorType::const_vector(&v.into_iter().map(const_int).collect_vec())
}

/// Returns the name of an LLVM type used in names of intrinsics (e.g. "i32" for
/// a 32-bit integer, or "v3i64" for a vector of three 64-bit integers).
pub fn intrinsic_type_name(ty: impl BasicType<'static>) -> String {
    match ty.as_basic_type_enum() {
        BasicTypeEnum::ArrayType(_) => unimplemented!(),
        BasicTypeEnum::IntType(ty) => format!("i{}", ty.get_bit_width()),
        BasicTypeEnum::FloatType(_) => unimplemented!(),
        BasicTypeEnum::PointerType(_) => unimplemented!(),
        BasicTypeEnum::StructType(_) => unimplemented!(),
        BasicTypeEnum::VectorType(ty) => format!(
            "v{}{}",
            ty.get_size(),
            intrinsic_type_name(ty.get_element_type()),
        ),
    }
}

/// Trait for [`IntValue`]s and [`VectorValue`]s that provides extra utility beyond
/// [`inkwell::values::IntMathValue`].
pub trait IntMathValue: inkwell::values::IntMathValue<'static> + Copy {
    /// Returns the base type.
    fn base_type(self) -> Self::BaseType;

    /// Returns a constant signed value of the same type.
    fn same_type_const_signed(self, x: i64) -> Self;
    /// Returns a constant unsigned value of the same type.
    fn same_type_const_unsigned(self, x: u64) -> Self;
    /// Returns the minimum possible unsigned value or the maximum possible
    /// signed value of the same type.
    fn same_type_const_all_ones(self) -> Self;
    /// Returns a `0` value of the same type.
    fn same_type_const_zero(self) -> Self {
        self.same_type_const_signed(0)
    }
    /// Returns a `1` value of the same type.
    fn same_type_const_one(self) -> Self {
        self.same_type_const_signed(1)
    }
    /// Returns a `-1` value of the same type.
    fn same_type_const_neg_one(self) -> Self {
        self.same_type_const_signed(-1)
    }
}
impl IntMathValue for IntValue {
    fn base_type(self) -> Self::BaseType {
        self.get_type()
    }

    fn same_type_const_signed(self, x: i64) -> Self {
        self.get_type().const_int(x as u64, true)
    }
    fn same_type_const_unsigned(self, x: u64) -> Self {
        self.get_type().const_int(x, false)
    }
    fn same_type_const_all_ones(self) -> Self {
        self.get_type().const_all_ones()
    }
}
impl IntMathValue for VectorValue {
    fn base_type(self) -> Self::BaseType {
        self.get_type()
    }

    fn same_type_const_signed(self, x: i64) -> Self {
        let elem_ty = self.get_type().get_element_type().into_int_type();
        let v = elem_ty.const_int(x as u64, true);
        let len = self.get_type().get_size() as usize;
        VectorType::const_vector(&vec![v; len])
    }
    fn same_type_const_unsigned(self, x: u64) -> Self {
        let elem_ty = self.get_type().get_element_type().into_int_type();
        let v = elem_ty.const_int(x, false);
        let len = self.get_type().get_size() as usize;
        VectorType::const_vector(&vec![v; len])
    }
    fn same_type_const_all_ones(self) -> Self {
        let elem_ty = self.get_type().get_element_type().into_int_type();
        let v = elem_ty.const_all_ones();
        let len = self.get_type().get_size() as usize;
        VectorType::const_vector(&vec![v; len])
    }
}
