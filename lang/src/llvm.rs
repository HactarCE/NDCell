//! Type aliases, static refs and utility functions for low-level LLVM things.

pub use inkwell::context::Context;
pub use inkwell::targets::TargetData;
pub use inkwell::{AddressSpace, IntPredicate};
use itertools::Itertools;
use thread_local::ThreadLocal;

use ndcell_core::prelude::IRect6D;

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

pub type ArrayValue = inkwell::values::ArrayValue<'static>;
pub type BasicValueEnum = inkwell::values::BasicValueEnum<'static>;
pub type FunctionValue = inkwell::values::FunctionValue<'static>;
pub type IntValue = inkwell::values::IntValue<'static>;
pub type PhiValue = inkwell::values::PhiValue<'static>;
pub type PointerValue = inkwell::values::PointerValue<'static>;
pub type StructValue = inkwell::values::StructValue<'static>;
pub type VectorValue = inkwell::values::VectorValue<'static>;

pub type ArrayType = inkwell::types::ArrayType<'static>;
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
/// Returns the LLVM type used for cell arrays.
pub fn cell_array_type() -> PointerType {
    cell_type().ptr_type(AddressSpace::Generic)
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

pub fn const_shuffle_vector(v: impl IntoIterator<Item = usize>) -> VectorValue {
    VectorType::const_vector(
        &v.into_iter()
            .map(|i| ctx().i32_type().const_int(i as u64, false))
            .collect_vec(),
    )
}

/// Returns the name of an LLVM type used in names of intrinsics (e.g., "i32"
/// for a 32-bit integer, or "v3i64" for a vector of three 64-bit integers).
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

/// Obtains a constant `VectorValue`'s sign-extended value.
pub fn vector_value_as_constant(v: VectorValue) -> Option<Vec<LangInt>> {
    if v.is_constant_vector() {
        let len = v.get_type().get_size();
        (0..len)
            .map(|i| {
                v.get_element_as_constant(i)
                    .into_int_value()
                    .get_sign_extended_constant()
                    .map(|x| x as LangInt)
            })
            .collect()
    } else {
        None
    }
}

/// LLVM representation of an N-dimensional array.
#[derive(Debug, Copy, Clone)]
pub struct NdArrayValue {
    bounds: IRect6D,
    origin_ptr: PointerValue,
    strides: VectorValue,
    mutable: bool,
}
impl NdArrayValue {
    /// Constructs a new N-dimensional array from a pointer to the origin in the
    /// array.
    pub fn from_origin_ptr(
        bounds: IRect6D,
        origin_ptr: PointerValue,
        strides: VectorValue,
        mutable: bool,
    ) -> Self {
        Self {
            bounds,
            origin_ptr,
            strides,
            mutable,
        }
    }
    /// Constructs a new N-dimensional array from a pointer to the base of the
    /// array. The pointer must be known at compile-time.
    pub fn from_const_base_ptr(
        bounds: IRect6D,
        base_ptr: PointerValue,
        strides: &[LangInt],
        mutable: bool,
    ) -> Self {
        // Get a pointer to the origin in the array. Note that this GEP may be
        // out of bounds if the cell array does not contain the origin (but it
        // should always be reasonably nearby).
        let base_idx = crate::utils::ndarray_base_idx(bounds, strides);
        let origin_ptr = unsafe { base_ptr.const_gep(&[const_int(-base_idx)]) };
        let strides = const_vector(strides.iter().copied());
        Self::from_origin_ptr(bounds, origin_ptr, strides, mutable)
    }
    /// Constructs a new N-dimensional array with static contents.
    pub fn new_const(
        module: &mut Module,
        ndim: usize,
        bounds: IRect6D,
        ty: IntType,
        values: &[IntValue],
        name: &str,
    ) -> Self {
        // Create array.
        let array_value = ty.const_array(values);
        // Store the array as a global constant.
        let array_global = module.add_global(
            array_value.get_type(),
            Some(AddressSpace::Const),
            &format!("{}_array", name),
        );
        array_global.set_initializer(&array_value);
        // Mark this global as a constant; we don't ever intend to modify it.
        array_global.set_constant(true);
        // The address of the constant doesn't matter; please do merge it with
        // other identical values!
        array_global.set_unnamed_addr(true);

        // Get a pointer to the array.
        let array_base_ptr = array_global
            .as_pointer_value()
            .const_cast(ty.ptr_type(AddressSpace::Const));

        let strides = crate::utils::ndarray_strides(ndim, bounds);

        let mutable = false;
        Self::from_const_base_ptr(bounds, array_base_ptr, &strides, mutable)
    }

    /// Returns the number of dimensions.
    pub fn ndim(&self) -> usize {
        self.strides.get_type().get_size() as usize
    }
    /// Returns the bounding box of the array.
    pub fn bounds(&self) -> IRect6D {
        self.bounds
    }
    /// Returns a pointer to the array values.
    pub fn origin_ptr(&self) -> PointerValue {
        self.origin_ptr
    }
    /// Returns the strides of the array.
    pub fn strides(&self) -> VectorValue {
        self.strides
    }
    /// Returns whether the array is mutable.
    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    /// Returns the minimum coordinate of the array.
    pub fn min_vec(&self) -> Vec<LangInt> {
        self.bounds().min().0[..self.ndim()]
            .iter()
            .map(|&i| i as LangInt)
            .collect()
    }
    /// Returns the maximum coordinate of the array.
    pub fn max_vec(&self) -> Vec<LangInt> {
        self.bounds().max().0[..self.ndim()]
            .iter()
            .map(|&i| i as LangInt)
            .collect()
    }
}
