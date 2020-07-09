//! Vector functions.

use inkwell::IntPredicate;
use itertools::Itertools;
use std::convert::TryInto;

pub use super::enums::{MinMaxMode, RangeProperty, SumOrProduct};

use super::{FuncConstructor, FuncResult};
use crate::ast::{AssignableFunction, ErrorPointRef, FuncCallInfo, FuncCallInfoMut, Function};
use crate::compiler::{const_int, const_uint, Compiler, Value};
use crate::errors::*;
use crate::types::LangInt;
use crate::{ConstValue, Type};
use LangErrorMsg::{IndexOutOfBounds, IntegerOverflow};

/// Built-in function that returns a single component of a vector.
#[derive(Debug)]
pub struct Access {
    /// Index of the component of the vector to return. If this is None, then a
    /// second argument is used as the index.
    component_idx: Option<LangInt>,
    /// Error returned if the given component index is out of bounds.
    out_of_bounds_error: ErrorPointRef,
}
impl Access {
    /// Returns a constructor for a new Access instance that returns the
    /// component at the given index.
    pub fn with_component_idx(component_idx: Option<LangInt>) -> FuncConstructor {
        Box::new(move |info| {
            let arg_span = info
                .arg_types()
                .get(1)
                .map(|sp| sp.span)
                .unwrap_or(info.span);
            let out_of_bounds_error = info
                .userfunc
                .add_error_point(IndexOutOfBounds.with_span(arg_span));
            Ok(Box::new(Self {
                component_idx,
                out_of_bounds_error,
            }))
        })
    }
}
impl Function for Access {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        if self.component_idx.is_some() {
            info.check_args_len(1)?;
        } else {
            info.check_args_len(2)?;
            typecheck!(info.arg_types()[1], Int)?;
        }
        typecheck!(info.arg_types()[0], [Vector, Rectangle])?;
        match info.arg_types()[0].inner {
            Type::Vector(_) => Ok(Type::Int),
            Type::Rectangle(_) => Ok(Type::IntRange),
            _ => unreachable!(),
        }
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        let arg = args.compile(compiler, 0)?;

        // Get the length of the vector.
        let vector_len = match arg.ty() {
            Type::Vector(len) => len,
            Type::Rectangle(ndim) => ndim,
            _ => uncaught_type_error!(),
        };

        // Get the index.
        let element_idx = match self.component_idx {
            Some(i) => const_uint(i as u64),
            None => args.compile(compiler, 1)?.as_int()?,
        };

        // Make sure that the index is not negative.
        let is_negative = compiler.builder().build_int_compare(
            IntPredicate::SLT, // Signed Less-Than
            element_idx,
            const_int(0),
            "idxNegativeCheck",
        );
        compiler.build_conditional(
            is_negative,
            |c| Ok(self.out_of_bounds_error.compile(c)),
            |_| Ok(()),
        )?;

        // Check whether that the index less than the length of the vector.
        let in_range = compiler.builder().build_int_compare(
            IntPredicate::SLT, // Signed Less-Than
            element_idx,
            const_uint(vector_len as u64),
            "idxUpperBoundCheck",
        );

        // Here's where we diverge depending on argument type.
        let component_value;
        let zero;
        match arg {
            Value::Vector(vector_arg) => {
                // Get the component of the vector (poison if index is out of range).
                component_value = compiler.builder().build_extract_element(
                    vector_arg,
                    element_idx,
                    "tryVecAccess",
                );

                // Return zero if the index is out of range.
                zero = component_value
                    .into_int_value()
                    .get_type()
                    .const_zero()
                    .into();
            }
            Value::Rectangle(rect_arg) => {
                // Get the start and end vectors.
                let (start_vec, end_vec) = compiler.build_split_rectangle(rect_arg);
                // Get the start component (poison if index is out of range).
                let start = compiler
                    .builder()
                    .build_extract_element(start_vec, element_idx, "tryRectStartAccess")
                    .into_int_value();
                // Get the end component (poison if index is out of range).
                let end = compiler
                    .builder()
                    .build_extract_element(end_vec, element_idx, "tryRectEndAccess")
                    .into_int_value();
                // Construct an integer range (poison if index is out of range).
                component_value = compiler.build_construct_range(start, end, None).into();

                // Return an integer range of 0 if the index is out of range.
                zero = compiler
                    .build_construct_range(const_int(0), const_int(0), None)
                    .into();
            }
            _ => uncaught_type_error!(),
        }

        // Return zero if the index is out of range.
        let ret = compiler
            .builder()
            .build_select(in_range, component_value, zero, "vecAccess");

        // Note that result of an LLVM 'select' instruction is only poisoned if
        // its condition is poisoned (obviously not the case here) or its
        // selected value is poisoned; the unselected value may be poisoned
        // without poisoning the result of 'select.'
        // https://llvm.org/docs/LangRef.html#poisonvalues

        Ok(Value::from_basic_value(&info.ret_type(), ret))
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        // Get the index.
        let idx: LangInt = match self.component_idx {
            Some(i) => i,
            None => args.const_eval(1)?.as_int()?,
        };
        // Make sure that the index is not negative.
        let idx: usize = idx
            .try_into()
            .map_err(|_| self.out_of_bounds_error.error())?;

        // Get the component or return zero if the index is too large.
        let ret = match args.const_eval(0)? {
            ConstValue::Vector(values) => ConstValue::Int(values.get(idx).copied().unwrap_or(0)),
            ConstValue::Rectangle(start, end) => {
                let start = start.get(idx).copied().unwrap_or(0);
                let end = end.get(idx).copied().unwrap_or(0);
                let step = ConstValue::infer_range_step(start, end);
                ConstValue::IntRange { start, end, step }
            }
            _ => uncaught_type_error!(),
        };

        Ok(ret)
    }
    fn as_assignable(&self, info: FuncCallInfo) -> Option<&dyn AssignableFunction> {
        if info.arg_values().can_assign(0) {
            Some(self)
        } else {
            None
        }
    }
}
impl AssignableFunction for Access {
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        value: Value,
        info: FuncCallInfo,
    ) -> LangResult<()> {
        let args = info.arg_values();
        let arg = args.compile(compiler, 0)?;

        // Get the length of the vector.
        let vector_len = match arg.ty() {
            Type::Vector(len) => len,
            Type::Rectangle(ndim) => ndim,
            _ => uncaught_type_error!(),
        };

        // Get the index.
        let element_idx = match self.component_idx {
            Some(i) => const_uint(i as u64),
            None => args.compile(compiler, 1)?.as_int()?,
        };

        // Make sure that the index is not negative.
        let is_negative = compiler.builder().build_int_compare(
            IntPredicate::SLT, // Signed Less-Than
            element_idx,
            const_int(0),
            "idxNegativeCheck",
        );
        compiler.build_conditional(
            is_negative,
            |c| Ok(self.out_of_bounds_error.compile(c)),
            |_| Ok(()),
        )?;
        // Make sure that the index is not too large.
        let is_too_large = compiler.builder().build_int_compare(
            IntPredicate::SGE, // Signed Greater-Than
            element_idx,
            const_uint(vector_len as u64),
            "idxUpperBoundCheck",
        );
        // If either condition occurs, return an error.
        let is_out_of_bounds =
            compiler
                .builder()
                .build_or(is_negative, is_too_large, "idxBoundsCheck");
        compiler.build_conditional(
            is_out_of_bounds,
            |c| Ok(self.out_of_bounds_error.compile(c)),
            |_| Ok(()),
        )?;

        // Here's where we diverge depending on argument type.
        let ret = match arg {
            Value::Vector(vector_arg) => {
                // Set the element.
                Value::Vector(compiler.builder().build_insert_element(
                    vector_arg,
                    value.into_basic_value()?,
                    element_idx,
                    "vecAssign",
                ))
            }
            Value::Rectangle(rect_arg) => {
                let range_value = value.as_int_range()?;
                // Split the start and end vectors.
                let (start_vec, end_vec) = compiler.build_split_rectangle(rect_arg);
                // Extract the new start component.
                let idx = const_uint(RangeProperty::Start as u64);
                let new_start =
                    compiler
                        .builder()
                        .build_extract_element(range_value, idx, "rangeStart");
                // Extract the new end component.
                let idx = const_uint(RangeProperty::End as u64);
                let new_end =
                    compiler
                        .builder()
                        .build_extract_element(range_value, idx, "rangeEnd");
                // Set the start component.
                let new_start_vec = compiler.builder().build_insert_element(
                    start_vec,
                    new_start,
                    element_idx,
                    "rectSetStart",
                );
                // Set the end component.
                let new_end_vec = compiler.builder().build_insert_element(
                    end_vec,
                    new_end,
                    element_idx,
                    "rectSetEnd",
                );
                // Construct a rectangle.
                Value::Rectangle(compiler.build_construct_rectangle(new_start_vec, new_end_vec))
            }
            _ => uncaught_type_error!(),
        };

        // Assign it.
        args.compile_assign(compiler, 0, ret)?;
        Ok(())
    }
}

/// Built-in function that returns either the sum or product of a vector.
#[derive(Debug)]
pub struct Reduce {
    /// How to reduce the vector.
    mode: SumOrProduct,
    /// Error returned if overflow occurs.
    overflow_error: ErrorPointRef,
}
impl Reduce {
    /// Returns a constructor for a new Reduce instance that computes the sum of
    /// a vector's components.
    pub fn sum() -> FuncConstructor {
        Self::with_mode(SumOrProduct::Sum)
    }
    /// Returns a constructor for a new Reduce instance that computes the
    /// product of a vector's components.
    pub fn product() -> FuncConstructor {
        Self::with_mode(SumOrProduct::Product)
    }
    /// Returns a constructor for a new Reduce instance with the given mode.
    fn with_mode(mode: SumOrProduct) -> FuncConstructor {
        Box::new(move |info| {
            let overflow_error = info.add_error_point(IntegerOverflow);
            Ok(Box::new(Self {
                mode,
                overflow_error,
            }))
        })
    }
}
impl Function for Reduce {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        if info.arg_types().len() != 1 {
            Err(info.invalid_args_err())?;
        }
        typecheck!(info.arg_types()[0], Vector)?;
        Ok(Type::Int)
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        // Extract all the elements.
        let args = info.arg_values();
        let arg = args.compile(compiler, 0)?.as_vector()?;
        let components = (0..arg.get_type().get_size())
            .map(|i| {
                let idx = const_uint(i as u64);
                compiler
                    .builder()
                    .build_extract_element(arg, idx, "")
                    .into_int_value()
            })
            .collect_vec();
        // Compute the sum/product.
        let mut components = components.into_iter();
        let initial = components.next().unwrap();
        let name = match self.mode {
            SumOrProduct::Sum => "sadd",     // Signed Add
            SumOrProduct::Product => "smul", // Signed Multiply
        };
        let result = components.try_fold(initial, |lhs, rhs| {
            compiler
                .build_checked_int_arithmetic(lhs, rhs, name, |c| {
                    Ok(self.overflow_error.compile(c))
                })
                .map(|i| i.into_int_value())
        })?;
        Ok(Value::Int(result))
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        let mut components = args.const_eval(0)?.as_vector()?.into_iter();
        let initial = components.next().unwrap();
        let fold_fn = match self.mode {
            SumOrProduct::Sum => LangInt::checked_add,
            SumOrProduct::Product => LangInt::checked_mul,
        };
        let result = components
            .try_fold(initial, fold_fn)
            .ok_or(self.overflow_error.error())?;
        Ok(ConstValue::Int(result))
    }
}

/// Built-in function that returns the minimum or maximum component of a vector.
#[derive(Debug)]
pub struct MinMax {
    /// Whether to take the minimum or the maximum of the vector.
    mode: MinMaxMode,
}
impl MinMax {
    /// Returns a constructor for a new MinMax instance that returns the maximum
    /// of a vector's components.
    pub fn max() -> FuncConstructor {
        Self::with_mode(MinMaxMode::Max)
    }
    /// Returns a constructor for a new MinMax instance that returns the minimum
    /// of a vector's components.
    pub fn min() -> FuncConstructor {
        Self::with_mode(MinMaxMode::Min)
    }
    /// Returns a constructor for a new MinMax instance with the given mode.
    fn with_mode(mode: MinMaxMode) -> FuncConstructor {
        Box::new(move |_info| Ok(Box::new(Self { mode })))
    }
}
impl Function for MinMax {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        if info.arg_types().len() != 1 {
            Err(info.invalid_args_err())?;
        }
        typecheck!(info.arg_types()[0], Vector)?;
        Ok(Type::Int)
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        let arg = args.compile(compiler, 0)?.into_basic_value()?;
        compiler
            .build_reduce(&self.mode.to_string(), arg)
            .map(Value::Int)
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        let iter = args.const_eval(0)?.as_vector()?.into_iter();
        Ok(ConstValue::Int(match self.mode {
            MinMaxMode::Max => iter.max().unwrap(),
            MinMaxMode::Min => iter.min().unwrap(),
        }))
    }
}

/// Built-in function that returns the length of a vector or the number of
/// dimensions of a rectangle.
#[derive(Debug)]
pub struct GetLen;
impl GetLen {
    /// Constructs a new GetLen instance.
    pub fn construct(_info: &mut FuncCallInfoMut) -> FuncResult {
        Ok(Box::new(Self))
    }
}
impl Function for GetLen {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        if info.arg_types().len() != 1 {
            Err(info.invalid_args_err())?;
        }
        typecheck!(info.arg_types()[0], [Vector, Rectangle])?;
        Ok(Type::Int)
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        let ret = match args.compile(compiler, 0)?.ty() {
            Type::Vector(len) => len,
            Type::Rectangle(ndim) => ndim,
            _ => uncaught_type_error!(),
        };
        Ok(Value::Int(const_uint(ret as u64)))
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        Ok(ConstValue::Int(match args.const_eval(0)?.ty() {
            Type::Vector(len) => len as LangInt,
            Type::Rectangle(ndim) => ndim as LangInt,
            _ => uncaught_type_error!(),
        }))
    }
}
