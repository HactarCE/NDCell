//! Vector functions.

use inkwell::IntPredicate;
use itertools::Itertools;
use std::convert::TryInto;

pub use super::enums::{MinMaxMode, SumOrProduct};
use super::{FuncConstructor, FuncResult};
use crate::ast::{
    ArgTypes, ArgValues, AssignableFunction, ErrorPointRef, Function, FunctionKind, UserFunction,
};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::types::LangInt;
use crate::{ConstValue, Span, Type};
use LangErrorMsg::{IndexOutOfBounds, IntegerOverflow};

/// Built-in function that returns a single component of a vector.
#[derive(Debug)]
pub struct Access {
    /// Vector from which to extract a component (should be
    /// vec![Type::Vector(_)]).
    arg_types: ArgTypes,
    /// Index of the component of the vector to return. If this is None, then a
    /// second argument is used as the index.
    component_idx: Option<LangInt>,
    ///
    out_of_bounds_error: ErrorPointRef,
}
impl Access {
    /// Returns a constructor for a new Access instance that returns the
    /// component at the given index.
    pub fn with_component_idx(component_idx: Option<LangInt>) -> FuncConstructor {
        Box::new(move |userfunc, span, arg_types| {
            let arg_span = arg_types.get(1).map(|sp| sp.span).unwrap_or(span);
            let out_of_bounds_error =
                userfunc.add_error_point(IndexOutOfBounds.with_span(arg_span));
            Ok(Box::new(Self {
                arg_types,
                component_idx,
                out_of_bounds_error,
            }))
        })
    }
}
impl Function for Access {
    fn name(&self) -> String {
        "vector access".to_owned()
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Property
    }

    fn arg_types(&self) -> ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        if self.component_idx.is_some() {
            self.check_args_len(span, 1)?;
        } else {
            self.check_args_len(span, 2)?;
            self.arg_types[1].check_eq(Type::Int)?;
        }
        self.arg_types[0].check_vec()?;
        Ok(Type::Int)
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let zero = compiler.const_int(0);

        // Get the index.
        let element_idx = match self.component_idx {
            Some(i) => compiler.const_uint(i as u64),
            None => args.compile(compiler, 1)?.as_int()?,
        };
        let arg = args.compile(compiler, 0)?.as_vector()?;

        // Get the length of the vector.
        let len = arg.get_type().get_size();
        let len_value = compiler.const_uint(len as u64);

        // Make sure that the index is not negative.
        let is_negative = compiler.builder().build_int_compare(
            IntPredicate::SLT, // Signed Less-Than
            element_idx,
            zero,
            "idxNegativeCheck",
        );
        compiler.build_conditional(
            is_negative,
            |c| Ok(self.out_of_bounds_error.compile(c)),
            |_| Ok(()),
        )?;

        // Get the component of the vector (poison if index is out of range).
        let component_value = compiler
            .builder()
            .build_extract_element(arg, element_idx, "tryVecAccess")
            .into_int_value();

        // Check whether that the index less than the length of the vector.
        let in_range = compiler.builder().build_int_compare(
            IntPredicate::SLT, // Signed Less-Than
            element_idx,
            len_value,
            "idxUpperBoundCheck",
        );

        // Return zero if the index is out of range.
        let ret = compiler
            .builder()
            .build_select(in_range, component_value, zero, "vecAccess")
            .into_int_value();

        // Note that result of an LLVM 'select' instruction is only poisoned if
        // its condition is poisoned (obviously not the case here) or its
        // selected value is poisoned; the unselected value may be poisoned
        // without poisoning the result of 'select.'
        // https://llvm.org/docs/LangRef.html#poisonvalues

        Ok(Value::Int(ret))
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
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
        Ok(Some(ConstValue::Int(
            args.const_eval(0)?
                .as_vector()?
                .get(idx)
                .copied()
                .unwrap_or(0),
        )))
    }
    fn as_assignable(&self, args: &ArgValues) -> Option<&dyn AssignableFunction> {
        if args.can_assign(0) {
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
        args: ArgValues,
        value: Value,
    ) -> LangResult<()> {
        let zero = compiler.const_int(0);

        // Get the index.
        let element_idx = match self.component_idx {
            Some(i) => compiler.const_uint(i as u64),
            None => args.compile(compiler, 1)?.as_int()?,
        };
        let arg = args.compile(compiler, 0)?.as_vector()?;

        // Get the length of the vector.
        let len = arg.get_type().get_size();
        let len_value = compiler.const_uint(len as u64);

        // Make sure that the index is not negative.
        let is_negative = compiler.builder().build_int_compare(
            IntPredicate::SLT, // Signed Less-Than
            element_idx,
            zero,
            "idxNegativeCheck",
        );
        // Make sure that the index is not too large.
        let is_too_large = compiler.builder().build_int_compare(
            IntPredicate::SGE, // Signed Greater-Than
            element_idx,
            len_value,
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

        // Set the element.
        let ret = compiler.builder().build_insert_element(
            arg,
            value.into_basic_value()?,
            element_idx,
            "vecAssign",
        );
        // Assign it.
        args.compile_assign(compiler, 0, Value::Vector(ret))?;
        Ok(())
    }
}

/// Built-in function that returns either the sum or product of a vector.
#[derive(Debug)]
pub struct Reduce {
    /// Vector to reduce (should be vec![Type::Vector(_)]).
    arg_types: ArgTypes,
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
        Box::new(move |userfunc, span, arg_types| {
            let overflow_error = userfunc.add_error_point(IntegerOverflow.with_span(span));
            Ok(Box::new(Self {
                arg_types,
                mode,
                overflow_error,
            }))
        })
    }
}
impl Function for Reduce {
    fn name(&self) -> String {
        format!("vector.{}", self.mode)
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Property
    }

    fn arg_types(&self) -> ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        if self.arg_types.len() != 1 {
            Err(self.invalid_args_err(span))?;
        }
        self.arg_types[0].check_vec()?;
        Ok(Type::Int)
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        // Extract all the elements.
        let arg = args.compile(compiler, 0)?.as_vector()?;
        let components = (0..arg.get_type().get_size())
            .map(|i| {
                let idx = compiler.const_uint(i as u64);
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
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let mut components = args.const_eval(0)?.as_vector()?.into_iter();
        let initial = components.next().unwrap();
        let fold_fn = match self.mode {
            SumOrProduct::Sum => LangInt::checked_add,
            SumOrProduct::Product => LangInt::checked_mul,
        };
        let result = components
            .try_fold(initial, fold_fn)
            .ok_or(self.overflow_error.error())?;
        Ok(Some(ConstValue::Int(result)))
    }
}

/// Built-in function that returns the minimum or maximum component of a vector.
#[derive(Debug)]
pub struct MinMax {
    /// Vector to get the minimum or maximum of (should be
    /// vec![Type::Vector(_)]).
    arg_types: ArgTypes,
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
        Box::new(move |_userfunc, _span, arg_types| Ok(Box::new(Self { arg_types, mode })))
    }
}
impl Function for MinMax {
    fn name(&self) -> String {
        format!(
            "{}.{}",
            self.arg_types[0].inner,
            match self.mode {
                MinMaxMode::Max => "max",
                MinMaxMode::Min => "min",
            }
        )
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Property
    }

    fn arg_types(&self) -> ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        if self.arg_types.len() != 1 {
            Err(self.invalid_args_err(span))?;
        }
        self.arg_types[0].check_vec()?;
        Ok(Type::Int)
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let arg = args.compile(compiler, 0)?.into_basic_value()?;
        compiler
            .build_reduce(&self.mode.to_string(), arg)
            .map(Value::Int)
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let iter = args.const_eval(0)?.as_vector()?.into_iter();
        Ok(Some(ConstValue::Int(match self.mode {
            MinMaxMode::Max => iter.max().unwrap(),
            MinMaxMode::Min => iter.min().unwrap(),
        })))
    }
}

/// Built-in function that returns the length of a vector.
#[derive(Debug)]
pub struct GetLen {
    /// Vector to return the length of (should be vec![Type::Vector(_)]).
    arg_types: ArgTypes,
}
impl GetLen {
    /// Constructs a new GetLen instance.
    pub fn construct(_userfunc: &mut UserFunction, _span: Span, arg_types: ArgTypes) -> FuncResult {
        Ok(Box::new(Self { arg_types }))
    }
}
impl Function for GetLen {
    fn name(&self) -> String {
        "vector.len".to_owned()
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Property
    }

    fn arg_types(&self) -> ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        if self.arg_types.len() != 1 {
            Err(self.invalid_args_err(span))?;
        }
        self.arg_types[0].check_vec()?;
        Ok(Type::Int)
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let ret = args
            .compile(compiler, 0)?
            .as_vector()?
            .get_type()
            .get_size();
        Ok(Value::Int(compiler.const_uint(ret as u64)))
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        Ok(Some(ConstValue::Int(
            args.const_eval(0)?.as_vector()?.len() as LangInt,
        )))
    }
}
