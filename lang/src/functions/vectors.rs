//! Vector functions.

use inkwell::IntPredicate;
use std::convert::TryInto;

use super::{FuncConstructor, FuncResult};
use crate::ast::{ArgTypes, ArgValues, ErrorPointRef, Function, FunctionKind, UserFunction};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::types::{LangInt, MAX_VECTOR_LEN};
use crate::{ConstValue, Span, Type};
use LangErrorMsg::{IndexOutOfBounds, VectorTooBig};

/// Built-in function that constructs a vector from its arguments.
#[derive(Debug)]
pub struct BuildVec {
    /// Argument types (should be empty).
    arg_types: ArgTypes,
}
impl BuildVec {
    /// Constructs a new BuildVec instance.
    pub fn construct(_userfunc: &mut UserFunction, _span: Span, arg_types: ArgTypes) -> FuncResult {
        Ok(Box::new(Self { arg_types }))
    }
}
impl Function for BuildVec {
    fn name(&self) -> String {
        "vector literal".to_owned()
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Atom
    }

    fn arg_types(&self) -> crate::ast::ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        let len = self
            .arg_types
            .iter()
            .map(|ty| {
                ty.check_int_or_vec()?;
                Ok(match ty.inner {
                    Type::Int => 1,
                    Type::Vector(len) => len,
                    _ => unreachable!(),
                })
            })
            .sum::<LangResult<usize>>()?;
        if len > MAX_VECTOR_LEN {
            Err(VectorTooBig.with_span(span))
        } else {
            Ok(Type::Vector(len))
        }
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let mut components = vec![];
        for arg in args.compile_all(compiler)? {
            match arg {
                Value::Int(i) => components.push(i),
                Value::Vector(v) => {
                    for i in 0..v.get_type().get_size() {
                        let idx = compiler.int_type().const_int(i as u64, false);
                        components.push(
                            compiler
                                .builder()
                                .build_extract_element(v, idx, "")
                                .into_int_value(),
                        );
                    }
                }
                _ => unreachable!(),
            }
        }
        let mut ret = compiler
            .int_type()
            .vec_type(components.len() as u32)
            .get_undef();
        for (i, component) in components.into_iter().enumerate() {
            let idx = compiler.int_type().const_int(i as u64, false);
            ret = compiler
                .builder()
                .build_insert_element(ret, component, idx, "");
        }
        Ok(Value::Vector(ret))
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let mut components = vec![];
        for arg in args.const_eval_all()? {
            match arg {
                ConstValue::Int(i) => components.push(i),
                ConstValue::Vector(v) => components.extend_from_slice(&v),
                _ => unreachable!(),
            }
        }
        Ok(Some(ConstValue::Vector(components)))
    }
}

/// Built-in function that returns a single component of a vector.
#[derive(Debug)]
pub struct VecAccess {
    /// Vector from which to extract a component (should be
    /// vec![Type::Vector(_)]).
    arg_types: ArgTypes,
    /// Index of the component of the vector to return. If this is None, then a
    /// second argument is used as the index.
    component_idx: Option<LangInt>,
    ///
    out_of_bounds_error: ErrorPointRef,
}
impl VecAccess {
    /// Returns a constructor for a new VecAccess instance that returns the
    /// component at the given index.
    pub fn with_component_idx(component_idx: Option<LangInt>) -> FuncConstructor {
        Box::new(move |userfunc, span, arg_types| {
            let arg_span = arg_types.get(0).map(|sp| sp.span).unwrap_or(span);
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
impl Function for VecAccess {
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
            if self.arg_types.len() != 1 {
                Err(self.invalid_args_err(span))?;
            }
        } else {
            if self.arg_types.len() != 2 {
                Err(self.invalid_args_err(span))?;
            }
            self.arg_types[1].check_eq(Type::Int)?;
        }
        self.arg_types[0].check_vec()?;
        Ok(Type::Int)
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let zero = compiler.int_type().const_zero();

        // Get the index.
        let element_idx = match self.component_idx {
            Some(i) => compiler.int_type().const_int(i as u64, true),
            None => args.compile(compiler, 1)?.as_int()?,
        };
        let arg = args.compile(compiler, 0)?.as_vector()?;

        // Get the length of the vector.
        let len = arg.get_type().get_size();
        let len_value = compiler.int_type().const_int(len as u64, false);

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
}
