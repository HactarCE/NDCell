//! Functions that return literals.

use inkwell::IntPredicate;

use super::{FuncConstructor, FuncResult};
use crate::ast::{ArgTypes, ArgValues, Function, FunctionKind, UserFunction};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::types::{LangInt, MAX_VECTOR_LEN};
use crate::{ConstValue, Span, Type};
use LangErrorMsg::{InternalError, VectorTooBig};

/// Built-in function that returns a fixed integer. This struct can be
/// constructed directly.
#[derive(Debug)]
pub struct Int(LangInt);
impl Int {
    /// Returns a constructor for a new Int instance that returns the given
    /// constant integer.
    pub fn with_value(i: LangInt) -> FuncConstructor {
        Box::new(move |_userfunc, _span, arg_types| {
            if !arg_types.is_empty() {
                Err(InternalError(
                    "Arguments passed to int literal function".into(),
                ))?;
            }
            Ok(Box::new(Self(i)))
        })
    }
}
impl Function for Int {
    fn name(&self) -> String {
        "constant integer".to_owned()
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Atom
    }
    fn arg_types(&self) -> ArgTypes {
        vec![]
    }
    fn return_type(&self, _span: crate::Span) -> LangResult<Type> {
        // We checked argument types in the constructor, so we don't need to
        // worry about doing that here.
        Ok(Type::Int)
    }

    fn compile(&self, compiler: &mut Compiler, _args: ArgValues) -> LangResult<Value> {
        Ok(Value::Int(compiler.const_int(self.0)))
    }
    fn const_eval(&self, _args: ArgValues) -> LangResult<Option<ConstValue>> {
        Ok(Some(ConstValue::Int(self.0)))
    }
}

/// Built-in function that constructs a vector from its arguments.
#[derive(Debug)]
pub struct Vector {
    /// Argument types (should be empty).
    arg_types: ArgTypes,
}
impl Vector {
    /// Constructs a new Vector instance.
    pub fn construct(_userfunc: &mut UserFunction, _span: Span, arg_types: ArgTypes) -> FuncResult {
        Ok(Box::new(Self { arg_types }))
    }
}
impl Function for Vector {
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
                        let idx = compiler.const_uint(i as u64);
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
        let mut ret = compiler.vec_type(components.len()).get_undef();
        for (i, component) in components.into_iter().enumerate() {
            let idx = compiler.const_uint(i as u64);
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

/// Built-in function that constructs a range from its arguments that steps by 1
/// or -1 (depending on arguments).
#[derive(Debug)]
pub struct Range {
    /// Argument types (should have a length of 2).
    arg_types: ArgTypes,
}
impl Range {
    /// Constructs a new Range instance.
    pub fn construct(_userfunc: &mut UserFunction, _span: Span, arg_types: ArgTypes) -> FuncResult {
        Ok(Box::new(Self { arg_types }))
    }
}
impl Function for Range {
    fn name(&self) -> String {
        "range literal".to_owned()
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Operator
    }

    fn arg_types(&self) -> crate::ast::ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        self.check_args_len(span, 2)?;
        self.arg_types[0].check_eq(Type::Int)?;
        self.arg_types[1].check_eq(Type::Int)?;
        Ok(Type::IntRange)
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        // Compile the arguments.
        let start = args.compile(compiler, 0)?.as_int()?;
        let end = args.compile(compiler, 1)?.as_int()?;
        // Determine the step. If start <= end, then the default step is +1; if start >
        // end, then the default step is -1.
        let use_positive_step = compiler.builder().build_int_compare(
            IntPredicate::SLE, // Signed Less-Than or Equal
            start,
            end,
            "rangeStepTest",
        );
        let positive_one = compiler.const_int(1);
        let negative_one = compiler.const_int(-1);
        let step = compiler.builder().build_select(
            use_positive_step,
            positive_one,
            negative_one,
            "rangeStep",
        );
        // Combine
        let mut ret = compiler.int_range_type().get_undef();
        // Insert start.
        let idx = compiler.const_uint(0);
        ret = compiler
            .builder()
            .build_insert_element(ret, start, idx, "rangeTmp1");
        // Insert end.
        let idx = compiler.const_uint(1);
        ret = compiler
            .builder()
            .build_insert_element(ret, end, idx, "rangeTmp2");
        // Insert step.
        let idx = compiler.const_uint(2);
        ret = compiler
            .builder()
            .build_insert_element(ret, step, idx, "range");
        Ok(Value::IntRange(ret))
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
