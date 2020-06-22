//! Functions that return literals.

use super::{FuncConstructor, FuncResult};
use crate::ast::{ArgTypes, ArgValues, Function, FunctionKind, UserFunction};
use crate::compiler::{self, Compiler, Value};
use crate::errors::*;
use crate::types::{LangInt, TypeDesc, MAX_VECTOR_LEN};
use crate::{ConstValue, Span, Type};
use LangErrorMsg::VectorTooBig;

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
                internal_error!("Arguments passed to int literal function");
            }
            Ok(Box::new(Self(i)))
        })
    }
}
impl Function for Int {
    fn name(&self) -> String {
        format!("constant {}", Type::Int)
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
        format!("{} literal", TypeDesc::Vector)
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
                typecheck!(ty, [Int, Vector])?;
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
        let mut ret = compiler::types::vec(components.len()).get_undef();
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
        typecheck!(self.arg_types[0], [Int, Vector])?;
        typecheck!(self.arg_types[1], [Int, Vector])?;
        use Type::{Int, IntRange, Rectangle, Vector};
        match (&self.arg_types[0].inner, &self.arg_types[1].inner) {
            (Int, Int) => Ok(IntRange),
            (Vector(len), Int) | (Int, Vector(len)) => Ok(Rectangle(*len)),
            // Extend the shorter vector.
            (Vector(len1), Vector(len2)) => Ok(Rectangle(std::cmp::max(*len1, *len2))),
            _ => unreachable!(),
        }
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        // Compile the arguments.
        let arg1 = args.compile(compiler, 0)?;
        let arg2 = args.compile(compiler, 1)?;
        match self.unwrap_return_type() {
            Type::IntRange => {
                let start = arg1.as_int()?;
                let end = arg2.as_int()?;
                let step = None; // Infer from start and end.
                Ok(Value::IntRange(
                    compiler.build_construct_range(start, end, step),
                ))
            }
            Type::Rectangle(ndim) => {
                let start = compiler.build_vector_cast(arg1, ndim)?;
                let end = compiler.build_vector_cast(arg2, ndim)?;
                let ret = compiler.build_construct_rectangle(start, end);
                Ok(Value::Rectangle(ret))
            }
            _ => unreachable!(),
        }
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let arg1 = args.const_eval(0)?;
        let arg2 = args.const_eval(1)?;
        let ret = match self.unwrap_return_type() {
            Type::IntRange => {
                let start = arg1.as_int()?;
                let end = arg2.as_int()?;
                let step = ConstValue::infer_range_step(start, end);
                ConstValue::IntRange { start, end, step }
            }
            Type::Rectangle(ndim) => {
                let start = arg1.coerce_to_vector(ndim)?;
                let end = arg2.coerce_to_vector(ndim)?;
                ConstValue::Rectangle(start, end)
            }
            _ => unreachable!(),
        };
        Ok(Some(ret))
    }
}
