//! Vector functions.

use super::FuncResult;
use crate::ast::{ArgTypes, ArgValues, Function, FunctionKind, UserFunction};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::types::MAX_VECTOR_LEN;
use crate::{Span, Type};
use LangErrorMsg::VectorTooBig;

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
}
