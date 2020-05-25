//! Vector functions.

use inkwell::types::VectorType;

use crate::ast::{ArgValues, FnSignature, Function, FunctionKind, UserFunction};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::types::MAX_VECTOR_LEN;
use crate::{Span, Type};
use LangErrorMsg::VectorTooBig;

/// Built-in function that constructs a vector from its arguments.
#[derive(Debug, Clone)]
pub struct BuildVec {
    len: usize,
}
impl BuildVec {
    /// Returns a new BuildVec instance that constructs a vector of the given
    /// length.
    pub fn try_new(_userfunc: &mut UserFunction, span: Span, len: usize) -> LangResult<Self> {
        if len > MAX_VECTOR_LEN {
            Err(VectorTooBig.with_span(span))
        } else {
            Ok(Self { len })
        }
    }
}
impl Function for BuildVec {
    fn name(&self) -> String {
        format!("vec{} literal", self.len)
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Atom
    }
    fn signatures(&self) -> Vec<FnSignature> {
        vec![FnSignature::new(
            vec![Type::Int; self.len],
            Type::Vector(self.len),
        )]
    }
    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let args = (0..self.len)
            .map(|i| {
                Ok(args
                    .compile(compiler, i)?
                    .into_basic_value()?
                    .into_int_value())
            })
            .collect::<LangResult<Vec<_>>>()?;
        Ok(Value::Vector(VectorType::const_vector(&args)))
    }
}
