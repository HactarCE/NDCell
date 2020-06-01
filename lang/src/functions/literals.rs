//! Functions that return literals.

use super::FuncConstructor;
use crate::ast::{ArgTypes, ArgValues, Function, FunctionKind};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::types::LangInt;
use crate::{ConstValue, Type};
use LangErrorMsg::InternalError;

/// Built-in function that returns a fixed integer. This struct can be
/// constructed directly.
#[derive(Debug, Clone)]
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
        Ok(Value::Int(
            compiler.int_type().const_int(self.0 as u64, true),
        ))
    }
    fn const_eval(&self, _args: ArgValues) -> LangResult<Option<ConstValue>> {
        Ok(Some(ConstValue::Int(self.0)))
    }
}
