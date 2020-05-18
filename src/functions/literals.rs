//! Functions that return literals.

use super::super::ast::{ArgValues, FnSignature, Function, FunctionKind};
use super::super::compiler::{Compiler, Value};
use super::super::errors::*;
use super::super::types::LangInt;
use super::super::{ConstValue, Type};

/// Built-in function that returns a fixed integer. This struct can be
/// constructed directly.
#[derive(Debug, Clone)]
pub struct Int(pub LangInt);
impl Function for Int {
    fn name(&self) -> String {
        "constant integer".to_owned()
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Atom
    }
    fn signatures(&self) -> Vec<FnSignature> {
        vec![FnSignature::atom(Type::Int)]
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
