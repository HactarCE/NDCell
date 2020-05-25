//! Functions that return literals.

use crate::ast::{ArgValues, FnSignature, Function, FunctionKind};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::types::LangInt;
use crate::{ConstValue, Type};

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
