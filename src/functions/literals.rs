use super::super::ast::{ArgValues, FnSignature, Function};
use super::super::compiler::{Compiler, Value};
use super::super::errors::*;
use super::super::types::LangInt;
use super::super::{ConstValue, Type};

#[derive(Debug, Clone)]
pub struct Int(pub LangInt);
impl Function for Int {
    fn name(&self) -> String {
        "<const int>".to_owned()
    }
    fn is_method(&self) -> bool {
        false
    }
    fn signatures(&self) -> Vec<FnSignature> {
        vec![FnSignature::constant(Type::Int)]
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
