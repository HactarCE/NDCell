use super::super::ast::{ArgValues, FnSignature, Function, UserFunction};
use super::super::compiler::{Compiler, Value};
use super::super::errors::*;
use super::super::{Span, Type};

#[derive(Debug, Clone)]
pub struct GetVar {
    pub var_name: String,
    pub var_type: Type,
}
impl GetVar {
    pub fn try_new(userfunc: &mut UserFunction, span: Span, var_name: String) -> LangResult<Self> {
        let var_type = userfunc.try_get_var(span, &var_name)?;
        Ok(Self { var_name, var_type })
    }
}
impl Function for GetVar {
    fn name(&self) -> String {
        format!("<var {:?}>", self.var_name)
    }
    fn is_method(&self) -> bool {
        false
    }
    fn signatures(&self) -> Vec<FnSignature> {
        vec![FnSignature::constant(self.var_type)]
    }
    fn compile(&self, compiler: &mut Compiler, _args: ArgValues) -> LangResult<Value> {
        let var_ptr = compiler.vars()[&self.var_name];
        let value = compiler.builder().build_load(var_ptr, &self.var_name);
        Ok(Value::from_basic_value(self.var_type, value))
    }
}
