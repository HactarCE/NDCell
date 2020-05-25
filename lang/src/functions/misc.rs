//! Miscellaneous functions.

use crate::ast::{ArgValues, FnSignature, Function, FunctionKind, UserFunction};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::{Span, Type};

/// Built-in function that returns a fixed variable.
#[derive(Debug, Clone)]
pub struct GetVar {
    /// Name of variable to get.
    pub var_name: String,
    /// Type of this variable.
    pub var_type: Type,
}
impl GetVar {
    /// Returns a new GetVar instance that returns the value of the variable
    /// with the given name.
    pub fn try_new(userfunc: &mut UserFunction, span: Span, var_name: String) -> LangResult<Self> {
        let var_type = userfunc.try_get_var(span, &var_name)?;
        Ok(Self { var_name, var_type })
    }
}
impl Function for GetVar {
    fn name(&self) -> String {
        format!("variable {:?}", self.var_name)
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Atom
    }
    fn signatures(&self) -> Vec<FnSignature> {
        vec![FnSignature::atom(self.var_type)]
    }
    fn compile(&self, compiler: &mut Compiler, _args: ArgValues) -> LangResult<Value> {
        let var_ptr = compiler.vars()[&self.var_name].ptr;
        let value = compiler.builder().build_load(var_ptr, &self.var_name);
        Ok(Value::from_basic_value(self.var_type, value))
    }
}
