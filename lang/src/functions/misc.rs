//! Miscellaneous functions.

use crate::ast::{ArgValues, FnSignature, Function, FunctionKind, UserFunction};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::{Span, Type};
use LangErrorMsg::{InternalError, Unimplemented};

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

/// Built-in function that calls a user-written helper function.
#[derive(Debug, Clone)]
pub struct CallUserFn {
    /// Name of user function to call.
    func_name: String,
    /// Signature of functioin.
    signature: FnSignature,
    /// Error to give when the user tries to actually compile this.
    unimplemented_error: LangError,
}
impl CallUserFn {
    /// Returns a new CallUserFn instance that calls the function with name
    /// `func_name`.
    pub fn try_new(userfunc: &mut UserFunction, span: Span, func_name: String) -> LangResult<Self> {
        let signature = userfunc
            .rule_meta()
            .helper_function_signatures
            .get(&func_name)
            .ok_or_else(|| InternalError("Cannot find user function".into()).without_span())?
            .clone();
        Ok(Self {
            func_name,
            signature,
            unimplemented_error: Unimplemented.with_span(span),
        })
    }
}
impl Function for CallUserFn {
    fn name(&self) -> String {
        format!("helper function {:?}", self.func_name)
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Function
    }
    fn signatures(&self) -> Vec<FnSignature> {
        vec![self.signature.clone()]
    }
    fn compile(&self, _compiler: &mut Compiler, _args: ArgValues) -> LangResult<Value> {
        Err(self.unimplemented_error.clone())
    }
}
