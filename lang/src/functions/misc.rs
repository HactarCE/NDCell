//! Miscellaneous functions.

use super::FuncConstructor;
use crate::ast::{ArgTypes, ArgValues, FnSignature, Function, FunctionKind};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::{Span, Type};
use LangErrorMsg::{InternalError, Unimplemented};

/// Built-in function that returns a fixed variable.
#[derive(Debug)]
pub struct GetVar {
    /// Name of variable to get.
    var_name: String,
    /// Type of this variable.
    var_type: Type,
}
impl GetVar {
    /// Returns a constructor for a new GetVar instance with the given variable
    /// name.
    pub fn with_name(var_name: String) -> FuncConstructor {
        Box::new(|userfunc, span, arg_types| {
            if !arg_types.is_empty() {
                Err(InternalError(
                    "Arguments passed to variable access function".into(),
                ))?;
            }
            let var_type = userfunc.try_get_var(span, &var_name)?;
            Ok(Box::new(Self { var_name, var_type }))
        })
    }
}
impl Function for GetVar {
    fn name(&self) -> String {
        format!("variable {:?}", self.var_name)
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Atom
    }

    fn arg_types(&self) -> crate::ast::ArgTypes {
        vec![]
    }
    fn return_type(&self, _span: Span) -> LangResult<Type> {
        // We checked argument types in the constructor, so we don't need to
        // worry about doing that here.
        Ok(self.var_type)
    }

    fn compile(&self, compiler: &mut Compiler, _args: ArgValues) -> LangResult<Value> {
        let var_ptr = compiler.vars()[&self.var_name].ptr;
        let value = compiler.builder().build_load(var_ptr, &self.var_name);
        Ok(Value::from_basic_value(self.var_type, value))
    }
}

/// Built-in function that calls a user-written helper function.
#[derive(Debug)]
pub struct CallUserFn {
    /// Name of user function to call.
    func_name: String,
    /// Types of arguments passed to the function.
    arg_types: ArgTypes,
    /// Signature of the function.
    signature: FnSignature,
    /// Error to give when the user tries to actually compile this.
    unimplemented_error: LangError,
}
impl CallUserFn {
    /// Returns a constructor for a new CallUserFn instance that calls the
    /// function with the given name.
    pub fn with_name(func_name: String) -> FuncConstructor {
        Box::new(|userfunc, span, arg_types| {
            let signature = userfunc
                .rule_meta()
                .helper_function_signatures
                .get(&func_name)
                .ok_or_else(|| InternalError("Cannot find user function".into()).without_span())?
                .clone();
            Ok(Box::new(Self {
                func_name,
                arg_types,
                signature,
                unimplemented_error: Unimplemented.with_span(span),
            }))
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

    fn arg_types(&self) -> crate::ast::ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        if self.signature.matches(&self.arg_types) {
            Ok(self.signature.ret)
        } else {
            Err(self.invalid_args_err(span))
        }
    }

    fn compile(&self, _compiler: &mut Compiler, _args: ArgValues) -> LangResult<Value> {
        Err(self.unimplemented_error.clone())
    }
}
