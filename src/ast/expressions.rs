use super::super::compiler::*;
use super::super::errors::*;
use super::super::{ConstValue, Span, Type};
use super::{ArgTypes, ArgValues, Args, UserFunction};
use LangErrorMsg::{CannotEvalAsConst, InternalError, InvalidArguments};

#[derive(Debug)]
pub struct Expr {
    span: Span,
    func: Box<dyn Function>,
    args: Args,
    return_type: Type,
}
impl Expr {
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn return_type(&self) -> Type {
        self.return_type
    }
    pub fn try_new(
        span: Span,
        userfunc: &mut UserFunction,
        func: Box<dyn Function>,
        args: Args,
    ) -> LangResult<Self> {
        let arg_types = args.types(userfunc);
        let signature = func
            .get_signature(&arg_types)
            .ok_or_else(|| InvalidArguments {
                name: func.name(),
                is_method: func.is_method(),
                expected: func.signatures().iter().map(|s| s.args.clone()).collect(),
                got: arg_types,
            })?;
        Ok(Self {
            span,
            func,
            args,
            return_type: signature.ret,
        })
    }
    pub fn compile(&self, compiler: &mut Compiler, userfunc: &UserFunction) -> LangResult<Value> {
        let ret_val = self.func.compile(compiler, self.args.values(userfunc))?;
        // Check return type.
        if ret_val.ty() == self.return_type() {
            Ok(ret_val)
        } else {
            Err(InternalError("Expression returned wrong type".into()).with_span(self.span()))
        }
    }
    pub fn const_eval(&self, userfunc: &UserFunction) -> LangResult<ConstValue> {
        let ret_val = self
            .func
            .const_eval(self.args.values(userfunc))
            .transpose() // Convert Result<Option<_>, _> to Option<Result<_, _>>
            .unwrap_or_else(|| Err(CannotEvalAsConst.with_span(self.span())))?;
        // Check return type.
        if ret_val.ty() == self.return_type() {
            Ok(ret_val)
        } else {
            Err(InternalError("Expression returned wrong type".into()).with_span(self.span()))
        }
    }
}

pub trait Function: std::fmt::Debug {
    fn name(&self) -> String;
    fn is_method(&self) -> bool; // note that panics may occur if is_method() returns true but signatures has an empty FnSignature
    fn signatures(&self) -> Vec<FnSignature>; // return empty vec if can't list signatures
    fn get_signature(&self, args: &ArgTypes) -> Option<FnSignature> {
        self.signatures().into_iter().find(|s| s.matches(args))
    }
    // compile() may panic if given bad types
    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value>;
    fn const_eval(&self, _args: ArgValues) -> LangResult<Option<ConstValue>> {
        Ok(None)
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FunctionType {
    Function,
    Method,
    Operator,
    Property,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnSignature {
    pub args: ArgTypes,
    pub ret: Type,
}
impl FnSignature {
    pub fn constant(ret: Type) -> Self {
        Self::new(vec![], ret)
    }
    pub fn property(self_type: Type, ret: Type) -> Self {
        Self::new(vec![self_type], ret)
    }
    pub fn new(args: impl Into<ArgTypes>, ret: Type) -> Self {
        let args = args.into();
        Self { args, ret }
    }
    pub fn matches(&self, args: &ArgTypes) -> bool {
        &self.args == args
    }
}
