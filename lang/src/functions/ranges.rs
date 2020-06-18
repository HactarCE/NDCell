//! Range functions.

use super::{FuncConstructor, FuncResult};
use crate::ast::{ArgTypes, ArgValues, AssignableFunction, Function, FunctionKind, UserFunction};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::{ConstValue, Span, Type};

/// Property of an integer range.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RangeProperty {
    Start = 0,
    End = 1,
    Step = 2,
}

/// Built-in function that changes the step of an integer range and returns it.
#[derive(Debug)]
pub struct StepBy {
    /// Range to return the property of and new step to use (should be
    /// vec![Type::IntRange, Type::Int]).
    arg_types: ArgTypes,
}
impl StepBy {
    /// Constructs a new StepBy instance.
    pub fn construct(_userfunc: &mut UserFunction, _span: Span, arg_types: ArgTypes) -> FuncResult {
        Ok(Box::new(Self { arg_types }))
    }
}
impl Function for StepBy {
    fn name(&self) -> String {
        "by".to_owned()
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Method
    }

    fn arg_types(&self) -> ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        self.check_args_len(span, 2)?;
        self.arg_types[0].check_eq(Type::IntRange)?;
        self.arg_types[1].check_eq(Type::Int)?;
        Ok(Type::IntRange)
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let range = args.compile(compiler, 0)?.as_int_range()?;
        let new_step = args.compile(compiler, 1)?.as_int()?;
        let idx = compiler.const_uint(RangeProperty::Step as u64);
        let ret = compiler
            .builder()
            .build_insert_element(range, new_step, idx, "rangeWithNewStep");
        Ok(Value::IntRange(ret))
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let (start, end, _old_step) = args.const_eval(0)?.as_int_range()?;
        let step = args.const_eval(1)?.as_int()?;
        Ok(Some(ConstValue::IntRange { start, end, step }))
    }
}

/// Built-in function that returns a simple property of an integer range.
#[derive(Debug)]
pub struct Access {
    /// Range to return the property of (should be vec![Type::IntRange]).
    arg_types: ArgTypes,
    /// Property of the range to return.
    prop: RangeProperty,
}
impl Access {
    /// Returns a constructor for a new Access instance that returns the given
    /// property.
    pub fn with_prop(prop: RangeProperty) -> FuncConstructor {
        Box::new(move |_userfunc, _span, arg_types| Ok(Box::new(Self { arg_types, prop })))
    }
}
impl Function for Access {
    fn name(&self) -> String {
        match self.prop {
            RangeProperty::Start => "start".to_owned(),
            RangeProperty::End => "end".to_owned(),
            RangeProperty::Step => "step".to_owned(),
        }
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Property
    }

    fn arg_types(&self) -> ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        self.check_args_len(span, 1)?;
        self.arg_types[0].check_eq(Type::IntRange)?;
        Ok(Type::Int)
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let range = args.compile(compiler, 0)?.as_int_range()?;
        let idx = compiler.const_uint(self.prop as u64);
        let ret = compiler
            .builder()
            .build_extract_element(range, idx, &self.name())
            .into_int_value();
        Ok(Value::Int(ret))
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let (start, end, step) = args.const_eval(0)?.as_int_range()?;
        let ret = match self.prop {
            RangeProperty::Start => start,
            RangeProperty::End => end,
            RangeProperty::Step => step,
        };
        Ok(Some(ConstValue::Int(ret)))
    }
    fn as_assignable(&self, args: &ArgValues) -> Option<&dyn AssignableFunction> {
        if args.can_assign(0) {
            Some(self)
        } else {
            None
        }
    }
}
impl AssignableFunction for Access {
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        args: ArgValues,
        value: Value,
    ) -> LangResult<()> {
        let arg = args.compile(compiler, 0)?.as_int_range()?;
        let idx = compiler.const_uint(self.prop as u64);
        let ret = compiler.builder().build_insert_element(
            arg,
            value.as_int()?,
            idx,
            &format!("rangeAssign_{}", self.name()),
        );
        args.compile_assign(compiler, 0, Value::IntRange(ret))?;
        Ok(())
    }
}
