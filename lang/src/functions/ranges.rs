//! Range functions.

pub use super::enums::RangeProperty;

use super::{FuncConstructor, FuncResult};
use crate::ast::{AssignableFunction, FuncCallInfo, FuncCallInfoMut, Function};
use crate::compiler::{const_uint, Compiler, Value};
use crate::errors::*;
use crate::{ConstValue, Type};

/// Built-in function that changes the step of an integer range and returns it.
#[derive(Debug)]
pub struct StepBy;
impl StepBy {
    /// Constructs a new StepBy instance.
    pub fn construct(_info: &mut FuncCallInfoMut) -> FuncResult {
        Ok(Box::new(Self))
    }
}
impl Function for StepBy {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        info.check_args_len(2)?;
        typecheck!(info.arg_types()[0], IntRange)?;
        typecheck!(info.arg_types()[1], Int)?;
        Ok(Type::IntRange)
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        let range = args.compile(compiler, 0)?.as_int_range()?;
        let new_step = args.compile(compiler, 1)?.as_int()?;
        let idx = const_uint(RangeProperty::Step as u64);
        let ret = compiler
            .builder()
            .build_insert_element(range, new_step, idx, "rangeWithNewStep");
        Ok(Value::IntRange(ret))
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        let (start, end, _old_step) = args.const_eval(0)?.as_int_range()?;
        let step = args.const_eval(1)?.as_int()?;
        Ok(ConstValue::IntRange { start, end, step })
    }
}

/// Built-in function that returns a simple property of an integer range.
#[derive(Debug)]
pub struct Access {
    /// Property of the range to return.
    prop: RangeProperty,
}
impl Access {
    /// Returns a constructor for a new Access instance that returns the given
    /// property.
    pub fn with_prop(prop: RangeProperty) -> FuncConstructor {
        Box::new(move |_info| Ok(Box::new(Self { prop })))
    }
}
impl Function for Access {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        info.check_args_len(1)?;
        match self.prop {
            RangeProperty::Start | RangeProperty::End => {
                typecheck!(info.arg_types()[0], [IntRange, Rectangle])?
            }
            RangeProperty::Step => typecheck!(info.arg_types()[0], IntRange)?,
        }
        match info.arg_types()[0].inner {
            Type::IntRange => Ok(Type::Int),
            Type::Rectangle(ndim) => Ok(Type::Vector(ndim)),
            _ => uncaught_type_error!(),
        }
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        let arg = args.compile(compiler, 0)?;
        let ret = match arg {
            Value::IntRange(range_value) => Value::Int({
                let idx = const_uint(self.prop as u64);
                compiler
                    .builder()
                    .build_extract_element(range_value, idx, info.name)
                    .into_int_value()
            }),
            Value::Rectangle(rect_arg) => Value::Vector({
                let (start, end) = compiler.build_split_rectangle(rect_arg);
                match self.prop {
                    RangeProperty::Start => start,
                    RangeProperty::End => end,
                    RangeProperty::Step => uncaught_type_error!(),
                }
            }),
            _ => uncaught_type_error!(),
        };
        Ok(ret)
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        let ret = match args.const_eval(0)? {
            ConstValue::IntRange { start, end, step } => ConstValue::Int(match self.prop {
                RangeProperty::Start => start,
                RangeProperty::End => end,
                RangeProperty::Step => step,
            }),
            ConstValue::Rectangle(start, end) => ConstValue::Vector(match self.prop {
                RangeProperty::Start => start,
                RangeProperty::End => end,
                RangeProperty::Step => uncaught_type_error!(),
            }),
            _ => uncaught_type_error!(),
        };
        Ok(ret)
    }
    fn as_assignable(&self, info: FuncCallInfo) -> Option<&dyn AssignableFunction> {
        if info.arg_values().can_assign(0) {
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
        value: Value,
        info: FuncCallInfo,
    ) -> LangResult<()> {
        let args = info.arg_values();
        let arg = args.compile(compiler, 0)?;
        let ret = match arg {
            Value::IntRange(range_arg) => Value::IntRange({
                let idx = const_uint(self.prop as u64);
                compiler.builder().build_insert_element(
                    range_arg,
                    value.as_int()?,
                    idx,
                    &format!("rangeAssign_{}", info.name),
                )
            }),
            Value::Rectangle(rect_arg) => Value::Rectangle(
                compiler
                    .builder()
                    .build_insert_value(
                        rect_arg,
                        value.as_vector()?,
                        self.prop as u32,
                        "rectSetProp",
                    )
                    .unwrap()
                    .into_struct_value(),
            ),
            _ => uncaught_type_error!(),
        };
        args.compile_assign(compiler, 0, ret)?;
        Ok(())
    }
}
