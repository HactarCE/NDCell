//! Functions that return literals.

use itertools::Itertools;

use super::{FuncConstructor, FuncResult};
use crate::ast::{FuncCallInfo, FuncCallInfoMut, Function};
use crate::compiler::{const_int, Compiler, Value};
use crate::errors::*;
use crate::types::{CellStateFilter, LangInt, Stencil, MAX_VECTOR_LEN};
use crate::{ConstValue, Type};
use LangErrorMsg::{MustBeConstant, VectorTooBig};

/// Built-in function that returns a fixed integer.
#[derive(Debug)]
pub struct Int(LangInt);
impl Int {
    /// Returns a constructor for a new Int instance that returns the given
    /// constant integer.
    pub fn with_value(i: LangInt) -> FuncConstructor {
        Box::new(move |_info| Ok(Box::new(Self(i))))
    }
}
impl Function for Int {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        if info.args.len() != 0 {
            internal_error!("Arguments passed to int literal function");
        }
        Ok(Type::Int)
    }
    fn compile(&self, _compiler: &mut Compiler, _info: FuncCallInfo) -> LangResult<Value> {
        Ok(Value::Int(const_int(self.0)))
    }
    fn const_eval(&self, _info: FuncCallInfo) -> LangResult<ConstValue> {
        Ok(ConstValue::Int(self.0))
    }
}

/// Built-in function that returns a fixed string.
#[derive(Debug)]
pub struct ConstStencil(Stencil);
impl ConstStencil {
    /// Returns a constructor for a new ConstStencil instance that returns the
    /// given constant stencil.
    pub fn with_value(s: Stencil) -> FuncConstructor {
        Box::new(move |_info| Ok(Box::new(Self(s))))
    }
}
impl Function for ConstStencil {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        if info.args.len() != 0 {
            internal_error!("Arguments passed to stencil literal function");
        }
        Ok(Type::Stencil)
    }
    fn compile(&self, _compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        Err(MustBeConstant.with_span(info.span))
    }
    fn const_eval(&self, _info: FuncCallInfo) -> LangResult<ConstValue> {
        Ok(ConstValue::Stencil(self.0.clone()))
    }
}

/// Built-in function that constructs a vector from its arguments.
#[derive(Debug)]
pub struct Vector;
impl Vector {
    /// Constructs a new Vector instance.
    pub fn construct(_info: &mut FuncCallInfoMut) -> FuncResult {
        Ok(Box::new(Self))
    }
}
impl Function for Vector {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        let mut is_rect = false;
        let len = info
            .arg_types()
            .iter()
            .map(|ty| {
                typecheck!(ty, [Int, Vector, IntRange, Rectangle])?;
                if matches!(ty.inner, Type::IntRange | Type::Rectangle(_)) {
                    is_rect = true;
                }
                Ok(match ty.inner {
                    Type::Int | Type::IntRange => 1,
                    Type::Vector(len) | Type::Rectangle(len) => len,
                    _ => unreachable!(),
                })
            })
            .sum::<LangResult<usize>>()?;
        if len > MAX_VECTOR_LEN {
            Err(VectorTooBig.with_span(info.span))
        } else {
            Ok(if is_rect {
                Type::Rectangle(len)
            } else {
                Type::Vector(len)
            })
        }
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();

        // Store both the 'start' and 'end' coordinates -- if we are
        // constructing a vector and not a rectangle, we'll just ignore one.
        let mut components: Vec<(_, _)> = vec![];

        // Collect components.
        for arg in args.compile_all(compiler)? {
            match arg {
                Value::Int(i) => components.push((i, i)),
                Value::Vector(v) => {
                    components.extend(compiler.build_split_vector(v).into_iter().map(|i| (i, i)))
                }
                Value::IntRange(r) => {
                    let (start, end, _step) = compiler.build_split_range(r);
                    components.push((start, end));
                }
                Value::Rectangle(r) => {
                    let (start, end) = compiler.build_split_rectangle(r);
                    let start = compiler.build_split_vector(start);
                    let end = compiler.build_split_vector(end);
                    components.extend(start.into_iter().zip(end));
                }
                _ => unreachable!(),
            }
        }

        // Construct return value.
        let start_components = components.iter().map(|&(start, _end)| start).collect_vec();
        let end_components = components.iter().map(|&(_start, end)| end).collect_vec();
        let ret = match info.ret_type() {
            Type::Vector(_) => Value::Vector(compiler.build_construct_vector(&start_components)),
            Type::Rectangle(_) => {
                let start = compiler.build_construct_vector(&start_components);
                let end = compiler.build_construct_vector(&end_components);
                Value::Rectangle(compiler.build_construct_rectangle(start, end))
            }
            _ => unreachable!(),
        };
        Ok(ret)
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();

        // Store both the 'start' and 'end' coordinates -- if we are
        // constructing a vector and not a rectangle, we'll just ignore one.
        let mut start_components: Vec<LangInt> = vec![];
        let mut end_components: Vec<LangInt> = vec![];

        // Collect components.
        for arg in args.const_eval_all()? {
            match arg {
                ConstValue::Int(i) => {
                    start_components.push(i);
                    end_components.push(i);
                }
                ConstValue::Vector(v) => {
                    start_components.extend_from_slice(&v);
                    end_components.extend_from_slice(&v);
                }
                ConstValue::IntRange { start, end, .. } => {
                    start_components.push(start);
                    end_components.push(end);
                }
                ConstValue::Rectangle(start, end) => {
                    start_components.extend(start);
                    end_components.extend(end)
                }
                _ => unreachable!(),
            }
        }

        // Construct return value.
        let ret = match info.ret_type() {
            Type::Vector(_) => ConstValue::Vector(start_components),
            Type::Rectangle(_) => ConstValue::Rectangle(start_components, end_components),
            _ => unreachable!(),
        };
        Ok(ret)
    }
}

/// Built-in function that constructs a range from its arguments that steps by 1
/// or -1 (depending on arguments).
#[derive(Debug)]
pub struct Range;
impl Range {
    /// Constructs a new Range instance.
    pub fn construct(_info: &mut FuncCallInfoMut) -> FuncResult {
        Ok(Box::new(Self))
    }
}
impl Function for Range {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        info.check_args_len(2)?;
        typecheck!(info.arg_types()[0], [Int, Vector, CellState])?;
        if info.arg_types()[0].inner == Type::CellState {
            typecheck!(info.arg_types()[1], CellState)?;
            return Ok(Type::CellStateFilter(
                info.userfunc.rule_meta().states.len(),
            ));
        }
        typecheck!(info.arg_types()[1], [Int, Vector])?;
        use Type::{Int, IntRange, Rectangle, Vector};
        match (&info.arg_types()[0].inner, &info.arg_types()[1].inner) {
            (Int, Int) => Ok(IntRange),
            (Vector(len), Int) | (Int, Vector(len)) => Ok(Rectangle(*len)),
            // Extend the shorter vector.
            (Vector(len1), Vector(len2)) => Ok(Rectangle(std::cmp::max(*len1, *len2))),
            _ => unreachable!(),
        }
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();

        // Compile the arguments.
        let arg1 = args.compile(compiler, 0)?;
        let arg2 = args.compile(compiler, 1)?;
        match info.ret_type() {
            Type::IntRange => {
                let start = arg1.as_int()?;
                let end = arg2.as_int()?;
                let step = None; // Infer from start and end.
                Ok(Value::IntRange(
                    compiler.build_construct_range(start, end, step),
                ))
            }
            Type::Rectangle(ndim) => {
                let start = compiler.build_vector_cast(arg1, *ndim)?;
                let end = compiler.build_vector_cast(arg2, *ndim)?;
                let ret = compiler.build_construct_rectangle(start, end);
                Ok(Value::Rectangle(ret))
            }
            Type::CellStateFilter(state_count) => {
                let start = arg1.clone().as_cell_state()?;
                let end = arg2.clone().as_cell_state()?;
                let up_to_start =
                    compiler.build_construct_cell_state_filter_below(*state_count, start)?;
                let up_to_end =
                    compiler.build_construct_cell_state_filter_below(*state_count, end)?;
                let range_missing_top = compiler.builder().build_xor(
                    up_to_start,
                    up_to_end,
                    "cellStateFilterRangeMissingTop",
                );
                // Bitwise OR in the start and end to make sure that the endpoints are included in the range.
                let only_start = compiler.build_cell_state_filter_cast(arg1, *state_count)?;
                let only_end = compiler.build_cell_state_filter_cast(arg2, *state_count)?;
                let ret = range_missing_top;
                let ret = compiler.builder().build_or(ret, only_start, "");
                let ret = compiler.builder().build_or(ret, only_end, "");
                Ok(Value::CellStateFilter(*state_count, ret))
            }
            _ => unreachable!(),
        }
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();

        let arg1 = args.const_eval(0)?;
        let arg2 = args.const_eval(1)?;
        let ret = match info.ret_type() {
            Type::IntRange => {
                let start = arg1.as_int()?;
                let end = arg2.as_int()?;
                let step = ConstValue::infer_range_step(start, end);
                ConstValue::IntRange { start, end, step }
            }
            Type::Rectangle(ndim) => {
                let start = arg1.coerce_to_vector(*ndim)?;
                let end = arg2.coerce_to_vector(*ndim)?;
                ConstValue::Rectangle(start, end)
            }
            Type::CellStateFilter(state_count) => {
                let start = arg1.as_cell_state()?;
                let end = arg2.as_cell_state()?;
                let mut ret = CellStateFilter::none(*state_count);
                for i in start..=end {
                    ret.set_bit(i, true);
                }
                ConstValue::CellStateFilter(ret)
            }
            _ => unreachable!(),
        };
        Ok(ret)
    }
}
