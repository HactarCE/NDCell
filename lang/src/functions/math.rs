//! Math functions.

use inkwell::types::VectorType;
use inkwell::values::{BasicValueEnum, IntMathValue};
use inkwell::IntPredicate;
use itertools::Itertools;
use std::convert::TryInto;

pub use super::enums::{MinMaxMode, NegOrAbsMode};

use super::{FuncConstructor, FuncResult};
use crate::ast::{ErrorPointRef, FuncCallInfo, FuncCallInfoMut, Function};
use crate::compiler::{const_int, const_uint, Compiler, Value};
use crate::errors::*;
use crate::lexer::OperatorToken;
use crate::types::{CellStateFilter, LangInt, LangUint};
use crate::{ConstValue, Type};
use LangErrorMsg::{DivideByZero, IntegerOverflow, NegativeExponent};

/// Built-in function that negates an integer or vector or takes its absolute value.
#[derive(Debug)]
pub struct NegOrAbs {
    /// Whether to perform absolute value or negation.
    mode: NegOrAbsMode,
    /// Error returned if overflow occurs.
    overflow_error: ErrorPointRef,
}
impl NegOrAbs {
    /// Returns a constructor for a new NegOrAbs instance with the given mode.
    pub fn with_mode(mode: NegOrAbsMode) -> FuncConstructor {
        Box::new(move |info| {
            Ok(Box::new(Self {
                mode,
                overflow_error: info.add_error_point(IntegerOverflow),
            }))
        })
    }

    /// Compiles this operation for a statically typed (integer vs. vector) LLVM
    /// value.
    fn compile_op<T: IntMathValue<'static> + Copy>(
        &self,
        compiler: &mut Compiler,
        arg: T,
        zero: T,
    ) -> LangResult<BasicValueEnum<'static>> {
        // To negate a number, subtract it from zero.
        let negated = compiler.build_checked_int_arithmetic(zero, arg, "ssub", |c| {
            Ok(self.overflow_error.compile(c))
        })?;
        let result = match self.mode {
            NegOrAbsMode::Negate => negated,
            NegOrAbsMode::AbsFunc | NegOrAbsMode::AbsMethod => {
                // Select between the original argument and the value we just
                // computed depending on whether it was negative.
                let selector = compiler.builder().build_int_compare(
                    IntPredicate::SLT, // Signed Less-Than
                    arg,
                    zero,
                    "absSelect",
                );
                compiler.builder().build_select(
                    selector,
                    negated,
                    arg.as_basic_value_enum(),
                    "absResult",
                )
            }
        };
        Ok(result)
    }
    /// Evaluates this operation for an integer.
    pub fn eval_op_int(&self, arg: LangInt) -> LangResult<LangInt> {
        match self.mode {
            NegOrAbsMode::Negate => arg.checked_neg(),
            NegOrAbsMode::AbsFunc | NegOrAbsMode::AbsMethod => arg.checked_abs(),
        }
        .ok_or_else(|| self.overflow_error.error())
    }
}
impl Function for NegOrAbs {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        info.check_args_len(1)?;
        match self.mode {
            NegOrAbsMode::Negate => {
                typecheck!(info.arg_types()[0], [Int, Vector, IntRange, Rectangle])?
            }
            _ => typecheck!(info.arg_types()[0], [Int, Vector])?,
        };
        Ok(info.arg_types()[0].inner.clone())
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        // Call Self::compile_op() with a different generic type for integer vs.
        // vector.
        match args.compile(compiler, 0)? {
            Value::Int(arg) => Ok(Value::Int(
                self.compile_op(compiler, arg, arg.get_type().const_zero())?
                    .into_int_value(),
            )),
            Value::Vector(arg) | Value::IntRange(arg) => Ok(Value::from_basic_value(
                &info.arg_types()[0].inner,
                self.compile_op(compiler, arg, arg.get_type().const_zero())?,
            )),
            Value::Rectangle(arg) => {
                let (start, end) = compiler.build_split_rectangle(arg);
                let new_start = self
                    .compile_op(compiler, start, start.get_type().const_zero())?
                    .into_vector_value();
                let new_end = self
                    .compile_op(compiler, end, end.get_type().const_zero())?
                    .into_vector_value();
                let ret = compiler.build_construct_rectangle(new_start, new_end);
                Ok(Value::Rectangle(ret))
            }
            _ => uncaught_type_error!(),
        }
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        let arg = args.const_eval(0)?;
        match arg {
            ConstValue::Int(i) => self.eval_op_int(i).map(ConstValue::Int),
            ConstValue::Vector(v) => v
                .into_iter()
                .map(|i| self.eval_op_int(i))
                .try_collect()
                .map(ConstValue::Vector),
            ConstValue::IntRange { start, end, step } => [start, end, step]
                .iter()
                .map(|&i| self.eval_op_int(i))
                .collect::<LangResult<Vec<LangInt>>>()
                .map(|v| ConstValue::IntRange {
                    start: v[0],
                    end: v[1],
                    step: v[2],
                }),
            ConstValue::Rectangle(start, end) => Ok(ConstValue::Rectangle(
                start
                    .into_iter()
                    .map(|i| self.eval_op_int(i))
                    .try_collect()?,
                end.into_iter().map(|i| self.eval_op_int(i)).try_collect()?,
            )),
            _ => uncaught_type_error!(),
        }
    }
}

/// Built-in function that flips all the bits of an integer or vector, or
/// inverts a cell state filter.
#[derive(Debug)]
pub struct BitwiseNot;
impl BitwiseNot {
    pub fn construct(_info: &mut FuncCallInfoMut) -> LangResult<Box<dyn Function>> {
        Ok(Box::new(Self))
    }
}
impl Function for BitwiseNot {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        info.check_args_len(1)?;
        typecheck!(
            info.arg_types()[0],
            [Int, CellState, Vector, CellStateFilter]
        )?;
        if info.arg_types()[0].inner == Type::CellState {
            // Cell states will be cast to cell state filter.
            return Ok(Type::CellStateFilter(
                info.userfunc.rule_meta().states.len(),
            ));
        }
        // All other types will remain unchanged.
        Ok(info.arg_types()[0].inner.clone())
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        let mut arg = args.compile(compiler, 0)?;
        // Cast cell state to cell state filter.
        if arg.ty() == Type::CellState {
            let state_count = info.userfunc.rule_meta().states.len();
            let f = compiler.build_cell_state_filter_cast(arg, state_count)?;
            arg = Value::CellStateFilter(state_count, f);
        }
        match arg {
            Value::Int(i) => {
                let ret =
                    compiler
                        .builder()
                        .build_xor(i, i.get_type().const_all_ones(), "bitwiseNot");
                Ok(Value::Int(ret))
            }
            Value::CellState(_) => unreachable!(),
            Value::Vector(v) => {
                let vec_len = v.get_type().get_size() as usize;
                let all_ones = VectorType::const_vector(&vec![const_uint(LangUint::MAX); vec_len]);
                let ret = compiler.builder().build_xor(v, all_ones, "vecBitwiseNot");
                Ok(Value::Vector(ret))
            }
            Value::CellStateFilter(state_count, f) => {
                let all_ones = compiler
                    .value_from_const(ConstValue::CellStateFilter(!CellStateFilter::none(
                        state_count,
                    )))?
                    .as_cell_state_filter()?;
                let ret = compiler
                    .builder()
                    .build_xor(f, all_ones, "cellStateFilterInvert");
                Ok(Value::CellStateFilter(state_count, ret))
            }
            _ => uncaught_type_error!(),
        }
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        let mut arg = args.const_eval(0)?;
        // Cast cell state to cell state filter.
        if arg.ty() == Type::CellState {
            let state_count = info.userfunc.rule_meta().states.len();
            let f = arg.coerce_to_cell_state_filter(state_count)?;
            arg = ConstValue::CellStateFilter(f);
        }
        match arg {
            ConstValue::Int(i) => Ok(ConstValue::Int(!i)),
            ConstValue::CellState(_) => unreachable!(),
            ConstValue::Vector(v) => Ok(ConstValue::Vector(v.iter().map(|&x| !x).collect())),
            ConstValue::CellStateFilter(f) => Ok(ConstValue::CellStateFilter(!f)),
            _ => uncaught_type_error!(),
        }
    }
}

/// Built-in function that returns the minimum or maximum of several integers or
/// vectors (componentwise).
#[derive(Debug)]
pub struct MinMax {
    /// Whether to return the minimum or maximum.
    mode: MinMaxMode,
}
impl MinMax {
    /// Returns a constructor for a new MinMax instance that returns the maximum
    /// of its arguments.
    pub fn max() -> FuncConstructor {
        Self::with_mode(MinMaxMode::Max)
    }
    /// Returns a constructor for a new MinMax instance that returns the minimum
    /// of its arguments.
    pub fn min() -> FuncConstructor {
        Self::with_mode(MinMaxMode::Min)
    }
    /// Returns a constructor for a new MinMax instance with the given mode.
    fn with_mode(mode: MinMaxMode) -> FuncConstructor {
        Box::new(move |_info| Ok(Box::new(Self { mode })))
    }

    /// Compiles this operation for two values of the same type.
    fn compile_op(&self, compiler: &mut Compiler, arg1: Value, arg2: Value) -> LangResult<Value> {
        let ty = arg1.ty();
        let b = compiler.builder();
        let op = match self.mode {
            MinMaxMode::Max => IntPredicate::SGT,
            MinMaxMode::Min => IntPredicate::SLT,
        };
        use Value::{Int, Vector};
        let select_arg1: BasicValueEnum<'_> = match (&arg1, &arg2) {
            (Int(lhs), Int(rhs)) => b.build_int_compare(op, *lhs, *rhs, "selector").into(),
            (Vector(lhs), Vector(rhs)) => b.build_int_compare(op, *lhs, *rhs, "selector").into(),
            _ => internal_error!("Invalid arguments to MinMax::compile_op()"),
        };
        let ret = compiler.build_generic_select(
            select_arg1,
            arg1.into_basic_value()?,
            arg2.into_basic_value()?,
            "selectMinMax",
        );
        Ok(Value::from_basic_value(&ty, ret))
    }
    /// Evaluates this operation for two values of the same type.
    pub fn eval_op(&self, arg1: ConstValue, arg2: ConstValue) -> LangResult<ConstValue> {
        let f = match self.mode {
            MinMaxMode::Max => std::cmp::max,
            MinMaxMode::Min => std::cmp::min,
        };
        use ConstValue::{Int, Vector};
        let ret = match (arg1, arg2) {
            (Int(lhs), Int(rhs)) => ConstValue::Int(f(lhs, rhs)),
            (Vector(lhs), Vector(rhs)) => {
                ConstValue::Vector(lhs.into_iter().zip(rhs).map(|(a, b)| f(a, b)).collect())
            }
            _ => internal_error!("Invalid arguments to MinMax::eval_op()"),
        };
        Ok(ret)
    }
}
impl Function for MinMax {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        // At least two arguments are required.
        if info.arg_types().len() < 2 {
            Err(info.invalid_args_err())?;
        }
        // All arguments must be integers or vectors.
        for arg in &info.arg_types() {
            typecheck!(arg, [Int, Vector])?;
        }
        // The return type is the length of the longest argument.
        let mut ret_type = Type::Int;
        for arg in &info.arg_types() {
            if let Type::Vector(arg_len) = arg.inner {
                if let Type::Vector(ref mut ret_len) = &mut ret_type {
                    *ret_len = std::cmp::max(*ret_len, arg_len);
                } else {
                    ret_type = Type::Vector(arg_len);
                }
            }
        }
        Ok(ret_type)
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        // Cast all arguments to the return type.
        let ret_type = info.ret_type();
        let mut args = info
            .arg_values()
            .compile_all(compiler)?
            .into_iter()
            .map(|arg| {
                if let Type::Vector(len) = ret_type {
                    compiler.build_vector_cast(arg, *len).map(Value::Vector)
                } else {
                    Ok(arg)
                }
            });
        // Reduce the list, one at a time. This .unwrap() is safe because
        // return_type() checked that we have at least one argument.
        let initial = args.next().unwrap();
        // Collect the iterator to free compiler from the closure.
        let args = args.collect_vec();
        args.into_iter().fold(initial, |arg1, arg2| {
            self.compile_op(compiler, arg1?, arg2?)
        })
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        // Cast all arguments to the return type.
        let ret_type = info.ret_type();
        let mut args = info.arg_values().const_eval_all()?.into_iter().map(|arg| {
            if let Type::Vector(len) = ret_type {
                arg.coerce_to_vector(*len).map(ConstValue::Vector)
            } else {
                Ok(arg)
            }
        });
        // Reduce the list, one at a time.
        let initial = args.next().unwrap(); // return_type() checked that we have at least one argument.
        args.fold(initial, |arg1, arg2| self.eval_op(arg1?, arg2?))
    }
}

/// Built-in function that returns a number unchanged.
#[derive(Debug)]
pub struct UnaryPlus;
impl UnaryPlus {
    pub fn construct(_info: &mut FuncCallInfoMut) -> FuncResult {
        Ok(Box::new(Self))
    }
}
impl Function for UnaryPlus {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        info.check_args_len(1)?;
        typecheck!(info.arg_types()[0], [Int, Vector, IntRange, Rectangle])?;
        Ok(info.arg_types()[0].inner.clone())
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        args.compile(compiler, 0)
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        args.const_eval(0)
    }
}

/// Built-in function that performs a fixed two-input integer math operation.
#[derive(Debug)]
pub struct BinaryOp {
    /// Token signifying what operation to perform.
    op: OperatorToken,
    /// Error returned if overflow occurs.
    overflow_error: Option<ErrorPointRef>,
    /// Error returned if the divisor of an operation is negative.
    div_by_zero_error: Option<ErrorPointRef>,
    /// Error returned if the exponent is negative.
    negative_exponent_error: Option<ErrorPointRef>,
}
impl BinaryOp {
    /// Returns a constructor for a BinaryIntOp instance that performs the given
    /// operation.
    pub fn with_op(op: OperatorToken) -> FuncConstructor {
        Box::new(move |info| {
            // Construct errors.
            use OperatorToken::*;
            let overflow_error = if matches!(
                op,
                Plus | Minus
                    | Asterisk
                    | Slash
                    | Percent
                    | DoubleLessThan
                    | DoubleGreaterThan
                    | TripleGreaterThan
            ) {
                Some(info.add_error_point(IntegerOverflow))
            } else {
                None
            };
            let div_by_zero_error = if matches!(op, Slash | Percent) {
                Some(info.add_error_point(DivideByZero))
            } else {
                None
            };
            let negative_exponent_error = if matches!(op, DoubleAsterisk) {
                Some(info.add_error_point(NegativeExponent))
            } else {
                None
            };
            // Construct return value.
            Ok(Box::new(Self {
                op,
                overflow_error,
                div_by_zero_error,
                negative_exponent_error,
            }))
        })
    }

    /// Returns the OverflowError error point; panics if this function cannot
    /// return an Err(OverflowError).
    fn overflow_error(&self) -> &ErrorPointRef {
        self.overflow_error.as_ref().unwrap()
    }
    /// Returns the DivideByZero error point; panics if this function cannot
    /// return an Err(DivideByZero).
    fn div_by_zero_error(&self) -> &ErrorPointRef {
        self.div_by_zero_error.as_ref().unwrap()
    }
    /// Returns the NegativeExponent error point; panics if this function cannot
    /// return an Err(NegativeExponent).
    fn negative_exponent_error(&self) -> &ErrorPointRef {
        self.negative_exponent_error.as_ref().unwrap()
    }

    /// Compiles this operation for two statically typed (integer vs. vector)
    /// LLVM values.
    fn compile_op<T: IntMathValue<'static>>(
        &self,
        compiler: &mut Compiler,
        lhs: T,
        rhs: T,
        ret_type: &Type,
    ) -> LangResult<BasicValueEnum<'static>> {
        let b = compiler.builder();
        use OperatorToken::*;
        Ok(match self.op {
            // Addition, subtraction, and multiplication
            Plus | Minus | Asterisk => {
                let intrinsic = match self.op {
                    Plus => "sadd",     // Addition
                    Minus => "ssub",    // Subtraction
                    Asterisk => "smul", // Multiplication
                    _ => unreachable!(),
                };
                compiler.build_checked_int_arithmetic(lhs, rhs, intrinsic, |c| {
                    Ok(self.overflow_error().compile(c))
                })?
            }
            // Division and remainder (TODO: use euclidean div and modulo)
            Slash | Percent => {
                // Check for overflow and division by zero.
                match ret_type {
                    Type::Int => compiler.build_int_div_check(
                        lhs.as_basic_value_enum().into_int_value(),
                        rhs.as_basic_value_enum().into_int_value(),
                        |c| Ok(self.overflow_error().compile(c)),
                        |c| Ok(self.div_by_zero_error().compile(c)),
                    )?,
                    Type::Vector(_) => compiler.build_vec_div_check(
                        lhs.as_basic_value_enum().into_vector_value(),
                        rhs.as_basic_value_enum().into_vector_value(),
                        |c| Ok(self.overflow_error().compile(c)),
                        |c| Ok(self.div_by_zero_error().compile(c)),
                    )?,
                    _ => uncaught_type_error!(),
                };
                match self.op {
                    // Division
                    Slash => compiler.builder().build_int_signed_div(lhs, rhs, "tmp_div"),
                    // Remainder
                    Percent => compiler.builder().build_int_signed_rem(lhs, rhs, "tmp_rem"),
                    _ => unreachable!(),
                }
                .as_basic_value_enum()
            }
            // Exponentiation
            DoubleAsterisk => todo!("Exponentiation"),
            // Bitshift
            DoubleLessThan | DoubleGreaterThan | TripleGreaterThan => {
                // Check for overflow.
                match ret_type {
                    Type::Int => compiler.build_bitshift_int_check(
                        rhs.as_basic_value_enum().into_int_value(),
                        |c| Ok(self.overflow_error().compile(c)),
                    )?,
                    Type::Vector(_) => compiler.build_bitshift_vec_check(
                        rhs.as_basic_value_enum().into_vector_value(),
                        |c| Ok(self.overflow_error().compile(c)),
                    )?,
                    _ => uncaught_type_error!(),
                };
                match self.op {
                    // Bitshift left
                    DoubleLessThan => compiler
                        .builder()
                        .build_left_shift(lhs, rhs, "tmp_shl")
                        .as_basic_value_enum(),
                    // Bitshift right (arithmetic)
                    DoubleGreaterThan => compiler
                        .builder()
                        .build_right_shift(lhs, rhs, true, "tmp_shra")
                        .as_basic_value_enum(),
                    // Bitshift right (logical)
                    TripleGreaterThan => compiler
                        .builder()
                        .build_right_shift(lhs, rhs, false, "tmp_shrl")
                        .as_basic_value_enum(),
                    _ => unreachable!(),
                }
            }
            // Bitwise AND
            Ampersand => b.build_and(lhs, rhs, "tmp_and").as_basic_value_enum(),
            // Bitwise OR
            Pipe => b.build_or(lhs, rhs, "tmp_or").as_basic_value_enum(),
            // Bitwise XOR
            Caret => b.build_xor(lhs, rhs, "tmp_xor").as_basic_value_enum(),
            // Anything else
            Tilde | DotDot | Tag => internal_error!("Uncaught invalid operator for binary op"),
        })
    }
    /// Evaluates this operation for two integers.
    fn eval_op_int(&self, lhs: LangInt, rhs: LangInt) -> LangResult<LangInt> {
        use OperatorToken::*;
        // Perform the operation.
        match self.op {
            // Addition
            Plus => lhs.checked_add(rhs),
            // Subtraction
            Minus => lhs.checked_sub(rhs),
            // Multiplication
            Asterisk => lhs.checked_mul(rhs),
            // Division and remainder (TODO: use euclidean div and modulo)
            Slash | Percent => {
                if rhs == 0 {
                    self.div_by_zero_error().err()?
                } else {
                    match self.op {
                        Slash => lhs.checked_div(rhs),
                        Percent => lhs.checked_rem(rhs),
                        _ => unreachable!(),
                    }
                }
            }
            // Exponentiation
            DoubleAsterisk => {
                if rhs < 0 {
                    self.negative_exponent_error().err()?
                } else {
                    rhs.try_into().ok().map(|exp| lhs.pow(exp))
                }
            }
            // Bitshift left
            DoubleLessThan => rhs.try_into().ok().and_then(|rhs| lhs.checked_shl(rhs)),
            // Bitshift right (arithmetic)
            DoubleGreaterThan => rhs.try_into().ok().and_then(|rhs| lhs.checked_shr(rhs)),
            // Bitshift right (logical)
            TripleGreaterThan => rhs
                .try_into()
                .ok()
                .and_then(|rhs| (lhs as u64).checked_shr(rhs))
                .map(|i| i as LangInt),
            // Bitwise AND
            Ampersand => Some(lhs & rhs),
            // Bitwise OR
            Pipe => Some(lhs | rhs),
            // Bitwise XOR
            Caret => Some(lhs ^ rhs),
            // Anything else
            _ => internal_error!("Uncaught invalid operator"),
        }
        // If the operation returned None, assume an IntegerOverflow error.
        .ok_or_else(|| self.overflow_error().error())
    }
}
impl Function for BinaryOp {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        info.check_args_len(2)?;
        if matches!(info.arg_types()[0].inner, Type::IntRange | Type::Rectangle(_)) {
            // Ranges and rectangles only support addition, subtraction, and
            // multiplication by an integer (or vector, for rectangles).
            use OperatorToken::*;
            if !matches!(self.op, Plus | Minus | Asterisk) {
                Err(info.invalid_args_err())?;
            }
            if info.arg_types()[0].inner == Type::IntRange {
                typecheck!(info.arg_types()[1], Int)?;
            } else {
                typecheck!(info.arg_types()[1], [Int, Vector])?;
            }
        } else if matches!(
            info.arg_types()[0].inner,
            Type::CellState | Type::CellStateFilter(_)
        ) {
            // Cell states coerce to cell state filters, which support set
            // operations using the bitwise operators AND, OR, and XOR.
            use OperatorToken::*;
            if !matches!(self.op, Ampersand | Pipe | Caret) {
                Err(info.invalid_args_err())?;
            }
            typecheck!(info.arg_types()[1], [CellState, CellStateFilter])?;
            return Ok(Type::CellStateFilter(
                info.userfunc.rule_meta().states.len(),
            ));
        } else {
            // Otherwise, we expect an integer or vector.
            typecheck!(info.arg_types()[0], [Int, Vector])?;
            typecheck!(info.arg_types()[1], [Int, Vector])?;
        }
        // When performing an operation where one argument being zero causes an
        // error (e.g. division) or always causes the result to be zero (e.g.
        // multiplication, bitwise AND), the longer vector is truncated. When
        // performing any other operation, the shorter vector is extended with
        // zeros.
        let lhs = &info.arg_types()[0].inner;
        let rhs = &info.arg_types()[1].inner;
        match (lhs, rhs) {
            (Type::Int, other) | (other, Type::Int) => Ok(other.clone()),
            (Type::Vector(len1), Type::Vector(len2)) => {
                use OperatorToken::*;
                match self.op {
                    // Extend the shorter vector with zeros.
                    Plus | Minus | DoubleLessThan | DoubleGreaterThan | TripleGreaterThan
                    | Pipe | Caret => Ok(Type::Vector(std::cmp::max(*len1, *len2))),
                    // Truncate the longer vector.
                    Asterisk | Slash | Percent | DoubleAsterisk | Ampersand => {
                        Ok(Type::Vector(std::cmp::min(*len1, *len2)))
                    }
                    Tilde | DotDot | Tag => Err(info.invalid_args_err()),
                }
            }
            (Type::Rectangle(ndim1), Type::Vector(ndim2)) => {
                use OperatorToken::*;
                match self.op {
                    // Extend the shorter vector with zeros.
                    Plus | Minus => Ok(Type::Rectangle(std::cmp::max(*ndim1, *ndim2))),
                    // Truncate the longer vector.
                    Asterisk => Ok(Type::Rectangle(std::cmp::min(*ndim1, *ndim2))),
                    _ => Err(info.invalid_args_err()),
                }
            }
            (_, _) => Err(info.invalid_args_err()),
        }
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        let mut lhs = args.compile(compiler, 0)?;
        let mut rhs = args.compile(compiler, 1)?;
        let ret_type = info.ret_type();

        // Handle rectangle operations separately.
        if let Type::Rectangle(ndim) = ret_type {
            let (start, end) = compiler.build_split_rectangle(lhs.as_rectangle()?);
            let rhs = compiler.build_vector_cast(rhs, *ndim)?;
            let start = compiler.build_vector_cast(Value::Vector(start), *ndim)?;
            let end = compiler.build_vector_cast(Value::Vector(end), *ndim)?;
            let start = self
                .compile_op(compiler, start, rhs, ret_type)?
                .into_vector_value();
            let end = self
                .compile_op(compiler, end, rhs, ret_type)?
                .into_vector_value();
            let ret = compiler.build_construct_rectangle(start, end);
            return Ok(Value::Rectangle(ret));
        }
        // Cast to vectors of the proper length, if necessary.
        match &ret_type {
            Type::Vector(len) => {
                lhs = Value::Vector(compiler.build_vector_cast(lhs, *len)?);
                rhs = Value::Vector(compiler.build_vector_cast(rhs, *len)?);
            }
            Type::IntRange => {
                lhs = Value::Vector(lhs.as_int_range()?);
                rhs = Value::Vector(compiler.build_vector_cast(rhs, 3)?);
                // Preserve step during addition/subtraction.
                if matches!(self.op, OperatorToken::Plus | OperatorToken::Minus) {
                    let idx = const_uint(super::ranges::RangeProperty::Step as u64);
                    rhs = Value::Vector(compiler.builder().build_insert_element(
                        rhs.as_vector()?,
                        const_int(0),
                        idx,
                        "offsetWithoutStep",
                    ));
                }
            }
            Type::CellStateFilter(state_count) => {
                // Cell state filters essentially support bitwise operations, so
                // we can just treat them like normal vectors. We'll turn them
                // back into cell state filters with Value::from_basic_value()
                // at the end.
                lhs = Value::Vector(compiler.build_cell_state_filter_cast(lhs, *state_count)?);
                rhs = Value::Vector(compiler.build_cell_state_filter_cast(rhs, *state_count)?);
            }
            _ => (),
        }
        // Perform the operation.
        let result = match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => self.compile_op(compiler, lhs, rhs, ret_type)?,
            (Value::Vector(lhs), Value::Vector(rhs)) => {
                self.compile_op(compiler, lhs, rhs, ret_type)?
            }
            _ => uncaught_type_error!(),
        };
        Ok(Value::from_basic_value(&ret_type, result))
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        let lhs = args.const_eval(0)?;
        let rhs = args.const_eval(1)?;

        match info.ret_type() {
            Type::Rectangle(ndim) => {
                let (start, end) = lhs.as_rectangle()?;
                // Cast to vectors of the proper length.
                let start = ConstValue::Vector(start).coerce_to_vector(*ndim)?;
                let end = ConstValue::Vector(end).coerce_to_vector(*ndim)?;
                let rhs = rhs.coerce_to_vector(*ndim)?;
                // And perform the operation componentwise.
                let start = start
                    .into_iter()
                    .zip(&rhs)
                    .map(|(l, &r)| self.eval_op_int(l, r))
                    .try_collect()?;
                let end = end
                    .into_iter()
                    .zip(&rhs)
                    .map(|(l, &r)| self.eval_op_int(l, r))
                    .try_collect()?;
                Ok(ConstValue::Rectangle(start, end))
            }
            Type::Vector(len) => {
                // Cast to vectors of the proper length.
                let lhs = lhs.coerce_to_vector(*len)?;
                let rhs = rhs.coerce_to_vector(*len)?;
                // And perform the operation componentwise.
                let ret = lhs
                    .into_iter()
                    .zip(rhs)
                    .map(|(l, r)| self.eval_op_int(l, r))
                    .try_collect()?;
                Ok(ConstValue::Vector(ret))
            }
            Type::IntRange => todo!("add int range to int"),
            Type::Int => {
                // Otherwise use integers.
                self.eval_op_int(lhs.as_int()?, rhs.as_int()?)
                    .map(ConstValue::Int)
            }
            _ => unreachable!(),
        }
    }
}
