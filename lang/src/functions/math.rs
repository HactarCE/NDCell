//! Math functions.

use inkwell::values::{BasicValueEnum, IntMathValue};
use inkwell::IntPredicate;
use std::convert::TryInto;

use super::FuncConstructor;
use crate::ast::{ArgTypes, ArgValues, ErrorPointRef, Function, FunctionKind};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::lexer::OperatorToken;
use crate::types::LangInt;
use crate::{ConstValue, Span, Type};
use LangErrorMsg::{DivideByZero, IntegerOverflow, InternalError, NegativeExponent};

/// Whether to perform negation or absolute value.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NegOrAbsMode {
    /// Unconditionally negate the input.
    Negate,
    /// Return the absolute value of the input (used as a function).
    AbsFunc,
    /// Return the absolute value of the input (used as a method).
    AbsMethod,
}

/// Built-in function that negates an integer or vector or takes its absolute value.
#[derive(Debug)]
pub struct NegOrAbs {
    /// Whether to perform absolute value or negation.
    mode: NegOrAbsMode,
    /// Type to negate (should have a length of 1).
    arg_types: ArgTypes,
    /// Error returned if overflow occurs.
    overflow_error: ErrorPointRef,
}
impl NegOrAbs {
    /// Returns a constructor for a new NegOrAbs instance with the given mode.
    pub fn with_mode(mode: NegOrAbsMode) -> FuncConstructor {
        Box::new(move |userfunc, span, arg_types| {
            Ok(Box::new(Self {
                overflow_error: userfunc.add_error_point(IntegerOverflow.with_span(span)),
                arg_types,
                mode,
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
    ) -> LangResult<Value> {
        // To negate a number, subtract it from zero.
        let negated = compiler.build_checked_int_arithmetic(zero, arg, "ssub", |c| {
            Ok(self.overflow_error.compile(c))
        })?;
        let result = match self.mode {
            NegOrAbsMode::Negate => negated,
            NegOrAbsMode::AbsFunc | NegOrAbsMode::AbsMethod => {
                // Select between the original argument and the value we just
                // computed depending on whether it was negative.
                let selector =
                    compiler
                        .builder()
                        .build_int_compare(IntPredicate::SLT, arg, zero, "absSelect");
                compiler.builder().build_select(
                    selector,
                    negated,
                    arg.as_basic_value_enum(),
                    "absResult",
                )
            }
        };
        Ok(Value::from_basic_value(self.arg_types[0].inner, result))
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
    fn name(&self) -> String {
        match self.mode {
            NegOrAbsMode::Negate => {
                format!("unary {:?} operator", OperatorToken::Minus.to_string())
            }
            NegOrAbsMode::AbsFunc | NegOrAbsMode::AbsMethod => "abs".to_string(),
        }
    }
    fn kind(&self) -> FunctionKind {
        match self.mode {
            NegOrAbsMode::Negate => FunctionKind::Operator,
            NegOrAbsMode::AbsFunc => FunctionKind::Function,
            NegOrAbsMode::AbsMethod => FunctionKind::Method,
        }
    }
    fn arg_types(&self) -> ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        match self.arg_types.as_slice() {
            [ty] => {
                ty.check_int_or_vec()?;
                Ok(ty.inner)
            }
            _ => Err(self.invalid_args_err(span)),
        }
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        // Call Self::compile_op() with a different generic type for integer vs.
        // vector.
        match args.compile(compiler, 0)? {
            Value::Int(arg) => self.compile_op(compiler, arg, arg.get_type().const_zero()),
            Value::Vector(arg) => self.compile_op(compiler, arg, arg.get_type().const_zero()),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let arg = args.const_eval(0)?;
        match arg {
            ConstValue::Int(i) => self.eval_op_int(i).map(ConstValue::Int),
            ConstValue::Vector(v) => v
                .into_iter()
                .map(|i| self.eval_op_int(i))
                .collect::<LangResult<Vec<LangInt>>>()
                .map(ConstValue::Vector),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
        .map(Some)
    }
}

/// Built-in function that performs a fixed two-input integer math operation.
#[derive(Debug)]
pub struct BinaryOp {
    /// Token signifying what operation to perform.
    op: OperatorToken,
    /// Types to apply the operation to (should have a length of 2).
    arg_types: ArgTypes,
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
        Box::new(move |userfunc, span, arg_types| {
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
                Some(userfunc.add_error_point(IntegerOverflow.with_span(span)))
            } else {
                None
            };
            let div_by_zero_error = if matches!(op, Slash | Percent) {
                Some(userfunc.add_error_point(DivideByZero.with_span(span)))
            } else {
                None
            };
            let negative_exponent_error = if matches!(op, DoubleAsterisk) {
                Some(userfunc.add_error_point(NegativeExponent.with_span(span)))
            } else {
                None
            };
            // Construct return value.
            Ok(Box::new(Self {
                op,
                arg_types,
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

    /// Returns the type of the result of this operation, based on the types of
    /// the arguments. Assumes arguments are valid.
    ///
    /// When performing an operation where one argument being zero causes an
    /// error (e.g. division) or always causes the result to be zero (e.g.
    /// multiplication, bitwise AND), the longer vector is truncated. When
    /// performing any other operation, the shorter vector is extended with
    /// zeros.
    fn result_type(&self) -> Type {
        let lhs = self.arg_types[0].inner;
        let rhs = self.arg_types[1].inner;
        match (lhs, rhs) {
            (Type::Int, other) | (other, Type::Int) => other,
            (Type::Vector(len1), Type::Vector(len2)) => {
                use OperatorToken::*;
                match self.op {
                    // Extend the shorter vector with zeros.
                    Plus | Minus | DoubleLessThan | DoubleGreaterThan | TripleGreaterThan
                    | Pipe | Caret => Type::Vector(std::cmp::max(len1, len2)),
                    // Truncate the longer vector.
                    Asterisk | Slash | Percent | DoubleAsterisk | Ampersand => {
                        Type::Vector(std::cmp::min(len1, len2))
                    }
                    DotDot | Tag => panic!("Uncaught invalid operator for binary op"),
                }
            }
            (_, _) => panic!("Uncaught invalid types for binary op"),
        }
    }

    /// Compiles this operation for two statically typed (integer vs. vector)
    /// LLVM values.
    fn compile_op<T: IntMathValue<'static>>(
        &self,
        compiler: &mut Compiler,
        lhs: T,
        rhs: T,
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
                match self.result_type() {
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
                    _ => Err(UNCAUGHT_TYPE_ERROR)?,
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
                match self.result_type() {
                    Type::Int => compiler.build_bitshift_int_check(
                        rhs.as_basic_value_enum().into_int_value(),
                        |c| Ok(self.overflow_error().compile(c)),
                    )?,
                    Type::Vector(_) => compiler.build_bitshift_vec_check(
                        rhs.as_basic_value_enum().into_vector_value(),
                        |c| Ok(self.overflow_error().compile(c)),
                    )?,
                    _ => Err(UNCAUGHT_TYPE_ERROR)?,
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
            DotDot | Tag => {
                Err(InternalError("Uncaught invalid operator for binary op".into()).without_span())?
            }
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
            _ => Err(InternalError("Uncaught invalid operator".into()).without_span())?,
        }
        // If the operation returned None, assume an IntegerOverflow error.
        .ok_or_else(|| self.overflow_error().error())
    }
}
impl Function for BinaryOp {
    fn name(&self) -> String {
        format!("binary {:?} operator", self.op.to_string())
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Operator
    }
    fn arg_types(&self) -> ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        match self.arg_types.as_slice() {
            [lhs, rhs] => {
                lhs.check_int_or_vec()?;
                rhs.check_int_or_vec()?;
                Ok(self.result_type())
            }
            _ => Err(self.invalid_args_err(span)),
        }
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let mut lhs = args.compile(compiler, 0)?;
        let mut rhs = args.compile(compiler, 1)?;
        // Cast to vectors of the proper length, if necessary.
        if let Type::Vector(len) = self.result_type() {
            lhs = Value::Vector(compiler.build_vector_cast(lhs, len)?);
            rhs = Value::Vector(compiler.build_vector_cast(rhs, len)?);
        }
        // Perform the operation.
        let result = match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => self.compile_op(compiler, lhs, rhs)?,
            (Value::Vector(lhs), Value::Vector(rhs)) => self.compile_op(compiler, lhs, rhs)?,
            _ => unreachable!(),
        };
        Ok(Value::from_basic_value(self.result_type(), result))
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let lhs = args.const_eval(0)?;
        let rhs = args.const_eval(1)?;
        if let Type::Vector(len) = self.result_type() {
            // Cast to vectors of the proper length, if necessary.
            let lhs = lhs.coerce_to_vector(len)?;
            let rhs = rhs.coerce_to_vector(len)?;
            // And perform the operation componentwise.
            let ret = lhs
                .into_iter()
                .zip(rhs)
                .map(|(l, r)| self.eval_op_int(l, r))
                .collect::<LangResult<Vec<LangInt>>>()?;
            Ok(Some(ConstValue::Vector(ret)))
        } else {
            // Otherwise use integers.
            self.eval_op_int(lhs.as_int()?, rhs.as_int()?)
                .map(ConstValue::Int)
                .map(Some)
        }
    }
}
