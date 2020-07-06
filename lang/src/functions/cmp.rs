//! Comparison functions.

use inkwell::values::{IntValue, VectorValue};
use std::fmt;

use super::{FuncConstructor, FuncResult};
use crate::ast::{FuncCallInfo, FuncCallInfoMut, Function};
use crate::compiler::{self, Compiler, Value};
use crate::errors::*;
use crate::lexer::ComparisonToken;
use crate::{ConstValue, Span, Type};
use LangErrorMsg::{CannotEvalAsConst, CmpError};

/// Built-in function that performs some fixed number of comparisons.
#[derive(Debug)]
pub struct Cmp {
    /// Individual 2-input comparison functions (one less than the number of
    /// types).
    comparators: Vec<Comparator>,
}
impl Cmp {
    /// Returns a constructor for a new Cmp instance that compares each adjacent
    /// pair of the given arguments using the corresponding one of the given
    /// comparisons.
    pub fn with_comparisons(comparisons: Vec<ComparisonToken>) -> FuncConstructor {
        Box::new(move |info| {
            let comparisons_iter = comparisons.iter();
            // Iterate over adjacent pairs of arguments.
            let arg_types = info.arg_types();
            let type_pair_iter = arg_types.windows(2).map(|types| (&types[0], &types[1]));
            let comparators = comparisons_iter
                .zip(type_pair_iter)
                .map(|(&cmp, (lhs, rhs))| {
                    let span = Span::merge(lhs, rhs);
                    let lhs = &lhs.inner;
                    let rhs = &rhs.inner;
                    Comparator::try_new(lhs, cmp, rhs).ok_or_else(|| {
                        CmpError {
                            lhs: lhs.clone(),
                            cmp,
                            rhs: rhs.clone(),
                        }
                        .with_span(span)
                    })
                })
                .collect::<LangResult<Vec<_>>>()?;
            Ok(Box::new(Self { comparators }))
        })
    }
}
impl Function for Cmp {
    fn return_type(&self, _info: &mut FuncCallInfoMut) -> LangResult<Type> {
        // We checked argument types in the constructor, so we don't need to
        // worry about doing that here.
        Ok(Type::Int)
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();

        let old_bb = compiler.builder().get_insert_block().unwrap();

        // Build a basic block to skip to if the condition is false. The last
        // condition will have an unconditional jump.
        let merge_bb = compiler.append_basic_block("multiCompareShortCircuit");
        compiler.builder().position_at_end(merge_bb);

        // Create a phi node for the final result.
        let phi = compiler
            .builder()
            .build_phi(compiler::types::int(), "multiCompareMerge");

        compiler.builder().position_at_end(old_bb);
        // Compile the first argument.
        let mut lhs = args.compile(compiler, 0)?;
        for (rhs_arg_index, comparator) in (1..).zip(&self.comparators) {
            // Compile the second argument of this comparison (which will be the
            // first argument of the next one).
            let rhs = args.compile(compiler, rhs_arg_index)?;
            // Compare the arguments.
            let compare_result = (comparator.compile)(compiler, lhs, rhs.clone())?;
            // If the condition is false, skip ahead to the merge and give the
            // phi node a value of 0. If it is true, continue on to check the
            // next condition.
            let next_bb = compiler.append_basic_block("compare");
            compiler
                .builder()
                .build_conditional_branch(compare_result, next_bb, merge_bb);
            phi.add_incoming(&[(
                &compiler.const_uint(0),
                compiler.builder().get_insert_block().unwrap(),
            )]);
            compiler.builder().position_at_end(next_bb);
            // The current right-hand side becomes the next left-hand side.
            lhs = rhs;
        }

        // After the last comparison, unconditionally jump directly to the merge
        // block and give the phi node a value of 1 because all conditions were
        // true.
        compiler.builder().build_unconditional_branch(merge_bb);
        phi.add_incoming(&[(
            &compiler.const_uint(1),
            compiler.builder().get_insert_block().unwrap(),
        )]);

        // Position the builder at the end of the merge block for later
        // instructions.
        compiler.builder().position_at_end(merge_bb);

        // This phi node now contains 1 if all conditions were true and 0 if all
        // conditions were false.
        Ok(Value::Int(phi.as_basic_value().into_int_value()))
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        let mut lhs = args.const_eval(0)?;
        for (rhs_arg_index, comparator) in (1_usize..).zip(&self.comparators) {
            let rhs = args.const_eval(rhs_arg_index)?;
            if let Some(const_eval_fn) = &comparator.const_eval {
                // It is possible to evaluate this comparison at compile time.
                if !(const_eval_fn)(lhs, rhs.clone())? {
                    // Short-circuit if any comparison returns false.
                    return Ok(ConstValue::Int(0));
                }
            } else {
                // It is not possible to evaluate this comparison at compile
                // time.
                return Err(CannotEvalAsConst.with_span(info.span));
            }
            // The current right-hand side becomes the next left-hand side.
            lhs = rhs;
        }
        // If all comparisons returned true, then return true.
        Ok(ConstValue::Int(1))
    }
}

/// Two-input comparison function.
struct Comparator {
    /// Function to produce LLVM IR that perfomr this comparison.
    compile: Box<dyn Fn(&mut Compiler, Value, Value) -> LangResult<IntValue<'static>>>,
    /// Function to evaluate this comparison (optional).
    const_eval: Option<Box<dyn Fn(ConstValue, ConstValue) -> LangResult<bool>>>,
}
impl fmt::Debug for Comparator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<comparator>")
    }
}
impl Comparator {
    /// Constructs a new comparator that compares the given types using
    /// the given comparison token (if possible).
    fn try_new(lhs: &Type, cmp: ComparisonToken, rhs: &Type) -> Option<Self> {
        use Type::*;
        let eq_only = cmp.is_eq_only();
        match (lhs, rhs) {
            (Int, Int) => Some(Self::int_cmp(Int, cmp)),
            (CellState, CellState) if eq_only => Some(Self::int_cmp(CellState, cmp)),
            (Vector(len), Int) | (Int, Vector(len)) => {
                // Coerce the integer to a vector of the same length.
                Some(Self::vec_cmp(*len, cmp))
            }
            (Vector(len1), Vector(len2)) => {
                // Extend the shorter vector to the length of the longer one.
                let len = std::cmp::max(len1, len2);
                Some(Self::vec_cmp(*len, cmp))
            }
            (IntRange, IntRange) if eq_only => Some(Self::vec_cmp(3, cmp)),
            (Rectangle(ndim), IntRange) | (IntRange, Rectangle(ndim)) if eq_only => {
                Some(Self::rect_cmp(*ndim, cmp))
            }
            (Rectangle(ndim1), Rectangle(ndim2)) if eq_only => {
                // Add dimensions so that both rectangles have the same number of dimensions.
                let ndim = std::cmp::max(ndim1, ndim2);
                Some(Self::rect_cmp(*ndim, cmp))
            }
            _ => None,
        }
    }

    /// Constructs a new comparator that compares two values of the same type
    /// (integer or cell state) represented internally by integers, signed or
    /// unsigned.
    fn int_cmp(ty: Type, cmp: ComparisonToken) -> Self {
        // Convert from ComparisonToken to inkwell::IntPredicate.
        let signed = match ty {
            Type::Int => true,
            Type::CellState => false,
            _ => panic!("Invalid type for int_cmp()"),
        };
        let inkwell_predicate = cmp.inkwell_predicate(signed);
        Self {
            compile: Box::new(move |compiler, lhs, rhs| {
                Ok(compiler.builder().build_int_compare(
                    inkwell_predicate,
                    lhs.into_basic_value()?.into_int_value(),
                    rhs.into_basic_value()?.into_int_value(),
                    "intCmp",
                ))
            }),
            const_eval: Some(match ty {
                Type::Int => Box::new(move |lhs, rhs| Ok(cmp.eval(lhs.as_int()?, rhs.as_int()?))),
                Type::CellState => Box::new(move |lhs, rhs| {
                    Ok(cmp.eval(lhs.as_cell_state()?, rhs.as_cell_state()?))
                }),
                _ => unreachable!(),
            }),
        }
    }
    /// Constructs a new comparator that compares two signed integer or vector
    /// values by coercing both to vectors of the given length.
    ///
    /// Also works on integer ranges by treating them as a vector [start, end,
    /// step].
    fn vec_cmp(len: usize, cmp: ComparisonToken) -> Self {
        // Convert from ComparisonToken to inkwell::IntPredicate.
        let inkwell_predicate = cmp.inkwell_predicate(true);
        Self {
            compile: Box::new(move |compiler, lhs, rhs| {
                // Get a vector of booleans, one for each component.
                let bool_vec = Self::compile_vec_cmp(compiler, len, inkwell_predicate, lhs, rhs)?;
                // Collapse that down to a single boolean.
                let result = compiler.build_reduce(
                    match cmp {
                        // Only one component has to be unequal to satisfy `!=`.
                        ComparisonToken::Neq => "or",
                        // For all the other comparisons, the condition must be true for all components.
                        _ => "and",
                    },
                    bool_vec.into(),
                )?;
                Ok(result)
            }),
            const_eval: Some(Box::new(move |lhs, rhs| {
                // Get vectors of integers.
                let lhs = lhs.coerce_to_vector(len)?;
                let rhs = rhs.coerce_to_vector(len)?;
                // Compare them componentwise, checking that all the comparisons
                // are true.
                let result = lhs.into_iter().zip(rhs).all(|(l, r)| cmp.eval(l, r));
                Ok(result)
            })),
        }
    }
    /// Constructs a new comparator that compares two rectangles of the same length.
    /// Only Eql and Neq are allowed.
    fn rect_cmp(ndim: usize, cmp: ComparisonToken) -> Self {
        assert!(
            matches!(cmp, ComparisonToken::Eql | ComparisonToken::Neq),
            "Cannot compare rectangles using ordered comparison operators",
        );
        let vec_cmp = Self::vec_cmp(ndim, cmp);
        Self {
            compile: Box::new(move |compiler, lhs, rhs| {
                // Cast to rectangles of the same dimensionality.
                let lhs = compiler.build_rectangle_cast(lhs, ndim)?;
                let rhs = compiler.build_rectangle_cast(rhs, ndim)?;
                // Separate the start and end corners of the rectangle.
                let (lhs_start, lhs_end) = compiler.build_split_rectangle(lhs);
                let (rhs_start, rhs_end) = compiler.build_split_rectangle(rhs);
                // Wrap those corners in Value::Vector().
                let lhs_start = Value::Vector(lhs_start);
                let lhs_end = Value::Vector(lhs_end);
                let rhs_start = Value::Vector(rhs_start);
                let rhs_end = Value::Vector(rhs_end);
                // Delegate to Self::vec_cmp().
                let start_cmp_result = (vec_cmp.compile)(compiler, lhs_start, rhs_start)?;
                let end_cmp_result = (vec_cmp.compile)(compiler, lhs_end, rhs_end)?;
                // Combine results.
                match cmp {
                    // Both pairs of corners must be equal.
                    ComparisonToken::Eql => Ok(compiler.builder().build_and(
                        start_cmp_result,
                        end_cmp_result,
                        "rectangleCmp",
                    )),
                    // One pair of corners must be unequal.
                    ComparisonToken::Neq => Ok(compiler.builder().build_or(
                        start_cmp_result,
                        end_cmp_result,
                        "rectangleCmp",
                    )),
                    _ => unreachable!(),
                }
            }),
            const_eval: Some(Box::new(move |lhs, rhs| match cmp {
                // Rust can do this for us. :)
                ComparisonToken::Eql => Ok(lhs == rhs),
                ComparisonToken::Neq => Ok(lhs != rhs),
                _ => unreachable!(),
            })),
        }
    }

    /// Compiles a vector comparison but returns the result for each vector
    /// element separately rather than reducing them to a single boolean.
    fn compile_vec_cmp(
        compiler: &mut Compiler,
        len: usize,
        inkwell_predicate: inkwell::IntPredicate,
        lhs: Value,
        rhs: Value,
    ) -> LangResult<VectorValue<'static>> {
        // Get vectors of integers.
        let lhs = match lhs {
            Value::IntRange(v) => v,
            _ => compiler.build_vector_cast(lhs, len)?,
        };
        let rhs = match rhs {
            Value::IntRange(v) => v,
            _ => compiler.build_vector_cast(rhs, len)?,
        };
        // Return a vector of booleans (result of comparison for each
        // component).
        let bool_vec =
            compiler
                .builder()
                .build_int_compare(inkwell_predicate, lhs, rhs, "vectorCmp");
        Ok(bool_vec)
    }
}

/// Built-in function that checks for membership/matching.
#[derive(Debug, Copy, Clone)]
pub struct Is;
impl Is {
    /// Constructs a new Is instance.
    pub fn construct(_info: &mut FuncCallInfoMut) -> FuncResult {
        Ok(Box::new(Self))
    }
}
impl Function for Is {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        let lhs = &info.arg_types()[0];
        let rhs = &info.arg_types()[1];
        // Check right-hand side first, then left-hand side.
        typecheck!(rhs, [IntRange, Rectangle])?;
        match rhs.inner {
            Type::IntRange => typecheck!(lhs, [Int, Vector])?,
            Type::Rectangle(_) => typecheck!(lhs, [Int, Vector])?,
            _ => unreachable!(),
        }
        // Always return a boolean.
        Ok(Type::Int)
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        let lhs = args.compile(compiler, 0)?;
        let rhs = args.compile(compiler, 1)?;
        match rhs {
            Value::IntRange(_) | Value::Rectangle(_) => {
                // Extract the start (a) and end (b) from the RHS
                // range/rectangle.
                let (a, b) = match rhs {
                    Value::IntRange(range_value) => {
                        let (start, end, _step) = compiler.build_split_range(range_value);
                        (Value::Int(start), Value::Int(end))
                    }
                    Value::Rectangle(rect_value) => {
                        let (start, end) = compiler.build_split_rectangle(rect_value);
                        (Value::Vector(start), Value::Vector(end))
                    }
                    _ => unreachable!(),
                };

                // Determine the type to cast to before comparing. Instead of
                // handling integers and vectors separately, just turn integers
                // into vectors of length 1.
                let target_len = match (lhs.ty(), rhs.ty()) {
                    (Type::Int, Type::IntRange) => 1,
                    (Type::Int, Type::Rectangle(ndim)) => ndim,
                    (Type::Vector(len), Type::IntRange) => len,
                    (Type::Vector(len), Type::Rectangle(ndim)) => std::cmp::max(len, ndim),
                    _ => uncaught_type_error!(),
                };
                let predicate = inkwell::IntPredicate::SLE; // Signed Less-Than or Equal

                // "Function alias" for convenience.
                let compile_vec_cmp = move |compiler: &'_ mut Compiler, l: &Value, r: &Value| {
                    // compile_vec_cmp() will handle any casting to vectors.
                    Comparator::compile_vec_cmp(
                        compiler,
                        target_len,
                        predicate,
                        l.clone(),
                        r.clone(),
                    )
                };

                // If `a <= lhs <= b` OR `b <= lhs <= a`, then return true.

                let a_lte_lhs = compile_vec_cmp(compiler, &a, &lhs)?;
                let lhs_lte_b = compile_vec_cmp(compiler, &lhs, &b)?;
                // a <= lhs <= b
                let cond1 = compiler
                    .builder()
                    .build_and(a_lte_lhs, lhs_lte_b, "rangeTestNormal");

                let b_lte_lhs = compile_vec_cmp(compiler, &b, &lhs)?;
                let lhs_lte_a = compile_vec_cmp(compiler, &lhs, &a)?;
                // b <= lhs <= a
                let cond2 = compiler
                    .builder()
                    .build_and(b_lte_lhs, lhs_lte_a, "rangeTestInverted");

                // a <= lhs <= b || b <= lhs <= a
                let cond3 = compiler.builder().build_or(cond1, cond2, "rangeTestResult");
                // Reduce to a single boolean.
                let reduced = compiler.build_reduce("and", cond3.into())?;

                // Cast boolean to integer.
                let ret = compiler.builder().build_int_z_extend(
                    reduced,
                    compiler::types::int(),
                    "rangeTestResultAsInt",
                );
                Ok(Value::Int(ret))
            }
            _ => uncaught_type_error!(),
        }
    }
}
