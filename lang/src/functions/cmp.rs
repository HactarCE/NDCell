//! Comparison functions.

use inkwell::values::IntValue;
use std::fmt;

use super::FuncConstructor;
use crate::ast::{ArgTypes, ArgValues, Function, FunctionKind};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::lexer::ComparisonToken;
use crate::{ConstValue, Span, Spanned, Type};
use LangErrorMsg::CmpError;

/// Built-in function that performs some fixed number of comparisons.
#[derive(Debug)]
pub struct Cmp {
    /// Types to compare (at least two).
    args: ArgTypes,
    /// Individual 2-input comparison functions (one less than the number of
    /// types).
    comparators: Vec<Comparator>,
}
impl Cmp {
    /// Returns a constructor for a new Cmp instance that compares each adjacent
    /// pair of the given arguments using the corresponding one of the given
    /// comparisons.
    pub fn with_comparisons(comparisons: Vec<ComparisonToken>) -> FuncConstructor {
        Box::new(move |_userfunc, _span, args| {
            let comparisons_iter = comparisons.iter();
            // Iterate over adjacent pairs.
            let type_pair_iter = args.windows(2);
            let comparators = comparisons_iter
                .zip(type_pair_iter)
                .map(|(&cmp, types)| Comparator::try_new(&types[0], cmp, &types[1]))
                .collect::<LangResult<Vec<_>>>()?;
            Ok(Box::new(Self { args, comparators }))
        })
    }
}
impl Function for Cmp {
    fn name(&self) -> String {
        format!("{:?} to {:?} comparison", self.args, self.comparators)
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Operator
    }

    fn arg_types(&self) -> ArgTypes {
        self.args.clone()
    }
    fn return_type(&self, _span: Span) -> LangResult<Type> {
        // We checked argument types in the constructor, so we don't need to
        // worry about doing that here.
        Ok(Type::Int)
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let old_bb = compiler.builder().get_insert_block().unwrap();

        // Build a basic block to skip to if the condition is false. The last
        // condition will have an unconditional jump.
        let merge_bb = compiler.append_basic_block("multiCompareShortCircuit");
        compiler.builder().position_at_end(merge_bb);

        // Create a phi node for the final result.
        let int_type = compiler.int_type();
        let phi = compiler.builder().build_phi(int_type, "multiCompareMerge");

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
                &int_type.const_zero(),
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
            &int_type.const_int(1, false),
            compiler.builder().get_insert_block().unwrap(),
        )]);

        // Position the builder at the end of the merge block for later
        // instructions.
        compiler.builder().position_at_end(merge_bb);

        // This phi node now contains 1 if all conditions were true and 0 if all
        // conditions were false.
        Ok(Value::Int(phi.as_basic_value().into_int_value()))
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let mut lhs = args.const_eval(0)?;
        for (rhs_arg_index, comparator) in (1_usize..).zip(&self.comparators) {
            let rhs = args.const_eval(rhs_arg_index)?;
            if let Some(const_eval_fn) = &comparator.const_eval {
                // It is possible to evaluate this comparison at compile time.
                if !(const_eval_fn)(lhs, rhs.clone())? {
                    // Short-circuit if any comparison returns false.
                    return Ok(Some(ConstValue::Int(0)));
                }
            } else {
                // It is not possible to evaluate this comparison at compile
                // time.
                return Ok(None);
            }
            // The current right-hand side becomes the next left-hand side.
            lhs = rhs;
        }
        // If all comparisons returned true, then return true.
        Ok(Some(ConstValue::Int(1)))
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
    fn try_new(lhs: &Spanned<Type>, cmp: ComparisonToken, rhs: &Spanned<Type>) -> LangResult<Self> {
        let span = Span::merge(lhs, rhs);
        let lhs = lhs.inner.clone();
        let rhs = rhs.inner.clone();
        let eq_only = cmp == ComparisonToken::Eql || cmp == ComparisonToken::Neq;
        match (lhs, rhs) {
            (Type::Int, Type::Int) => Ok(Self::int_cmp(Type::Int, cmp, true)),
            (Type::CellState, Type::CellState) if eq_only => {
                Ok(Self::int_cmp(Type::CellState, cmp, false))
            }
            (Type::Vector(len), Type::Int) | (Type::Int, Type::Vector(len)) => {
                // Coerce the integer to a vector of the same length.
                Ok(Self::vec_cmp(len, cmp))
            }
            (Type::Vector(len1), Type::Vector(len2)) => {
                // Extend the shorter vector to the length of the longer one.
                let len = std::cmp::max(len1, len2);
                Ok(Self::vec_cmp(len, cmp))
            }
            (Type::IntRange, Type::IntRange) if eq_only => Ok(Self::vec_cmp(3, cmp)),
            (Type::Rectangle(ndim1), Type::Rectangle(ndim2)) if eq_only => {
                // Add dimensions so that both rectangles have the same number of dimensions.
                let ndim = std::cmp::max(ndim1, ndim2);
                Ok(Self::rect_cmp(ndim, cmp))
            }
            (lhs, rhs) => Err(CmpError { lhs, cmp, rhs }.with_span(span)),
        }
    }

    /// Constructs a new comparator that compares two values of the same type
    /// represented internally by integers, signed or unsigned.
    fn int_cmp(ty: Type, cmp: ComparisonToken, signed: bool) -> Self {
        // Convert from ComparisonToken to inkwell::IntPredicate.
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
                _ => panic!("Invalid type for int_cmp()"),
            }),
        }
    }
    /// Constructs a new comparator that compares two signed integer or vector
    /// values by coercing both to vectors of the given length.
    fn vec_cmp(len: usize, cmp: ComparisonToken) -> Self {
        // Convert from ComparisonToken to inkwell::IntPredicate.
        let inkwell_predicate = cmp.inkwell_predicate(true);
        Self {
            compile: Box::new(move |compiler, lhs, rhs| {
                // Get vectors of integers.
                let lhs = match lhs {
                    Value::IntRange(v) => v,
                    _ => compiler.build_vector_cast(lhs, len)?,
                };
                let rhs = match rhs {
                    Value::IntRange(v) => v,
                    _ => compiler.build_vector_cast(rhs, len)?,
                };
                // Get a vector of booleans (result of comparison for each
                // component).
                let bool_vec =
                    compiler
                        .builder()
                        .build_int_compare(inkwell_predicate, lhs, rhs, "vectorCmp");
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
            "Cannot compare rectangles using ordered comparison operators"
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
}
