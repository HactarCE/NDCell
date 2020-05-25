//! Comparison functions.

use inkwell::values::IntValue;
use std::fmt;

use crate::ast::{ArgValues, Args, FnSignature, Function, FunctionKind, UserFunction};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::lexer::ComparisonToken;
use crate::{ConstValue, Span, Spanned, Type};
use LangErrorMsg::CmpError;

/// Built-in function that performs some fixed number of comparisons.
#[derive(Debug)]
pub struct Cmp {
    /// Types to compare (at least two).
    types: Vec<Type>,
    /// Individual 2-input comparison functions (one less than the number of
    /// types).
    comparators: Vec<Comparator>,
}
impl Cmp {
    /// Constructs a new Cmp instance that compares each adjacent pair of the
    /// given arguments using the corresponding one of the given comparisons.
    ///
    /// This method checks the types of the arguments and returns an error if
    /// they cannot be compared using the given function.
    pub fn try_new(
        userfunc: &mut UserFunction,
        args: &Args,
        comparisons: Vec<ComparisonToken>,
    ) -> LangResult<Self> {
        let types: Vec<Spanned<Type>> = args
            .iter(userfunc)
            .map(|e| Spanned {
                span: e.span(),
                inner: e.return_type(),
            })
            .collect();
        let comparisons_iter = comparisons.iter();
        let type_pair_iter = types.iter().zip(types.iter().skip(1));
        let comparators = comparisons_iter
            .zip(type_pair_iter)
            .map(|(&cmp, (&lhs, &rhs))| Comparator::try_new(lhs, cmp, rhs))
            .collect::<LangResult<Vec<_>>>()?;
        Ok(Self {
            types: types.iter().map(|s| s.inner).collect(),
            comparators,
        })
    }
}
impl Function for Cmp {
    fn name(&self) -> String {
        format!("{:?} to {:?} comparison", self.types, self.comparators)
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Operator
    }
    fn signatures(&self) -> Vec<FnSignature> {
        vec![FnSignature::new(self.types.clone(), Type::Int)]
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
    fn try_new(lhs: Spanned<Type>, cmp: ComparisonToken, rhs: Spanned<Type>) -> LangResult<Self> {
        // TODO: convert integers to vectors for comparison
        let span = Span::merge(lhs, rhs);
        let lhs = lhs.inner;
        let rhs = rhs.inner;
        if lhs != rhs {
            return Err(CmpError { lhs, cmp, rhs }.with_span(span));
        }
        let ty = lhs;
        let eq_only = cmp == ComparisonToken::Eql || cmp == ComparisonToken::Neq;
        match ty {
            Type::Int => Ok(Self::int_cmp(ty, cmp, true)),
            Type::CellState if eq_only => Ok(Self::int_cmp(ty, cmp, false)),
            _ => Err(CmpError { lhs, cmp, rhs }.with_span(span)),
        }
    }

    /// Constructs a new comparator that compares two values of the same type
    /// represented internally by integers, signed or unsigned.
    fn int_cmp(ty: Type, cmp: ComparisonToken, signed: bool) -> Self {
        // Convert from ast::Cmp to inkwell::IntPredicate.
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
}
