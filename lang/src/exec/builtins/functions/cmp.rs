//! Comparison functions.

use codemap::Spanned;

use crate::ast;
use crate::data::{self, CpVal, LangInt, RtVal};
use crate::errors::{Error, Result};
use crate::exec::Compiler;
use crate::llvm;

pub fn eval(op: ast::CompareOp, lhs: &Spanned<RtVal>, rhs: &Spanned<RtVal>) -> Result<bool> {
    let span = lhs.span.merge(rhs.span);

    let lhs_ty = lhs.ty();
    let rhs_ty = rhs.ty();

    let eql_or_neq = matches!(op, ast::CompareOp::Eql | ast::CompareOp::Neq);

    // Compare vectors (componentwise).
    if let Some((l, r)) = data::coerce_vectors_together(lhs, rhs, std::cmp::max) {
        return Ok(eval_vector_cmp(op, l, r));
    }

    match (&lhs.node, &rhs.node) {
        // Compare integers.
        (RtVal::Integer(l), RtVal::Integer(r)) => Ok(op.eval(l, r)),

        // Compare cells using only `==` and `!=`.
        (RtVal::Cell(l), RtVal::Cell(r)) if eql_or_neq => Ok(op.eval(l, r)),

        // TODO: compare more types
        _ => return Err(Error::cmp_type_error(span, op, &lhs_ty, &rhs_ty)),
    }
}

pub fn compile(
    compiler: &mut Compiler,
    op: ast::CompareOp,
    lhs: &Spanned<CpVal>,
    rhs: &Spanned<CpVal>,
) -> Result<llvm::IntValue> {
    let span = lhs.span.merge(rhs.span);

    let lhs_ty = lhs.ty();
    let rhs_ty = rhs.ty();

    let eql_or_neq = matches!(op, ast::CompareOp::Eql | ast::CompareOp::Neq);

    // Compare vectors (componentwise).
    if let Some((l, r)) = compiler.build_coerce_vectors_together(lhs, rhs, std::cmp::max) {
        return compile_for_int_math_values(compiler, op, l, r);
    }

    match [&lhs.node, &rhs.node] {
        // Compare integers.
        [CpVal::Integer(l), CpVal::Integer(r)] => compile_for_int_math_values(compiler, op, *l, *r),

        // Compare cells using only `==` and `!=`.
        [CpVal::Cell(l), CpVal::Cell(r)] if eql_or_neq => {
            compile_for_int_math_values(compiler, op, *l, *r)
        }

        // TODO: compare more types
        _ => Err(Error::cmp_type_error(span, op, &lhs_ty, &rhs_ty)),
    }
}

fn eval_vector_cmp(op: ast::CompareOp, mut lhs: Vec<LangInt>, mut rhs: Vec<LangInt>) -> bool {
    let len = std::cmp::max(lhs.len(), rhs.len());
    lhs.resize(len, 0);
    rhs.resize(len, 0);
    let mut pairs = lhs.into_iter().zip(rhs);
    if op == ast::CompareOp::Neq {
        pairs.any(|(l, r)| l != r)
    } else {
        pairs.all(|(l, r)| op.eval(l, r))
    }
}

fn compile_for_int_math_values<M: llvm::IntMathValue>(
    compiler: &mut Compiler,
    op: ast::CompareOp,
    lhs: M,
    rhs: M,
) -> Result<llvm::IntValue> {
    // Assume `lhs` and `rhs` have the same length.

    let predicate = llvm_predicate(op);
    match op {
        ast::CompareOp::Neq => compiler.build_all_cmp(predicate, lhs, rhs),
        _ => compiler.build_any_cmp(predicate, lhs, rhs),
    }
}

fn llvm_predicate(op: ast::CompareOp) -> llvm::IntPredicate {
    match op {
        ast::CompareOp::Eql => llvm::IntPredicate::EQ,
        ast::CompareOp::Neq => llvm::IntPredicate::NE,
        ast::CompareOp::Lt => llvm::IntPredicate::SLT,
        ast::CompareOp::Gt => llvm::IntPredicate::SGT,
        ast::CompareOp::Lte => llvm::IntPredicate::SLE,
        ast::CompareOp::Gte => llvm::IntPredicate::SGE,
    }
}
