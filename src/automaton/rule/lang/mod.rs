use std::rc::Rc;

mod ast;
mod bytecode;
mod errors;
mod runtime;
mod span;
mod vars;

use super::{Rule, TransitionFunction};
use crate::automaton::space::*;
use bytecode::*;
use errors::*;
use runtime::Runtime;
use span::{Span, Spanned, TextPoint};
use vars::*;

const MAX_STEPS: usize = 100;
const DEFAULT_DIM: usize = 2;
const DEFAULT_RADIUS: usize = 1;

#[derive(Debug)]
pub struct RuleAttributes {
    dim: usize,
    radius: usize,
}
impl Default for RuleAttributes {
    fn default() -> Self {
        Self { dim: 2, radius: 1 }
    }
}

pub struct NdcaRuleGenerator {
    pub transition_fn: ast::Block,
    pub dim_expr: Option<Spanned<ast::Expr>>,
}
impl NdcaRuleGenerator {
    pub fn from_source(source_code: &str) -> LangResult<Self> {
        let ast = ast::make_ast(source_code)?;
        let mut transition_fn = None;
        let mut dim_expr = None;
        for directive in ast {
            match directive.inner {
                ast::Directive::Transition(block) => {
                    if transition_fn.is_some() {
                        lang_err(directive.span, "Too many transition functions; there must be exactly one transition function")?;
                    } else {
                        transition_fn = Some(block);
                    }
                }
            }
        }
        let transition_fn = transition_fn.ok_or(lang_error(
            Span::default(),
            "No transition function; there must be exactly one transition function",
        ))?;
        Ok(Self {
            transition_fn,
            dim_expr,
        })
    }
    pub fn eval_dim(&self) -> LangResult<usize> {
        if let Some(dim_expr) = &self.dim_expr {
            let function = &bytecode::compile_expr(dim_expr, &[], Some(Type::Int))?;
            let result = Runtime::new(function).run(MAX_STEPS)?;
            if let Value::Int(i) = result {
                Ok(i as usize)
            } else {
                panic!("Dimension expression returned something other than an integer");
            }
        } else {
            Ok(DEFAULT_DIM)
        }
    }
    pub fn make_rule<D: Dim>(&self) -> LangResult<NdcaRule<D>> {
        let dim = self.eval_dim()?;
        if D::NDIM != dim {
            panic!("Requested invalid rule");
        }
        let radius = DEFAULT_RADIUS;
        let transition_function = bytecode::compile_transition_function(&self.transition_fn)?;
        Ok(NdcaRule {
            phantom: std::marker::PhantomData,
            attr: RuleAttributes { dim, radius },
            transition_function,
        })
    }
}

#[derive(Debug)]
pub struct NdcaRule<D: Dim> {
    phantom: std::marker::PhantomData<D>,
    pub attr: RuleAttributes,
    pub transition_function: Function,
}
impl Rule<u8, Dim2D> for NdcaRule<Dim2D> {
    fn radius(&self) -> usize {
        self.attr.radius
    }
    fn get_transition_function(&self) -> TransitionFunction<u8, Dim2D> {
        let mut runtime = Runtime::new(&self.transition_function);
        Box::new(move |napkin| {
            let this = napkin[&NdVec::origin()];
            runtime.reset();
            runtime.set_args(&[Value::Pattern(napkin), Value::CellState(this as i64)]);
            let result = loop {
                if let Some(result) = runtime
                    .step()
                    .expect("Error occurred in transition function")
                {
                    break result;
                }
            };
            match result {
                Value::CellState(i) => (i & 255) as u8,
                Value::Void => this,
                _ => panic!(
                    "Transition function returned '{:?}' instead of a cell state",
                    result
                ),
            }
        })
    }
}
