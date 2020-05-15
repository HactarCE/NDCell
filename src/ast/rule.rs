use std::convert::TryFrom;
use std::rc::Rc;

use super::super::errors::*;
use super::super::parser::{Directive, DirectiveContents, ParseTree};
use super::super::{ConstValue, Type, MAX_NDIM, MAX_STATES};
use super::UserFunction;
use LangErrorMsg::{
    Expected, InvalidDimensionCount, InvalidStateCount, MissingTransitionFunction, TypeError,
};

const DEFAULT_NDIM: u8 = 2;
const DEFAULT_STATES: &[CellState] = &[CellState, CellState];

pub struct Rule {
    meta: Rc<RuleMeta>,
    transition_function: UserFunction,
}
impl TryFrom<ParseTree> for Rule {
    type Error = LangError;
    fn try_from(parse_tree: ParseTree) -> LangResult<Self> {
        let mut temp_func = UserFunction::default();

        // Get number of dimensions.
        let ndim = match parse_tree.get_single_directive(Directive::Dimensions)? {
            None => DEFAULT_NDIM,
            Some(DirectiveContents::Expr(expr)) => {
                let ndim_expr = temp_func.build_expression_ast(expr)?;
                let ndim_value = temp_func.const_eval_expr(ndim_expr)?;
                match ndim_value {
                    ConstValue::Int(i @ 1..=MAX_NDIM) => i as u8,
                    ConstValue::Int(_) => Err(InvalidDimensionCount.with_span(expr))?,
                    _ => Err(TypeError {
                        expected: Type::Int,
                        got: ndim_value.ty(),
                    }
                    .with_span(expr.span))?,
                }
            }
            Some(DirectiveContents::Block(block)) => Err(Expected("expression").with_span(block))?,
        };

        // Get states.
        let states = match parse_tree.get_single_directive(Directive::States)? {
            None => DEFAULT_STATES.to_vec(),
            Some(DirectiveContents::Expr(expr)) => {
                let states_expr = temp_func.build_expression_ast(expr)?;
                let states_value = temp_func.const_eval_expr(states_expr)?;
                match states_value {
                    ConstValue::Int(i @ 1..=MAX_STATES) => vec![CellState::default(); i as usize],
                    ConstValue::Int(_) => Err(InvalidStateCount.with_span(expr))?,
                    _ => Err(TypeError {
                        expected: Type::Int,
                        got: states_value.ty(),
                    }
                    .with_span(expr.span))?,
                }
            }
            Some(DirectiveContents::Block(block)) => Err(Expected("expression").with_span(block))?,
        };

        let meta = Rc::new(RuleMeta { ndim, states });

        // Build transition function.
        let mut transition_function = UserFunction::new_transition_function(meta.clone());
        match parse_tree
            .get_single_directive(Directive::Transition)?
            .ok_or_else(|| MissingTransitionFunction.without_span())?
        {
            DirectiveContents::Block(statements) => {
                transition_function.build_statement_block_ast(&statements.inner)?;
            }
            DirectiveContents::Expr(expr) => {
                Err(Expected("code block beginning with '{'").with_span(expr))?;
            }
        }

        // Construct the rule.
        Ok(Rule {
            meta,
            transition_function,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuleMeta {
    /// Number of dimensions (from 1 to 6).
    pub ndim: u8,
    /// List of cell states.
    pub states: Vec<CellState>,
    // /// Cell state tags.
    // tags: HashMap<String, Tag>,
}
impl Default for RuleMeta {
    fn default() -> Self {
        Self {
            ndim: DEFAULT_NDIM,
            states: DEFAULT_STATES.to_vec(),
        }
    }
}
impl RuleMeta {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CellState;
