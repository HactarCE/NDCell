//! Root node of the AST.

use std::convert::TryFrom;
use std::rc::Rc;

use super::super::errors::*;
use super::super::parser::{Directive, DirectiveContents, ParseTree};
use super::super::{ConstValue, Type, MAX_NDIM, MAX_STATES};
use super::UserFunction;
use LangErrorMsg::{
    Expected, InvalidDimensionCount, InvalidStateCount, MissingTransitionFunction, TypeError,
};

/// Number of dimensions to use when the user doesn't specify.
const DEFAULT_NDIM: u8 = 2;
/// Number of states to use when the user doesn't specify.
const DEFAULT_STATE_COUNT: usize = 2;

/// Returns a list of cell states if the user does not specify, given an
/// optional number of states (defaults to DEFAULT_STATE_COUNT).
fn make_default_states(count: Option<usize>) -> Vec<CellState> {
    return vec![CellState::default(); count.unwrap_or(DEFAULT_STATE_COUNT)];
}

/// Root node of an abstract syntax tree representing a Rule, along with any
/// associated metadata (such as cell state information).
pub struct Rule {
    /// Metadata describing this rule.
    meta: Rc<RuleMeta>,
    /// Transition function used to simulate this rule.
    transition_function: UserFunction,
}
impl TryFrom<ParseTree> for Rule {
    type Error = LangError;
    fn try_from(parse_tree: ParseTree) -> LangResult<Self> {
        let mut temp_func = UserFunction::default();

        // Get number of dimensions.
        let ndim = match parse_tree.get_single_directive(Directive::Dimensions)? {
            // There is no `@dimensions` directive; use the default.
            None => DEFAULT_NDIM,
            // There is an `@dimensions` directive.
            Some(DirectiveContents::Expr(expr)) => {
                let ndim_expr = temp_func.build_expression_ast(expr)?;
                let ndim_value = temp_func.const_eval_expr(ndim_expr)?;
                match ndim_value {
                    // The user specified a valid dimension count.
                    ConstValue::Int(i @ 1..=MAX_NDIM) => i as u8,
                    // The user specified a number, but it's not a valid
                    // dimension count.
                    ConstValue::Int(_) => Err(InvalidDimensionCount.with_span(expr))?,
                    // The user specified some other value.
                    _ => Err(TypeError {
                        expected: Type::Int,
                        got: ndim_value.ty(),
                    }
                    .with_span(expr.span))?,
                }
            }
            // The user gave a block of code instead of an expression.
            Some(DirectiveContents::Block(block)) => Err(Expected("expression").with_span(block))?,
        };

        // Get states.
        let states = match parse_tree.get_single_directive(Directive::States)? {
            // There is no `@states` directive; use the default states.
            None => make_default_states(None),
            // There is an `@states` directive.
            Some(DirectiveContents::Expr(expr)) => {
                let states_expr = temp_func.build_expression_ast(expr)?;
                let states_value = temp_func.const_eval_expr(states_expr)?;
                match states_value {
                    // The user specified a valid state count.
                    ConstValue::Int(i @ 1..=MAX_STATES) => make_default_states(Some(i as usize)),
                    // The user specified a number, but it's not a valid state
                    // count.
                    ConstValue::Int(_) => Err(InvalidStateCount.with_span(expr))?,
                    // The user specified some other value.
                    _ => Err(TypeError {
                        expected: Type::Int,
                        got: states_value.ty(),
                    }
                    .with_span(expr.span))?,
                }
            }
            // The user gave a block of code instead of an expression.
            Some(DirectiveContents::Block(block)) => Err(Expected("expression").with_span(block))?,
        };

        let meta = Rc::new(RuleMeta { ndim, states });

        // Build transition function.
        let mut transition_function = UserFunction::new_transition_function(meta.clone());
        match parse_tree
            .get_single_directive(Directive::Transition)?
            .ok_or_else(|| MissingTransitionFunction.without_span())?
        {
            // The user gave a block of code.
            DirectiveContents::Block(statements) => {
                transition_function.build_statement_block_ast(&statements.inner)?;
            }
            // The user gave an expression as the transition function.
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

/// Metadata about a rule, such as the number of dimensions and a list of
/// possible cell states.
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
            states: make_default_states(None),
        }
    }
}
impl RuleMeta {
    /// Constructs a new RuleMeta with default values.
    pub fn new() -> Self {
        Self::default()
    }
}

/// A cell state.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CellState;
