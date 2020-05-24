//! Root node of the AST.

use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

use super::super::errors::*;
use super::super::parser::{Directive, DirectiveContents, HelperFunc, ParseTree};
use super::super::{ConstValue, Type, MAX_NDIM, MAX_STATES};
use super::{FnSignature, UserFunction};
use LangErrorMsg::{
    Expected, FunctionNameConflict, InternalError, InvalidDimensionCount, InvalidStateCount,
    TypeError,
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
#[derive(Debug)]
pub struct Rule {
    /// Metadata (e.g. source code, cell state information).
    meta: Rc<RuleMeta>,
    /// Helper functions.
    helper_functions: HashMap<String, UserFunction>,
    /// Transition function used to simulate this rule.
    transition_function: UserFunction,
}
impl TryFrom<ParseTree> for Rule {
    type Error = LangError;
    fn try_from(mut parse_tree: ParseTree) -> LangResult<Self> {
        let mut temp_func = UserFunction::default();

        // Get number of dimensions.
        let ndim = match parse_tree.take_single_directive(Directive::Dimensions)? {
            // There is no `@dimensions` directive; use the default.
            None => DEFAULT_NDIM,
            // There is an `@dimensions` directive.
            Some((_span, DirectiveContents::Expr(expr))) => {
                let ndim_expr = temp_func.build_expression_ast(&expr)?;
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
            // The user gave something else instead of an expression.
            Some((span, _contents)) => Err(Expected("expression").with_span(span))?,
        };

        // Get states.
        let states = match parse_tree.take_single_directive(Directive::States)? {
            // There is no `@states` directive; use the default states.
            None => make_default_states(None),
            // There is an `@states` directive.
            Some((_span, DirectiveContents::Expr(expr))) => {
                let states_expr = temp_func.build_expression_ast(&expr)?;
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
            // The user gave something else instead of an expression.
            Some((span, _contents)) => Err(Expected("expression").with_span(span))?,
        };

        // Gather a list of helper functions.
        let helper_function_parse_trees: Vec<HelperFunc> = parse_tree
            .directives
            .remove(&Directive::Function)
            .unwrap_or_default()
            .into_iter()
            .map(|contents| match contents.inner {
                DirectiveContents::Func(f) => Ok(f),
                _ => Err(
                    InternalError("Invalid parse tree on helper function".into()).without_span(),
                ),
            })
            .collect::<LangResult<Vec<_>>>()?;
        let mut helper_function_signatures = HashMap::new();
        for helper_func in &helper_function_parse_trees {
            if helper_function_signatures.contains_key(&helper_func.name.inner) {
                Err(FunctionNameConflict.with_span(helper_func.name.span))?;
            } else {
                helper_function_signatures.insert(
                    helper_func.name.inner.clone(),
                    FnSignature::from_helper_function_parse_tree(helper_func, ndim),
                );
            }
        }

        let meta = Rc::new(RuleMeta {
            source_code: parse_tree.source_code.clone(),
            ndim,
            states,
            helper_function_signatures,
        });

        // Build helper functions.
        let helper_functions = helper_function_parse_trees
            .into_iter()
            .map(|helper_func| {
                Ok((
                    helper_func.name.inner.clone(),
                    UserFunction::build_helper_function(&meta, helper_func)?,
                ))
            })
            .collect::<LangResult<HashMap<_, _>>>()?;

        // Build transition function.
        let mut transition_function = UserFunction::new_transition_function(meta.clone());
        match parse_tree.take_single_directive(Directive::Transition)? {
            // The user gave a block of code.
            Some((_span, DirectiveContents::Block(statements))) => {
                transition_function.build_top_level_statement_block_ast(&statements.inner)?;
            }
            // The user gave something else instead of a code block.
            Some((span, _contents)) => {
                Err(Expected("code block").with_span(span))?;
            }
            // The user did not provide a transition function.
            None => transition_function.build_top_level_statement_block_ast(&vec![])?,
        }

        // No directive left behind!
        if let Some((dir, _contents)) = parse_tree
            .directives
            .drain()
            .filter(|(_, v)| !v.is_empty())
            .next()
        {
            Err(InternalError(format!("Unused directive {:?}", dir).into()))?;
        }

        // Construct the rule.
        Ok(Rule {
            meta,
            helper_functions,
            transition_function,
        })
    }
}
impl Rule {
    /// Returns this rule's transition function.
    pub fn transition_function(&self) -> &UserFunction {
        &self.transition_function
    }
    /// Returns this rule's helper functions.
    pub fn helper_functions(&self) -> &HashMap<String, UserFunction> {
        &self.helper_functions
    }
}

/// Metadata about a rule, such as the number of dimensions and a list of
/// possible cell states.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuleMeta {
    /// Raw source code.
    pub source_code: Rc<String>,
    /// Number of dimensions (from 1 to 6).
    pub ndim: u8,
    /// List of cell states.
    pub states: Vec<CellState>,
    /// Map of names and signatures of helper functions.
    pub helper_function_signatures: HashMap<String, FnSignature>,
    // /// Cell state tags.
    // tags: HashMap<String, Tag>,
}
impl Default for RuleMeta {
    fn default() -> Self {
        Self {
            source_code: Rc::new(String::new()),
            ndim: DEFAULT_NDIM,
            states: make_default_states(None),
            helper_function_signatures: HashMap::new(),
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
