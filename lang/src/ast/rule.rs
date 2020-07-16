//! Root node of the AST.

use itertools::Itertools;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

use super::UserFunction;
use crate::errors::*;
use crate::parser::{Directive, DirectiveContents, HelperFunc, ParseTree};
use crate::types::LangInt;
use crate::{ConstValue, RuleMeta, Type, DEFAULT_NDIM};
use LangErrorMsg::{
    Expected, FunctionNameConflict, InvalidDimensionCount, InvalidStateCount, TypeError,
};

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
                const MAX_NDIM: LangInt = crate::MAX_NDIM as LangInt;
                match ndim_value {
                    // The user specified a valid dimension count.
                    ConstValue::Int(i @ 1..=MAX_NDIM) => i as u8,
                    // The user specified a number, but it's not a valid
                    // dimension count.
                    ConstValue::Int(_) => Err(InvalidDimensionCount.with_span(expr))?,
                    // The user specified some other value.
                    _ => Err(TypeError {
                        expected: Type::Int.into(),
                        got: ndim_value.ty(),
                    }
                    .with_span(expr.span))?,
                }
            }
            // The user gave something else instead of an expression.
            Some((span, _contents)) => Err(Expected("expression".to_owned()).with_span(span))?,
        };

        // Get states.
        let states = match parse_tree.take_single_directive(Directive::States)? {
            // There is no `@states` directive; use the default states.
            None => crate::meta::make_default_states(None),
            // There is an `@states` directive.
            Some((_span, DirectiveContents::Expr(expr))) => {
                let states_expr = temp_func.build_expression_ast(&expr)?;
                let states_value = temp_func.const_eval_expr(states_expr)?;
                const MAX_STATES: LangInt = crate::MAX_STATE_COUNT as LangInt;
                match states_value {
                    // The user specified a valid state count.
                    ConstValue::Int(i @ 1..=MAX_STATES) => {
                        crate::meta::make_default_states(Some(i as usize))
                    }
                    // The user specified a number, but it's not a valid state
                    // count.
                    ConstValue::Int(_) => Err(InvalidStateCount.with_span(expr))?,
                    // The user specified some other value.
                    _ => Err(TypeError {
                        expected: Type::Int.into(),
                        got: states_value.ty(),
                    }
                    .with_span(expr.span))?,
                }
            }
            // The user gave something else instead of an expression.
            Some((span, _contents)) => Err(Expected("expression".to_owned()).with_span(span))?,
        };

        let mut meta = RuleMeta {
            source_code: parse_tree.source_code.clone(),
            ndim,
            states,
            helper_function_signatures: HashMap::new(),
        };

        // Gather a list of helper functions.
        let helper_function_parse_trees: Vec<HelperFunc> = parse_tree
            .directives
            .remove(&Directive::Function)
            .unwrap_or_default()
            .into_iter()
            .map(|contents| match contents.inner {
                DirectiveContents::Func(f) => Ok(f),
                _ => internal_error!("Invalid parse tree on helper function"),
            })
            .try_collect()?;
        for helper_func in &helper_function_parse_trees {
            if meta
                .helper_function_signatures
                .contains_key(&**helper_func.name.inner)
            {
                Err(FunctionNameConflict.with_span(helper_func.name.span))?;
            } else {
                meta.helper_function_signatures.insert(
                    String::clone(&*helper_func.name.inner),
                    helper_func.fn_signature(&meta),
                );
            }
        }

        // The rule metadata is set in stone at this point.
        let meta = Rc::new(meta);

        // Build helper functions.
        let helper_functions = helper_function_parse_trees
            .into_iter()
            .map(|helper_func| {
                Ok((
                    String::clone(&*helper_func.name.inner),
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
                Err(Expected("code block".to_owned()).with_span(span))?;
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
            internal_error!("Unused directive {:?}", dir);
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
