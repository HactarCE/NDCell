//! Functions and data structures for construction, representation, and
//! compilation/evaluation of the abstract syntax tree.
//!
//! Semantically, the AST consists of a top-level Rule node, which contains
//! UserFunction nodes, which contain Statement nodes, which contain Expr
//! ("expression") nodes. Expr nodes may contain other Expr nodes.
//!
//! Statement and Expr nodes are actually owned by the UserFunction, and they
//! are more often manipulated using StatementRef and ExprRef structs, which
//! contain an identifier that can be used to retrieve them from a UserFunction.
//! (UserFunction implements Index<StatementRef> and Index<ExprRef> for this.)

use std::convert::TryFrom;
use std::sync::Arc;

mod args;
mod expression;
mod rule;
pub mod statement;
mod userfunc;

pub use args::*;
pub use expression::*;
pub use rule::*;
pub use statement::{Statement, StatementBlock};
pub use userfunc::*;

use crate::errors::*;

pub fn make_rule(source_code: Arc<String>) -> LangResult<Rule> {
    let tokens = super::lexer::tokenize(&source_code)?;
    let parse_tree = super::parser::parse(source_code.clone(), &tokens)?;
    Rule::try_from(parse_tree)
}
