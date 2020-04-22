//! Methods and structures for creating and verifying an abstract syntax tree.
//!
//! This happens in several steps:
//!
//! 1. Split source code into list of tokens (ast::tokens)
//! 2. Create AST from tokens (ast::builder)
//!     - result is made from ast::components::{common, untyped}
//! 3. Typecheck AST (ast::typecheck)
//!     - result is made from ast::components::{common, typed}

use std::convert::{TryFrom, TryInto};

mod builder;
mod components;
mod tokens;
mod typecheck;

use super::errors::LangResult;
use super::span::Spanned;
pub use components::common::{self, *};
pub use components::typed::{self, *};
pub use components::untyped;
pub use typecheck::ResolveTypes;

/// Produces an ast::typed::Rule from the given source code.
pub fn make_ndca_rule(source_code: &str) -> LangResult<typed::Rule> {
    let tokens = tokens::tokenize(source_code)?;
    let untyped_ast = builder::build_ast(&tokens)?;
    let untyped_rule = untyped::Rule::try_from(untyped_ast)?;
    let typed_rule = untyped_rule.try_into()?;
    Ok(typed_rule)
}

/// "Flattens" a block of instructions by replacing the body of all branching
/// instructions (If, ForLoop, WhileLoop, etc.) with a single Goto and moving
/// their contents to the end of the instruction list. This is mainly useful for
/// the interpreter, which cannot handle nested structure.
pub fn flatten_block(block: &mut typed::StatementBlock) {
    if block.is_empty() {
        return;
    }
    let end = block.last().unwrap().span.end;
    // Push an End statement at the end of the function as a signal to the
    // interpreter.
    block.push(Spanned::new(end, end, typed::Statement::End));
    // This is basically a for-each loop that allows adding new elements
    // to the end during iteration.
    for i in 0.. {
        use typed::Statement::{Goto, If};
        let idx_at_end = block.len();
        if let Some(statement) = block.get_mut(i) {
            let span = statement.span;
            // This is a function that returns a Goto instruction that jumps to
            // the given index, using the Span of the current instruction.
            let goto_idx = |idx| Spanned {
                span,
                inner: Goto(idx),
            };
            // Convert any nested blocks into Goto statements.
            match &mut statement.inner {
                If {
                    ref mut if_true,
                    ref mut if_false,
                    ..
                } => {
                    // Replace all the statements in if_true with a single Goto
                    // that jumps to the current end of the block, where we will
                    // put the if_true statements.
                    let mut new_statements =
                        std::mem::replace(if_true, vec![goto_idx(idx_at_end - 1)]);
                    new_statements.push(goto_idx(i));
                    // Replace all the statements in if_false with a single Goto
                    // that jumps to right after where the last statement from
                    // if_true will end up, which is where we will put the
                    // if_false statements. This requires a mutable reference to
                    // the "if" statement that we're modifying, which is why we
                    // can't add new_statements to the end of the block yet.
                    if !if_false.is_empty() {
                        new_statements.extend(std::mem::replace(
                            if_false,
                            vec![goto_idx(idx_at_end - 1 + new_statements.len())],
                        ));
                        new_statements.push(goto_idx(i));
                    }
                    // Actually add those if_true and if_false statements to the
                    // end of the block.
                    block.extend(new_statements);
                }
                _ => (),
            }
        } else {
            // End of block
            return;
        }
    }
}

#[cfg(test)]
mod tests;
