mod builder;
mod components;
mod tokens;
mod typecheck;

use super::span::Spanned;
pub use builder::{make_program, AstBuilder};
pub use components::common::{self, *};
pub use components::typed::{self, *};
pub use components::untyped;
pub use typecheck::ResolveTypes;

/// "Flatten" a block of instructions by replacing the body of all branching
/// instructions (If, ForLoop, WhileLoop, etc.) with a single Goto and moving
/// their contents to the end of the instruction list. This is mainly useful for
/// the interpreter, which cannot handle nested structure.
pub fn flatten_block(block: &mut typed::StatementBlock) {
    if block.is_empty() {
        return;
    }
    let end = block.last().unwrap().span.end;
    block.push(Spanned::new(end, end, typed::Statement::End));
    // This is basically a for-each loop that allows adding new elements
    // to the end during iteration.
    for i in 0.. {
        use typed::Statement::{Goto, If};
        let idx_at_end = block.len();
        if let Some(statement) = block.get_mut(i) {
            let span = statement.span;
            let goto_idx = |idx| Spanned {
                span,
                inner: Goto(idx),
            };
            match &mut statement.inner {
                If {
                    ref mut if_true,
                    ref mut if_false,
                    ..
                } => {
                    let mut new_statements =
                        std::mem::replace(if_true, vec![goto_idx(idx_at_end - 1)]);
                    new_statements.push(goto_idx(i));
                    if !if_false.is_empty() {
                        new_statements.extend(std::mem::replace(
                            if_false,
                            vec![goto_idx(idx_at_end - 1 + new_statements.len())],
                        ));
                        new_statements.push(goto_idx(i));
                    }
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
