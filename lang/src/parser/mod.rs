//! Functions for producing a parse tree.
use std::collections::HashMap;
use std::rc::Rc;

#[macro_use]
mod macros;
mod eaters;
mod feeder;
mod tree;

pub use tree::*;

use crate::errors::*;
use crate::lexer::*;
use crate::Spanned;
use feeder::TokenFeeder;

/// Parses the given tokens and returns a ParseTree.
pub fn parse(source_code: Rc<String>, tokens: &[Token]) -> LangResult<ParseTree> {
    let mut directives: HashMap<Directive, Vec<Spanned<DirectiveContents>>> = HashMap::new();
    // When parsing, span information is attached to the whole directive (name
    // and contents). But because we only store the name of each directive once
    // in the HashMap, we have to put the span information only on the directive
    // contents.
    for Spanned {
        span,
        inner: (directive_name, directive_contents),
    } in TokenFeeder::from(tokens)
        .feed(eaters::Directives)?
        .into_iter()
    {
        directives.entry(directive_name).or_default().push(Spanned {
            span,
            inner: directive_contents,
        });
    }
    Ok(ParseTree {
        source_code,
        directives,
    })
}
