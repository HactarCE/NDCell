use rowan::{GreenNode, GreenNodeBuilder};

use super::{SyntaxKind, Token};
use SyntaxKind::*;

/// Parse result.
pub struct Parse {
    /// Root "green node" of the syntax tree.
    green_node: GreenNode,
    /// List of syntax errors.
    errors: Vec<String>,
}

struct Parser {
    /// Input tokens, including whitespace, in *reverse* order.
    tokens: Vec<(SyntaxKind, String)>,
    /// In-progress syntax tree.
    builder: GreenNodeBuilder<'static>,
    /// List of syntax errors found so far.
    errors: Vec<String>,
}

/// Produces a parse tree from source code.
///
/// Note that this function does not return a `Result`; syntax errors are
/// embedded in the `Parse` return value.
fn parse(source: &str) -> Parse {
    T!['('];
    todo!()
}
