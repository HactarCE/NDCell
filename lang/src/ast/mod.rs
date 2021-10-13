use codemap::{CodeMap, File, Span};
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};
use std::sync::Arc;

mod cellstate;
mod nodes;
mod ops;

pub use cellstate::CellState;
pub use nodes::*;
pub use ops::*;

#[derive(Debug, Default)]
pub struct Program {
    pub codemap: CodeMap,
    pub files: Vec<Arc<File>>,

    pub nodes: Vec<AnyNode>,
    pub directives: Vec<DirectiveId>,
}
impl Program {
    /// Constructs a new empty program.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a file to the code map but does not parse it.
    pub fn add_file(&mut self, name: String, source: String) -> Arc<File> {
        self.files.push(self.codemap.add_file(name, source));
        Arc::clone(self.files.last().unwrap())
    }

    /// Returns the code map.
    pub fn codemap(&self) -> &CodeMap {
        &self.codemap
    }
    /// Returns a list of files in the code map.
    pub fn files(&self) -> &[Arc<File>] {
        &self.files
    }
    /// Returns a list of directives in the AST.
    pub fn directives<'ast>(&'ast self) -> impl 'ast + Iterator<Item = Directive<'ast>> {
        self.directives.iter().map(move |&id| self.get_node(id))
    }
    /// Returns whether `f` returns `true` for any directive.
    pub(crate) fn has_directive(&self, f: fn(&DirectiveData) -> bool) -> bool {
        self.directives().map(|node| node.data()).any(f)
    }

    /// Adds a node to the AST.
    pub(crate) fn add_node<D>(&mut self, span: Span, data: D) -> NodeId<Node<D>>
    where
        Node<D>: NodeTrait,
    {
        let id = self.nodes.len();
        self.nodes.push(Node { id, span, data }.into_any_node());
        NodeId(id, PhantomData)
    }
    /// Adds a directive to the directives list.
    pub(crate) fn add_directive(&mut self, id: DirectiveId) {
        self.directives.push(id);
    }

    /// Returns a node in the AST.
    pub fn get_node<'ast, N: NodeTrait>(&'ast self, id: NodeId<N>) -> AstNode<'ast, N> {
        AstNode { ast: self, id }
    }
    /// Returns a list of nodes in the AST.
    pub fn get_node_list<'ast, N: NodeTrait>(
        &'ast self,
        ids: &[NodeId<N>],
    ) -> Vec<AstNode<'ast, N>> {
        ids.iter().map(|&id| self.get_node(id)).collect()
    }
}
