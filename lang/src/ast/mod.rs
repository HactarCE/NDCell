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
    codemap: CodeMap,
    files: Vec<Arc<File>>,

    nodes: Vec<AnyNode>,
    directives: Vec<DirectiveId>,
}
impl Program {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_file(&mut self, name: String, source: String) -> Arc<File> {
        self.files.push(self.codemap.add_file(name, source));
        Arc::clone(self.files.last().unwrap())
    }

    pub fn directives(&self) -> &[DirectiveId] {
        &self.directives
    }

    pub fn codemap(&self) -> &CodeMap {
        &self.codemap
    }

    pub(crate) fn add_node<D>(&mut self, span: Span, data: D) -> NodeId<Node<D>>
    where
        Node<D>: NodeTrait,
    {
        let id = self.nodes.len();
        self.nodes.push(Node { id, span, data }.into_any_node());
        NodeId(id, PhantomData)
    }

    pub(crate) fn add_directive(&mut self, id: DirectiveId) {
        self.directives.push(id);
    }

    pub fn get_node<'ast, N: NodeTrait>(&'ast self, id: NodeId<N>) -> AstNode<'ast, N> {
        AstNode { ast: self, id }
    }
    pub fn get_node_list<'ast, N: NodeTrait>(
        &'ast self,
        ids: &[NodeId<N>],
    ) -> Vec<AstNode<'ast, N>> {
        ids.iter().map(|&id| self.get_node(id)).collect()
    }
}
