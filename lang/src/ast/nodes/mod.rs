use codemap::Spanned;
use std::collections::HashSet;
use std::fmt;
use std::hash::{Hash, Hasher};

mod directive;
mod expression;
mod statement;

use super::*;
use crate::data::RtVal;
use crate::LangMode;
pub use directive::*;
pub use expression::*;
pub use statement::*;

#[derive(Debug)]
pub struct Node<D> {
    pub id: usize,
    pub span: Span,
    pub mode: LangMode,
    pub data: D,
}

#[derive(Debug)]
pub enum AnyNode {
    Directive(DirectiveNode),
    Expr(ExprNode),
    Stmt(StmtNode),
}

pub struct NodeId<N>(pub(super) usize, pub(super) PhantomData<N>);
impl<N> fmt::Debug for NodeId<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}#{}", std::any::type_name::<N>(), self.0)
    }
}
impl<N> Copy for NodeId<N> {}
impl<N> Clone for NodeId<N> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}
impl<N> PartialEq for NodeId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<N> Eq for NodeId<N> {}
impl<N> Hash for NodeId<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

pub trait NodeTrait: Sized + fmt::Debug {
    fn id(&self) -> NodeId<Self>;
    fn span(&self) -> Span;
    fn mode(&self) -> LangMode;

    fn from_any_node(any: AnyNode) -> Option<Self>;
    fn from_any_node_ref(any: &AnyNode) -> Option<&Self>;
    fn from_any_node_mut(any: &mut AnyNode) -> Option<&mut Self>;
    fn into_any_node(self) -> AnyNode;
}
macro_rules! impl_node_trait {
    (for AnyNode::$variant:ident(Node<$data:ty>)) => {
        impl NodeTrait for Node<$data> {
            fn id(&self) -> NodeId<Self> {
                NodeId(self.id, PhantomData)
            }
            fn span(&self) -> Span {
                self.span
            }
            fn mode(&self) -> LangMode {
                self.mode
            }

            fn from_any_node(any: AnyNode) -> Option<Self> {
                match any {
                    AnyNode::$variant(node) => Some(node),
                    _ => None,
                }
            }
            fn from_any_node_ref(any: &AnyNode) -> Option<&Self> {
                match any {
                    AnyNode::$variant(node) => Some(node),
                    _ => None,
                }
            }
            fn from_any_node_mut(any: &mut AnyNode) -> Option<&mut Self> {
                match any {
                    AnyNode::$variant(node) => Some(node),
                    _ => None,
                }
            }
            fn into_any_node(self) -> AnyNode {
                AnyNode::$variant(self)
            }
        }
    };
}
impl_node_trait!(for AnyNode::Directive(Node<DirectiveData>));
impl_node_trait!(for AnyNode::Expr(Node<ExprData>));
impl_node_trait!(for AnyNode::Stmt(Node<StmtData>));
macro_rules! match_any_node {
    (match $match_expr:expr; $node:ident => $result:expr) => {
        match_any_node!(match $match_expr; $node => $result;
            for Directive, Expr, Stmt
        )
    };
    (match $match_expr:expr; $node:ident => $result:expr; for $($variant:ident),+) => {
        match $match_expr { $( AnyNode::$variant($node) => $result ),+ }
    };
}
impl NodeTrait for AnyNode {
    fn id(&self) -> NodeId<Self> {
        let id = match_any_node!(match self; node => node.id);
        NodeId(id, PhantomData)
    }
    fn span(&self) -> Span {
        match_any_node!(match self; node => node.span)
    }
    fn mode(&self) -> LangMode {
        match_any_node!(match self; node => node.mode)
    }

    fn from_any_node(any: AnyNode) -> Option<Self> {
        Some(any)
    }
    fn from_any_node_ref(any: &AnyNode) -> Option<&Self> {
        Some(any)
    }
    fn from_any_node_mut(any: &mut AnyNode) -> Option<&mut Self> {
        Some(any)
    }
    fn into_any_node(self) -> AnyNode {
        self
    }
}

impl<N: NodeTrait> Index<NodeId<N>> for Program {
    type Output = N;

    fn index(&self, index: NodeId<N>) -> &Self::Output {
        N::from_any_node_ref(&self.nodes[index.0]).expect("Wrong node type")
    }
}
impl<N: NodeTrait> IndexMut<NodeId<N>> for Program {
    fn index_mut(&mut self, index: NodeId<N>) -> &mut Self::Output {
        N::from_any_node_mut(&mut self.nodes[index.0]).expect("Wrong node type")
    }
}

pub struct AstNode<'ast, N> {
    pub ast: &'ast Program,
    pub id: NodeId<N>,
}
impl<N> fmt::Debug for AstNode<'_, N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.id)
    }
}
impl<N> Copy for AstNode<'_, N> {}
impl<N> Clone for AstNode<'_, N> {
    fn clone(&self) -> Self {
        Self {
            ast: self.ast,
            id: self.id,
        }
    }
}
impl<'ast, N: NodeTrait> AstNode<'ast, N> {
    pub fn node(self) -> &'ast N {
        &self.ast[self.id]
    }
    pub fn span(self) -> Span {
        self.node().span()
    }
    pub fn mode(self) -> LangMode {
        self.node().mode()
    }
}
impl<'ast, D> AstNode<'ast, Node<D>>
where
    Node<D>: NodeTrait,
{
    pub fn data(self) -> &'ast D {
        &self.node().data
    }
}