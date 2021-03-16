use super::*;

pub type Var<'ast> = AstNode<'ast, VarNode>;
pub type VarId = NodeId<VarNode>;
pub type VarNode = Node<VarData>;

#[derive(Debug)]
pub struct VarData {
    pub name: String,
    pub ty: Fallible<Type>,
}
