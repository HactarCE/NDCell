use super::*;

pub type Func<'ast> = AstNode<'ast, FuncNode>;
pub type FuncId = NodeId<FuncNode>;
pub type FuncNode = Node<FuncData>;

#[derive(Debug)]
pub struct FuncData {
    pub name: Spanned<String>,
    pub params: Vec<VarId>,
    pub ret_type: Fallible<Type>,
    pub body: StmtId,
}
