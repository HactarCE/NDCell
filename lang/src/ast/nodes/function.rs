use super::*;

pub type Func<'ast> = AstNode<'ast, FuncNode>;
pub type FuncId = NodeId<FuncNode>;
pub type FuncNode = Node<FuncData>;

#[derive(Debug)]
pub struct FuncData {
    // pub name: Spanned<String>,
// pub param_names: Vec<Spanned<String>>,
// pub ret_type: Result<Type>,
// pub body: StmtId,
}
