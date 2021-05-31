use super::*;

pub type Directive<'ast> = AstNode<'ast, DirectiveNode>;
pub type DirectiveId = NodeId<DirectiveNode>;
pub type DirectiveNode = Node<DirectiveData>;

#[derive(Debug, Clone)]
pub enum DirectiveData {
    Compile {
        param_types: Spanned<Vec<ExprId>>,
        body: StmtId,
    },

    Init(StmtId),
    // Rule(Spanned<Arc<String>>),
    Ndim(ExprId),
    States(ExprId),
    // Function(FuncId),
    // Transition(StmtId),
}
