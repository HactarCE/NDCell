use super::*;

pub type Directive<'ast> = AstNode<'ast, DirectiveNode>;
pub type DirectiveId = NodeId<DirectiveNode>;
pub type DirectiveNode = Node<DirectiveData>;

#[derive(Debug, Clone)]
pub enum DirectiveData {
    Init {
        mode: LangMode,
        body: StmtId,
    },

    Function {
        mode: LangMode,
        name: Spanned<Arc<String>>,
        params: Vec<(Spanned<Arc<String>>, ExprId)>,
        ret_type: ExprId,
        body: StmtId,
    },
    Transition(StmtId),
    Compile {
        param_types: Spanned<Vec<ExprId>>,
        body: StmtId,
    },

    // Rule(Spanned<Arc<String>>),
    Ndim(ExprId),
    Radius(ExprId),
    States(ExprId),
}
