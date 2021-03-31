use super::*;

pub type Directive<'ast> = AstNode<'ast, DirectiveNode>;
pub type DirectiveId = NodeId<DirectiveNode>;
pub type DirectiveNode = Node<DirectiveData>;

#[derive(Debug, Clone)]
pub enum DirectiveData {
    Init(StmtId),
    // Rule(Spanned<Arc<String>>),
    // Ndim(ExprId),
    // States(ExprId),
    // Function(FuncId),
    // Transition(StmtId),
}
impl DirectiveData {
    pub fn kind(&self) -> DirectiveKind {
        match self {
            Self::Init(_) => DirectiveKind::Init,
            // Self::Rule(_) => DirectiveKind::Rule,
            // Self::Ndim(_) => DirectiveKind::Ndim,
            // Self::States(_) => DirectiveKind::States,
            // Self::Function(_) => DirectiveKind::Function,
            // Self::Transition(_) => DirectiveKind::Transition,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DirectiveKind {
    Init,
    // Rule,
    // Ndim,
    // States,
    // Function,
    // Transition,
}
