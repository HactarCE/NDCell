use super::*;

pub type Stmt<'ast> = AstNode<'ast, StmtNode>;
pub type StmtId = NodeId<StmtNode>;
pub type StmtNode = Node<StmtData>;

#[derive(Debug, Clone)]
pub enum StmtData {
    Block(Vec<StmtId>),

    // Assignment
    Assign {
        lhs: ExprId, // TODO: only certain exprs are allowed. NO PARENS OR BRACKETS AT TOP LEVEL
        op: Spanned<Option<AssignOp>>,
        rhs: ExprId,
    },

    // Branching
    IfElse {
        condition: ExprId,
        if_true: Option<StmtId>,
        if_false: Option<StmtId>,
    },

    // Debugging
    Assert {
        condition: ExprId,
        msg: Option<Spanned<Arc<String>>>,
    },
    Error {
        msg: Option<Spanned<Arc<String>>>,
    },

    // Loops
    Break,
    Continue,
    ForLoop {
        iter_var: Spanned<Arc<String>>,
        iter_expr: ExprId,
        block: StmtId,
    },

    // Returns
    Become(ExprId),
    Remain,
    Return(Option<ExprId>),
}
