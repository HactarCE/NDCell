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
impl Stmt<'_> {
    /// Finds all variables assigned in this statement and its children.
    pub fn find_all_assigned_vars(self, names: &mut HashMap<Arc<String>, Span>) {
        match self.data() {
            StmtData::Block(ids) => {
                for id in ids {
                    self.ast.get_node(*id).find_all_assigned_vars(names);
                }
            }

            StmtData::Assign { lhs, .. } => {
                if let Some(var) = self.ast.get_node(*lhs).find_assigned_var() {
                    names.entry(var.node).or_insert(var.span);
                }
            }

            StmtData::IfElse {
                if_true, if_false, ..
            } => {
                if let Some(id) = *if_true {
                    self.ast.get_node(id).find_all_assigned_vars(names);
                }
                if let Some(id) = *if_false {
                    self.ast.get_node(id).find_all_assigned_vars(names);
                }
            }

            StmtData::Assert { .. } => (),
            StmtData::Error { .. } => (),

            StmtData::Break => (),
            StmtData::Continue => (),
            StmtData::ForLoop {
                iter_var, block, ..
            } => {
                names
                    .entry(Arc::clone(&iter_var.node))
                    .or_insert(iter_var.span);
                self.ast.get_node(*block).find_all_assigned_vars(names);
            }

            StmtData::Become(_) => (),
            StmtData::Remain => (),
            StmtData::Return(_) => (),
        }
    }
}
