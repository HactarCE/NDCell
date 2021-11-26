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
        /// Name of the index iteration variable, if present.
        index_var: Option<Spanned<Arc<String>>>,
        /// Name of the main iteration variable.
        iter_var: Spanned<Arc<String>>,
        /// Expression to iterate over.
        iter_expr: ExprId,
        /// Span of just the first line (from `for` to before `{`).
        first_line_span: Span,
        /// Block of statements to execute in the loop.
        block: StmtId,
    },
    WhileLoop {
        condition: ExprId,
        /// Span of jsut the first line (from `while` to before `{`).
        first_line_span: Span,
        block: StmtId,
    },

    // Returns
    Become(ExprId),
    Remain,
    Return(Option<ExprId>),
}
impl Stmt<'_> {
    /// Finds all variables assigned in this statement and its children.
    pub fn find_all_assigned_vars(self, names: &mut HashSet<Arc<String>>) {
        match self.data() {
            StmtData::Block(ids) => {
                for id in ids {
                    self.ast.get_node(*id).find_all_assigned_vars(names);
                }
            }

            StmtData::Assign { lhs, .. } => {
                if let Some(var) = self.ast.get_node(*lhs).find_assigned_var() {
                    names.insert(var.node);
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
                index_var,
                iter_var,
                block,
                ..
            } => {
                if let Some(index_var) = index_var {
                    names.insert(Arc::clone(&index_var.node));
                }
                names.insert(Arc::clone(&iter_var.node));
                self.ast.get_node(*block).find_all_assigned_vars(names);
            }
            StmtData::WhileLoop { block, .. } => {
                self.ast.get_node(*block).find_all_assigned_vars(names);
            }

            StmtData::Become(_) => (),
            StmtData::Remain => (),
            StmtData::Return(_) => (),
        }
    }
}
