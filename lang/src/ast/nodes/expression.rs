use super::*;

pub type Expr<'ast> = AstNode<'ast, ExprNode>;
pub type ExprId = NodeId<ExprNode>;
pub type ExprNode = Node<ExprData>;

/// Wrapper around an expression providing an optional keyword.
pub type FuncArg<T> = (Option<Spanned<Arc<String>>>, T);

#[derive(Debug)]
pub enum ExprData {
    /// Parenthetical group.
    Paren(ExprId),

    /// Identifier.
    Identifier(Arc<String>),
    /// Tag name (without the leading `#`).
    TagName(Arc<String>),

    /// Constant value.
    Constant(RtVal),

    /// Binary operator.
    BinaryOp(ExprId, Spanned<BinaryOp>, ExprId),
    /// Prefix operator.
    PrefixOp(Spanned<PrefixOp>, ExprId),
    /// Comparison chain.
    CmpChain(Vec<ExprId>, Vec<Spanned<CompareOp>>),

    /// Function call.
    FuncCall {
        /// Name of function.
        func: Spanned<Arc<String>>,
        /// Arguments to the function call, each with an optional keyword.
        args: Vec<FuncArg<ExprId>>,
    },
    /// Attribute access or method call.
    MethodCall {
        /// Method receiver.
        obj: ExprId,
        /// Name of attribute or method.
        attr: Spanned<Arc<String>>,
        /// Arguments to the method call, each with an optional keyword.
        ///
        /// The span covers the pair of parentheses containing all arguments.
        args: Spanned<Vec<FuncArg<ExprId>>>,
    },
    /// Index operation.
    IndexOp {
        /// Object being indexed.
        obj: ExprId,
        /// Index arguments.
        ///
        /// The span covers the pair of brackets containing the arguments.
        args: Spanned<Vec<ExprId>>,
    },

    /// Vector constructor.
    VectorConstruct(Vec<ExprId>),
    /// Set constructor.
    SetConstruct(Vec<ExprId>),
}
impl Expr<'_> {
    /// Returns the variable assigned in the expression when used on the left
    /// side of an assignment statement.
    pub fn find_assigned_var(self) -> Option<Spanned<Arc<String>>> {
        match self.data() {
            ExprData::Paren(id) => self.ast.get_node(*id).find_assigned_var(),

            ExprData::Identifier(name) => Some(Spanned {
                node: Arc::clone(name),
                span: self.span(),
            }),
            ExprData::TagName(_) => None,

            ExprData::Constant(_) => None,

            ExprData::BinaryOp(_, _, _) => None,
            ExprData::PrefixOp(_, _) => None,
            ExprData::CmpChain(_, _) => None,

            ExprData::FuncCall { func, .. } => Some(func.clone()),
            ExprData::MethodCall { obj, .. } => self.ast.get_node(*obj).find_assigned_var(),
            ExprData::IndexOp { obj, .. } => self.ast.get_node(*obj).find_assigned_var(),

            ExprData::VectorConstruct(_) => None,
            ExprData::SetConstruct(_) => None,
        }
    }
}
