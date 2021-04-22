use super::*;

pub type Expr<'ast> = AstNode<'ast, ExprNode>;
pub type ExprId = NodeId<ExprNode>;
pub type ExprNode = Node<ExprData>;

#[derive(Debug)]
pub enum ExprData {
    /// Parenthetical group.
    Paren(ExprId),

    /// Identifier.
    Identifier(Arc<String>),

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
        /// Arguments to the function call.
        args: Vec<ExprId>,
    },
    /// Attribute access or method call.
    MethodCall {
        /// Name of attribute or method.
        attr: Spanned<Arc<String>>,
        /// Arguments to the method call (first must be method receiver).
        ///
        /// The span covers the pair of parentheses containing all but the first
        /// argument.
        args: Spanned<Vec<ExprId>>,
    },
    /// Index operation.
    IndexOp {
        /// Index arguments (first must be object being indexed).
        ///
        /// The span covers the pair of brackets containing all but the first
        /// argument.
        args: Spanned<Vec<ExprId>>,
    },

    /// Vector constructor.
    VectorConstruct(Vec<ExprId>),
}
impl ExprData {
    pub fn ret_type(&mut self) -> Type {
        todo!()
    }
}
