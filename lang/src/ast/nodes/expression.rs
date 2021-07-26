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
        /// Name of attribute or method.
        attr: Spanned<Arc<String>>,
        /// Arguments to the method call, each with an optional keyword (first
        /// must be method receiver).
        ///
        /// The span covers the pair of parentheses containing all but the first
        /// argument.
        args: Spanned<Vec<FuncArg<ExprId>>>,
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
    /// Set constructor.
    SetConstruct(Vec<ExprId>),
}
