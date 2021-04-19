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

    /// Attribute access or method call.
    MethodCall {
        obj: ExprId,
        attr: Spanned<Arc<String>>, // TODO: struct/enum for built-in method?
        args: Vec<ExprId>,
    },
    /// Function call.
    FuncCall {
        func: Spanned<Arc<String>>,
        args: Vec<ExprId>,
    },
    /// Index operation.
    IndexOp { obj: ExprId, args: Vec<ExprId> },

    /// Vector constructor.
    VectorConstruct(Vec<ExprId>),
}
impl ExprData {
    pub fn ret_type(&mut self) -> Type {
        todo!()
    }
}
