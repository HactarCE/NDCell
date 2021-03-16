pub enum ParseNode {
    ParenExpr,
    SymbolExpr,
    IntExpr,
    StringExpr,
    BinaryOpExpr,
    PrefixOpExpr,
    MethodCallExpr,
    FuncCallExpr,
    IndexOpExpr,
    VectorConstructExpr,
}
