use super::*;

pub type Stmt<'ast> = AstNode<'ast, StmtNode>;
pub type StmtId = NodeId<StmtNode>;
pub type StmtNode = Node<StmtData>;

#[derive(Debug)]
pub enum StmtData {
    Block(Vec<StmtId>),

    // Assignment
    Assign(ExprId, Spanned<AssignOp>, ExprId), // TODO: only certain exprs are allowed. NO PARENS OR BRACKETS AT TOP LEVEL

    // Branching
    IfElse(ExprId, Option<StmtId>, Option<StmtId>),
    Unless(ExprId, StmtId),

    // Debugging
    Assert(ExprId, Option<Spanned<String>>),
    Error(Option<Spanned<String>>),

    // Loops
    Break,
    Continue,
    ForLoop(VarId, ExprId, StmtId),

    // Returns
    Become(ExprId),
    Remain,
    Return(ExprId),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AssignOp {
    NoOp,

    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    Shl,
    ShrSigned,
    ShrUnsigned,

    And,
    Or,
    Xor,
}
impl TryFrom<Token> for AssignOp {
    type Error = ();

    fn try_from(token: Token) -> std::result::Result<Self, Self::Error> {
        match token {
            Token::Assign => Ok(Self::NoOp),

            Token::AssignPlus => Ok(Self::Add),
            Token::AssignMinus => Ok(Self::Sub),
            Token::AssignAsterisk => Ok(Self::Mul),
            Token::AssignSlash => Ok(Self::Div),
            Token::AssignPercent => Ok(Self::Mod),
            Token::AssignDoubleAsterisk => Ok(Self::Pow),
            Token::AssignDoubleLessThan => Ok(Self::Shl),
            Token::AssignDoubleGreaterThan => Ok(Self::ShrSigned),
            Token::AssignTripleGreaterThan => Ok(Self::ShrUnsigned),
            Token::AssignAmpersand => Ok(Self::And),
            Token::AssignPipe => Ok(Self::Or),
            Token::AssignCaret => Ok(Self::Xor),

            _ => Err(()),
        }
    }
}
