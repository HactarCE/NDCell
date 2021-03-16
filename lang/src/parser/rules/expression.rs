use codemap::Spanned;
use std::rc::Rc;

use super::{
    Ctx, Epsilon, Identifier, IntegerLiteral, List, Parser, StringLiteral, Surround, SyntaxRule,
    TryFromToken, VectorLiteral,
};
use crate::ast::{self, ExprId};
use crate::data::Value;
use crate::errors::Result;
use crate::lexer::Token;

/// Operator precedence table.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpPrecedence {
    LogicalOr,
    LogicalXor,
    LogicalAnd,
    LogicalNot,
    Comparison,
    Is,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Bitshift,
    AddSub,
    MulDiv,
    Pow,
    Range,
    Prefix,
    Suffix,
    Atom,
}
impl Default for OpPrecedence {
    fn default() -> Self {
        Self::lowest()
    }
}
impl OpPrecedence {
    /// Returns the lowest precedence level.
    pub const fn lowest() -> Self {
        Self::LogicalOr
    }
    /// Returns the highest precedence level.
    pub const fn highest() -> Self {
        Self::Atom
    }
    /// Returns the next-highest precedence level. Panics if given
    /// OpPrecedence::Atom.
    pub fn next(self) -> Self {
        match self {
            Self::LogicalOr => Self::LogicalXor,
            Self::LogicalXor => Self::LogicalAnd,
            Self::LogicalAnd => Self::LogicalNot,
            Self::LogicalNot => Self::Comparison,
            Self::Comparison => Self::Is,
            Self::Is => Self::BitwiseOr,
            Self::BitwiseOr => Self::BitwiseXor,
            Self::BitwiseXor => Self::BitwiseAnd,
            Self::BitwiseAnd => Self::Bitshift,
            Self::Bitshift => Self::AddSub,
            Self::AddSub => Self::MulDiv,
            Self::MulDiv => Self::Pow,
            Self::Pow => Self::Range,
            Self::Range => Self::Prefix,
            Self::Prefix => Self::Suffix,
            Self::Suffix => Self::Atom,
            Self::Atom => panic!("Tried to get operator precedence level beyond {:?}", self),
        }
    }

    /// Returns a list of binary operators at this precedence level.
    pub fn binary_ops(self) -> &'static [ast::BinaryOp] {
        use ast::BinaryOp::*;
        match self {
            Self::LogicalOr => &[LogicalOr],
            Self::LogicalXor => &[LogicalXor],
            Self::LogicalAnd => &[LogicalAnd],
            Self::LogicalNot => &[],
            Self::Comparison => &[],
            Self::Is => &[Is],
            Self::BitwiseOr => &[BitwiseOr],
            Self::BitwiseXor => &[BitwiseXor],
            Self::BitwiseAnd => &[BitwiseAnd],
            Self::Bitshift => &[Shl, ShrSigned, ShrUnsigned],
            Self::AddSub => &[Add, Sub],
            Self::MulDiv => &[Mul, Div, Mod],
            Self::Pow => &[Pow],
            Self::Range => &[Range],
            Self::Prefix => &[],
            Self::Suffix => &[],
            Self::Atom => &[],
        }
    }

    /// Returns a list of unary operators at this precedence level.
    pub fn prefix_ops(self) -> &'static [ast::PrefixOp] {
        use ast::PrefixOp::*;
        match self {
            Self::LogicalOr => &[],
            Self::LogicalXor => &[],
            Self::LogicalAnd => &[],
            Self::LogicalNot => &[LogicalNot],
            Self::Comparison => &[],
            Self::Is => &[],
            Self::BitwiseOr => &[],
            Self::BitwiseXor => &[],
            Self::BitwiseAnd => &[],
            Self::Bitshift => &[],
            Self::AddSub => &[],
            Self::MulDiv => &[],
            Self::Pow => &[],
            Self::Range => &[],
            Self::Prefix => &[Pos, Neg, BitwiseNot, IntToCell],
            Self::Suffix => &[],
            Self::Atom => &[],
        }
    }

    /// Returns whether the binary operators at this precedence level are
    /// right-associative.
    pub fn is_right_associative(self) -> bool {
        match self {
            OpPrecedence::Pow => true,
            _ => false,
        }
    }
}

/// Matches an expression.
#[derive(Debug, Copy, Clone)]
pub struct Expression;
impl_display!(for Expression, "expression");
impl SyntaxRule for Expression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        ExpressionWithPrecedence::default().might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse(ctx, ExpressionWithPrecedence::default())
    }
}

/// Matches an expression with the given precedence level.
#[derive(Debug, Default, Copy, Clone)]
struct ExpressionWithPrecedence(pub OpPrecedence);
impl_display!(for ExpressionWithPrecedence, "expression");
impl SyntaxRule for ExpressionWithPrecedence {
    type Output = ast::ExprId;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        match p.next().unwrap() {
            Token::LParen | Token::LBracket => true,

            Token::LBrace => false,
            Token::RParen | Token::RBracket | Token::RBrace => false,

            Token::Backtick => true,

            Token::Colon | Token::Comma | Token::Period | Token::Semicolon => false,
            Token::Eql | Token::Neq | Token::Lt | Token::Gt | Token::Lte | Token::Gte => false,

            Token::Plus | Token::Minus => true,

            Token::Asterisk
            | Token::Slash
            | Token::Percent
            | Token::DoubleAsterisk
            | Token::DoubleLessThan
            | Token::DoubleGreaterThan
            | Token::TripleGreaterThan
            | Token::Ampersand
            | Token::Pipe
            | Token::Caret
            | Token::Tilde
            | Token::Arrow
            | Token::DotDot => false,

            Token::Octothorpe => true,

            Token::Assign
            | Token::AssignPlus
            | Token::AssignMinus
            | Token::AssignAsterisk
            | Token::AssignSlash
            | Token::AssignPercent
            | Token::AssignDoubleAsterisk
            | Token::AssignDoubleLessThan
            | Token::AssignDoubleGreaterThan
            | Token::AssignTripleGreaterThan
            | Token::AssignAmpersand
            | Token::AssignPipe
            | Token::AssignCaret => false,

            Token::KeywordOr | Token::KeywordXor | Token::KeywordAnd => false,

            Token::KeywordNot => true,

            Token::KeywordIs | Token::KeywordIn => false,

            Token::KeywordInteger
            | Token::KeywordVector
            | Token::KeywordCell
            | Token::KeywordArray
            | Token::KeywordIntegerSet
            | Token::KeywordVectorSet
            | Token::KeywordCellSet
            | Token::KeywordPattern
            | Token::KeywordString
            | Token::KeywordType
            | Token::KeywordTag => true,

            Token::KeywordBreak
            | Token::KeywordContinue
            | Token::KeywordFor
            | Token::KeywordBecome
            | Token::KeywordRemain
            | Token::KeywordReturn
            | Token::KeywordIf
            | Token::KeywordElse
            | Token::KeywordUnless
            | Token::KeywordCase
            | Token::KeywordMatch
            | Token::KeywordAssert
            | Token::KeywordError => false,

            Token::Comment | Token::UnterminatedBlockComment => false,

            Token::StringLiteral | Token::UnterminatedStringLiteral => true,
            Token::IntegerLiteral => true,
            Token::Ident => true,

            Token::Whitespace => false,
            Token::Unknown => false,
        }
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        // Consume an expression at the given precedence level, which may
        // consist of expressions with higher precedence.
        match self.0 {
            OpPrecedence::Atom => parse_one_of!(
                p,
                ctx,
                [
                    IdentifierExpression.map(Some),
                    StringLiteralExpression.map(Some),
                    IntegerLiteralExpression.map(Some),
                    VectorLiteralExpression.map(Some),
                    ParenExpression.map(Some),
                    Epsilon.map(|_| None),
                ],
            )
            .transpose()
            .unwrap_or_else(|| p.expected(self)),

            OpPrecedence::Suffix => parse_suffix_expr(p, ctx),
            prec if !prec.binary_ops().is_empty() => parse_binary_ops_expr(p, ctx, prec),
            prec if !prec.prefix_ops().is_empty() => parse_prefix_ops(p, ctx, prec),
            prec => internal_error!("don't know what to do for precedence {:?}", prec),
        }
    }
}

/// Parses an expression with any number of binary operators at a specific
/// precedence level.
fn parse_binary_ops_expr(
    p: &mut Parser<'_>,
    ctx: &mut Ctx<'_>,
    precedence: OpPrecedence,
) -> Result<ast::ExprId> {
    let recursive_expression = ExpressionWithPrecedence(precedence.next());

    let allowed_ops = precedence.binary_ops();

    // First, just make a list of operators and expressions. `ops.len()`
    // should always be equal to `exprs.len() - 1`.
    let mut ops: Vec<Spanned<ast::BinaryOp>> = vec![];
    let mut exprs: Vec<ast::ExprId> = vec![p.parse(ctx, recursive_expression)?];
    let op_syntax_rule =
        TryFromToken::<ast::BinaryOp>::with_display("binary operator, such as '+' or '*'");
    while let Some(op) = p
        .parse(ctx, op_syntax_rule)
        .ok()
        .filter(|op| allowed_ops.contains(op))
    {
        ops.push(op);
        exprs.push(p.parse(ctx, recursive_expression)?);
    }

    let mut ret: ast::ExprId;
    if precedence.is_right_associative() {
        // Take out the last/rightmost expression; that's the deepest
        // AST node.
        ret = exprs.pop().unwrap();
        // Pair up the remaining operators and expressions, and make a new
        // AST node using the previous iteration as the right-hand side of
        // the expression.
        for (lhs, op) in exprs.into_iter().zip(ops).rev() {
            let lhs_span = ctx.get_node(lhs).span();
            let ret_span = ctx.get_node(ret).span();
            let span = ret_span.merge(lhs_span);

            ret = ctx.add_node(span, ast::ExprData::BinaryOp(lhs, op, ret));
        }
    } else {
        // Take out the first/leftmost expression; that's the deepest
        // AST node.
        let mut exprs_iter = exprs.into_iter();
        ret = exprs_iter.next().unwrap();
        // Pair up the remaining operators and expressions, and make a new
        // AST node using the previous iteration as the left-hand side of
        // the expression.
        for (op, rhs) in ops.into_iter().zip(exprs_iter) {
            let ret_span = ctx.get_node(ret).span();
            let rhs_span = ctx.get_node(rhs).span();
            let span = ret_span.merge(rhs_span);

            ret = ctx.add_node(span, ast::ExprData::BinaryOp(ret, op, rhs));
        }
    }
    Ok(ret)
}

/// Parses an expression with any number of prefix operators.
fn parse_prefix_ops(
    p: &mut Parser<'_>,
    ctx: &mut Ctx<'_>,
    precedence: OpPrecedence,
) -> Result<ast::ExprId> {
    let allowed_ops = precedence.prefix_ops();

    // Build a list of operators in the order that they appear in the source
    // code.
    let mut ops: Vec<Spanned<ast::PrefixOp>> = vec![];
    let op_syntax_rule =
        TryFromToken::<ast::PrefixOp>::with_display("prefix operator, such as '-' or '~'");
    while let Some(op) = p
        .parse(ctx, op_syntax_rule)
        .ok()
        .filter(|op| allowed_ops.contains(op))
    {
        ops.push(op);
    }
    let mut ret = p.parse(ctx, ExpressionWithPrecedence(precedence.next()))?;
    let mut span = ctx.get_node(ret).span();
    // Now pop the operators off the list from right-to-left.
    for op in ops {
        span = span.merge(op.span);
        ret = ctx.add_node(span, ast::ExprData::PrefixOp(op, ret));
    }
    Ok(ret)
}

/// Parses an expression with any number of suffixes.
fn parse_suffix_expr(p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<ast::ExprId> {
    // Parse the initial expression.
    let mut ret = p.parse(ctx, ExpressionWithPrecedence(OpPrecedence::Suffix.next()))?;
    // Repeatedly try to consume a suffix.
    while let Some(result) = parse_one_of!(
        p,
        ctx,
        [
            MethodCallSuffix(ret).map(Some),
            FuncCallSuffix(ret).map(Some),
            IndexSuffix(ret).map(Some),
            Epsilon.map(|_| None),
        ],
    )? {
        ret = result;
    }
    Ok(ret)
}

/// Matches an attribute access or method call suffix, such as `.y` or `.y()`
/// (presumably preceded by an expression).
struct MethodCallSuffix(ExprId);
impl_display!(for MethodCallSuffix, "method/attribute access");
impl SyntaxRule for MethodCallSuffix {
    type Output = ast::ExprId;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::Period)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse(ctx, Token::Period)?;
        let attr = p.parse(ctx, Identifier)?;

        let obj = self.0;
        let args_list = p
            .try_parse(ctx, List::paren_comma_sep(Expression))
            .unwrap_or(Ok(Spanned {
                span: attr.span,
                node: vec![],
            }))?;
        let args = args_list.node;
        let expr_data = ast::ExprData::MethodCall { obj, attr, args };

        let obj_expr_span = ctx.get_node(obj).span();
        let total_span = obj_expr_span.merge(args_list.span);
        Ok(ctx.add_node(total_span, expr_data))
    }
}

/// Matches a function call suffix, such as `(arg1, arg2)` (presumably preceded by an
/// expression that evaluates to a function value).
struct FuncCallSuffix(ExprId);
impl_display!(for FuncCallSuffix, "function call");
impl SyntaxRule for FuncCallSuffix {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        List::paren_comma_sep(Expression).might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        let func = self.0;
        let args_list = p.parse(ctx, List::paren_comma_sep(Expression))?;
        let args = args_list.node;
        let expr_data = ast::ExprData::FuncCall { func, args };

        let func_expr_span = ctx.get_node(func).span();
        let total_span = func_expr_span.merge(args_list.span);
        Ok(ctx.add_node(total_span, expr_data))
    }
}

/// Matches an indexing suffix, such as `[x, y]` (presumably preceded by an
/// expression).
struct IndexSuffix(ExprId);
impl_display!(for IndexSuffix, "indexing expression using square brackets");
impl SyntaxRule for IndexSuffix {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        List::bracket_comma_sep(Expression).might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        let obj = self.0;
        let args_list = p.parse(ctx, List::paren_comma_sep(Expression))?;
        let args = args_list.node;
        let expr_data = ast::ExprData::IndexOp { obj, args };

        let obj_expr_span = ctx.get_node(obj).span();
        let total_span = obj_expr_span.merge(args_list.span);
        Ok(ctx.add_node(total_span, expr_data))
    }
}

/// Consumes an identifier and wraps it in an expression.
#[derive(Debug, Copy, Clone)]
pub struct IdentifierExpression;
impl_display!(for IdentifierExpression, "identifier, such as a variable or function name");
impl SyntaxRule for IdentifierExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        Identifier.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ctx, |p, ctx| {
            let ident = p.parse(ctx, Identifier)?;
            Ok(ctx.lookup_symbol(ident.span, &ident.node))
        })
    }
}

/// Consumes an identifier and wraps it in an expression.
#[derive(Debug, Copy, Clone)]
pub struct StringLiteralExpression;
impl_display!(for StringLiteralExpression, "string literal");
impl SyntaxRule for StringLiteralExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        StringLiteral.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ctx, |p, ctx| {
            Ok(ast::ExprData::Constant(Ok(Value::String(Rc::new(
                p.parse(ctx, StringLiteral)?.node,
            )))))
        })
    }
}

/// Consumes an identifier and wraps it in an expression.
#[derive(Debug, Copy, Clone)]
pub struct IntegerLiteralExpression;
impl_display!(for IntegerLiteralExpression, "string literal");
impl SyntaxRule for IntegerLiteralExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        IntegerLiteral.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ctx, |p, ctx| {
            Ok(ast::ExprData::Constant(Ok(Value::Integer(
                p.parse(ctx, IntegerLiteral)?,
            ))))
        })
    }
}

/// Consumes an identifier and wraps it in an expression.
#[derive(Debug, Copy, Clone)]
pub struct VectorLiteralExpression;
impl_display!(for VectorLiteralExpression, "string literal");
impl SyntaxRule for VectorLiteralExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        VectorLiteral.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ctx, |p, ctx| {
            Ok(ast::ExprData::VectorConstruct(
                p.parse(ctx, VectorLiteral)?.node,
            ))
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ParenExpression;
impl_display!(for ParenExpression, "{}", Surround::paren(Expression));
impl SyntaxRule for ParenExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        Surround::paren(Expression).might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ctx, |p, ctx| {
            Ok(ast::ExprData::Paren(
                p.parse(ctx, Surround::paren(Expression))?.node,
            ))
        })
    }
}
