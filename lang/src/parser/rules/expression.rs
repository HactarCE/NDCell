use codemap::Spanned;
use std::sync::Arc;

use super::{
    Epsilon, Identifier, IntegerLiteral, List, Parser, SetLiteral, StringLiteral, Surround,
    SyntaxRule, TryFromToken, VectorLiteral,
};
use crate::ast;
use crate::data::RtVal;
use crate::errors::{Error, Result};
use crate::lexer::{Keyword, Token};

/// Operator precedence table.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpPrecedence {
    LogicalOr,
    LogicalXor,
    LogicalAnd,
    LogicalNot,
    Comparison,
    Is,
    Or,
    Xor,
    And,
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
            Self::Is => Self::Or,
            Self::Or => Self::Xor,
            Self::Xor => Self::And,
            Self::And => Self::Bitshift,
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
            Self::Or => &[Or],
            Self::Xor => &[Xor],
            Self::And => &[And],
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
            Self::Or => &[],
            Self::Xor => &[],
            Self::And => &[],
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
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse(ast, ExpressionWithPrecedence::default())
    }
}

/// Matches an expression with the given precedence level.
#[derive(Debug, Default, Copy, Clone)]
struct ExpressionWithPrecedence(pub OpPrecedence);
impl_display!(for ExpressionWithPrecedence, "expression");
impl SyntaxRule for ExpressionWithPrecedence {
    type Output = ast::ExprId;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        if let Some(t) = p.next() {
            // There are so many tokens that might match, it's more reliable to
            // just match all of them.
            match t {
                Token::LParen | Token::LBracket | Token::LBrace => true,

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
                | Token::Caret => false,

                Token::Tilde => true,

                Token::Arrow | Token::DotDot => false,

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

                Token::Keyword(kw) => match kw {
                    Keyword::Or | Keyword::Xor | Keyword::And => false,

                    Keyword::Not => true,

                    Keyword::Is | Keyword::In => false,

                    Keyword::Break
                    | Keyword::Continue
                    | Keyword::For
                    | Keyword::Become
                    | Keyword::Remain
                    | Keyword::Return
                    | Keyword::If
                    | Keyword::Else
                    | Keyword::Unless
                    | Keyword::Case
                    | Keyword::Match
                    | Keyword::Assert
                    | Keyword::Error => false,
                },

                Token::Comment | Token::UnterminatedBlockComment => false,

                Token::StringLiteral
                | Token::UnterminatedStringLiteral
                | Token::IntegerLiteral
                | Token::Ident => true,

                Token::Whitespace => false,
                Token::Unknown => false,
            }
        } else {
            false
        }
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        // Consume an expression at the given precedence level, which may
        // consist of expressions with higher precedence.
        match self.0 {
            OpPrecedence::Atom => parse_one_of!(
                p,
                ast,
                [
                    IdentifierExpression.map(Some),
                    StringLiteralExpression.map(Some),
                    IntegerLiteralExpression.map(Some),
                    VectorLiteralExpression.map(Some),
                    SetLiteralExpression.map(Some),
                    ParenExpression.map(Some),
                    Epsilon.map(|_| None),
                ],
            )
            .transpose()
            .unwrap_or_else(|| p.expected(self)),

            OpPrecedence::Suffix => parse_suffix_expr(p, ast),

            OpPrecedence::Comparison => parse_cmp_chain_expr(p, ast),

            prec if !prec.binary_ops().is_empty() => parse_binary_ops_expr(p, ast, prec),
            prec if !prec.prefix_ops().is_empty() => parse_prefix_ops(p, ast, prec),
            prec => internal_error!("don't know what to do for precedence {:?}", prec),
        }
    }
}

/// Parses an expression with any number of binary operators at a specific
/// precedence level.
fn parse_binary_ops_expr(
    p: &mut Parser<'_>,
    ast: &'_ mut ast::Program,
    precedence: OpPrecedence,
) -> Result<ast::ExprId> {
    let recursive_expression = ExpressionWithPrecedence(precedence.next());

    let allowed_ops = precedence.binary_ops();

    // First, just make a list of operators and expressions. `ops.len()`
    // should always be equal to `exprs.len() - 1`.
    let mut ops: Vec<Spanned<ast::BinaryOp>> = vec![];
    let mut exprs: Vec<ast::ExprId> = vec![p.parse(ast, recursive_expression)?];
    let op_syntax_rule =
        TryFromToken::<ast::BinaryOp>::with_display("binary operator, such as '+' or '*'");
    loop {
        if let Ok(op) = p.clone().parse(ast, op_syntax_rule) {
            if allowed_ops.contains(&op.node) {
                ops.push(p.parse(ast, op_syntax_rule)?);
                exprs.push(p.parse(ast, recursive_expression)?);
                continue;
            }
        }
        break;
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
            let lhs_span = ast.get_node(lhs).span();
            let ret_span = ast.get_node(ret).span();
            let span = ret_span.merge(lhs_span);

            ret = ast.add_node(span, ast::ExprData::BinaryOp(lhs, op, ret));
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
            let ret_span = ast.get_node(ret).span();
            let rhs_span = ast.get_node(rhs).span();
            let span = ret_span.merge(rhs_span);

            ret = ast.add_node(span, ast::ExprData::BinaryOp(ret, op, rhs));
        }
    }
    Ok(ret)
}

/// Parses an expression with any number of prefix operators.
fn parse_prefix_ops(
    p: &mut Parser<'_>,
    ast: &'_ mut ast::Program,
    precedence: OpPrecedence,
) -> Result<ast::ExprId> {
    let allowed_ops = precedence.prefix_ops();

    // Build a list of operators in the order that they appear in the source
    // code.
    let mut ops: Vec<Spanned<ast::PrefixOp>> = vec![];
    let op_syntax_rule =
        TryFromToken::<ast::PrefixOp>::with_display("prefix operator, such as '-' or '~'");
    loop {
        if let Ok(op) = p.clone().parse(ast, op_syntax_rule) {
            if allowed_ops.contains(&op.node) {
                ops.push(p.parse(ast, op_syntax_rule)?);
                continue;
            }
        }
        break;
    }
    let mut ret = p.parse(ast, ExpressionWithPrecedence(precedence.next()))?;
    let mut span = ast.get_node(ret).span();
    // Now pop the operators off the list from right to left.
    for op in ops {
        span = span.merge(op.span);
        ret = ast.add_node(span, ast::ExprData::PrefixOp(op, ret));
    }
    Ok(ret)
}

/// Parses an expression with any number of suffixes.
fn parse_suffix_expr(p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<ast::ExprId> {
    // Parse the initial expression.
    let mut ret = p.parse(ast, ExpressionWithPrecedence(OpPrecedence::Suffix.next()))?;

    // Repeatedly try to consume a suffix.
    while let Some(result) = parse_one_of!(
        p,
        ast,
        [
            MethodCallSuffix(ret).map(Some),
            FuncCallSuffix(ret).map(Some),
            IndexSuffix(ret).map(Some),
            // TODO tag suffix
            Epsilon.map(|_| None),
        ],
    )? {
        ret = result;
    }
    Ok(ret)
}

/// Parse an expression consisting of a comparison chain, such as `a < b == c > d`.
fn parse_cmp_chain_expr(p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<ast::ExprId> {
    let recursive_expression = ExpressionWithPrecedence(OpPrecedence::Comparison.next());

    // Parse the initial expression.
    let ret = p.parse(ast, recursive_expression)?;

    let op_syntax_rule =
        TryFromToken::<ast::CompareOp>::with_display("comparison operator, such as '<' or '=='");
    if op_syntax_rule.might_match(*p) {
        // If there is a comparison operator, parse a comparison chain.
        let mut exprs = vec![ret];
        let mut ops = vec![];
        while let Some(op) = p.try_parse(ast, op_syntax_rule) {
            ops.push(op?);
            exprs.push(p.parse(ast, recursive_expression)?);
        }
        let span1 = ast.get_node(exprs[0]).span();
        let span2 = ast.get_node(exprs[exprs.len() - 1]).span();
        Ok(ast.add_node(span1.merge(span2), ast::ExprData::CmpChain(exprs, ops)))
    } else {
        // Otherwise, just return the first expression.
        Ok(ret)
    }
}

/// Matches an attribute access or method call suffix, such as `.y` or `.y()`
/// (presumably preceded by an expression).
#[derive(Debug, Copy, Clone)]
struct MethodCallSuffix(ast::ExprId);
impl_display!(for MethodCallSuffix, "method/attribute access");
impl SyntaxRule for MethodCallSuffix {
    type Output = ast::ExprId;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::Period)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse(ast, Token::Period)?;
        let attr = p.parse(ast, Identifier)?;

        let obj = self.0;
        let mut args = p
            .try_parse(ast, List::paren_comma_sep(FuncArg))
            .unwrap_or(Ok(Spanned {
                span: attr.span,
                node: vec![],
            }))?;
        args.node.insert(0, (None, obj)); // Add method reciever as first argument.

        let obj_expr_span = ast.get_node(obj).span();
        let total_span = obj_expr_span.merge(args.span);
        let expr_data = ast::ExprData::MethodCall { attr, args };
        Ok(ast.add_node(total_span, expr_data))
    }
}

/// Matches a function call suffix, such as `(arg1, arg2)` (presumably preceded by an
/// expression that evaluates to a function value).
#[derive(Debug, Copy, Clone)]
struct FuncCallSuffix(ast::ExprId);
impl_display!(for FuncCallSuffix, "function call");
impl SyntaxRule for FuncCallSuffix {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        List::paren_comma_sep(Expression).might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        let func_name_expr = ast.get_node(self.0);
        let func_name_span = func_name_expr.span();
        let func = match func_name_expr.data() {
            ast::ExprData::Identifier(func_name) => Spanned {
                node: Arc::clone(func_name),
                span: func_name_span,
            },
            _ => return Err(Error::cannot_call_arbitrary_expression(func_name_span)),
        };

        let args_list = p.parse(ast, List::paren_comma_sep(FuncArg))?;
        let args = args_list.node;
        let expr_data = ast::ExprData::FuncCall { func, args };

        let total_span = func_name_span.merge(args_list.span);
        Ok(ast.add_node(total_span, expr_data))
    }
}

/// Matches a function argument, which consists of an expression optionally
/// preceded by an identifier and '='.
#[derive(Debug, Copy, Clone)]
struct FuncArg;
impl_display!(for FuncArg, "expression, optionally preceded by an identifier and '='");
impl SyntaxRule for FuncArg {
    type Output = (Option<Spanned<Arc<String>>>, ast::ExprId);

    fn might_match(&self, p: Parser<'_>) -> bool {
        Identifier.might_match(p) || Expression.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        let old_state = *p;
        {
            let ident = p.parse(ast, Identifier);
            if p.next() == Some(Token::Assign) {
                return Ok((Some(ident?), p.parse(ast, Expression)?));
            }
        }
        *p = old_state;
        Ok((None, p.parse(ast, Expression)?))
    }
}

/// Matches an indexing suffix, such as `[x, y]` (presumably preceded by an
/// expression).
#[derive(Debug, Copy, Clone)]
struct IndexSuffix(ast::ExprId);
impl_display!(for IndexSuffix, "indexing expression using square brackets");
impl SyntaxRule for IndexSuffix {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        List::bracket_comma_sep(Expression).might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        let obj = self.0;
        let mut args = p.parse(ast, List::bracket_comma_sep(Expression))?;
        args.node.insert(0, obj); // Add object being indexed as first argument.

        let obj_expr_span = ast.get_node(obj).span();
        let total_span = obj_expr_span.merge(args.span);
        let expr_data = ast::ExprData::IndexOp { args };
        Ok(ast.add_node(total_span, expr_data))
    }
}

/// Matches an identifier and wraps it in an expression.
#[derive(Debug, Copy, Clone)]
pub struct IdentifierExpression;
impl_display!(for IdentifierExpression, "identifier, such as a variable or function name");
impl SyntaxRule for IdentifierExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        Identifier.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ast, |p, ast| {
            Ok(ast::ExprData::Identifier(p.parse(ast, Identifier)?.node))
        })
    }
}

/// Matches a string literal and wraps it in an expression.
#[derive(Debug, Copy, Clone)]
pub struct StringLiteralExpression;
impl_display!(for StringLiteralExpression, "string literal");
impl SyntaxRule for StringLiteralExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        StringLiteral.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ast, |p, ast| {
            Ok(ast::ExprData::Constant(RtVal::String(
                p.parse(ast, StringLiteral)?.node,
            )))
        })
    }
}

/// Matches an integer literal and wraps it in an expression.
#[derive(Debug, Copy, Clone)]
pub struct IntegerLiteralExpression;
impl_display!(for IntegerLiteralExpression, "string literal");
impl SyntaxRule for IntegerLiteralExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        IntegerLiteral.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ast, |p, ast| {
            Ok(ast::ExprData::Constant(RtVal::Integer(
                p.parse(ast, IntegerLiteral)?,
            )))
        })
    }
}

/// Matches a vector literal delimited by square brackets and wraps it in an
/// expression.
#[derive(Debug, Copy, Clone)]
pub struct VectorLiteralExpression;
impl_display!(for VectorLiteralExpression, "string literal");
impl SyntaxRule for VectorLiteralExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        VectorLiteral.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ast, |p, ast| {
            Ok(ast::ExprData::VectorConstruct(
                p.parse(ast, VectorLiteral)?.node,
            ))
        })
    }
}

/// Matches a set literal delimited by curly braces and wraps it in an
/// expression.
#[derive(Debug, Copy, Clone)]
pub struct SetLiteralExpression;
impl_display!(for SetLiteralExpression, "set literal");
impl SyntaxRule for SetLiteralExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        SetLiteral.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ast, |p, ast| {
            Ok(ast::ExprData::SetConstruct(p.parse(ast, SetLiteral)?.node))
        })
    }
}

/// Matches a pair of parentheses containing an expression.
#[derive(Debug, Copy, Clone)]
pub struct ParenExpression;
impl_display!(for ParenExpression, "{}", Surround::paren(Expression));
impl SyntaxRule for ParenExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        Surround::paren(Expression).might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ast, |p, ast| {
            Ok(ast::ExprData::Paren(
                p.parse(ast, Surround::paren(Expression))?.node,
            ))
        })
    }
}
