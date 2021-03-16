use super::*;
use crate::lexer::*;
use crate::parser::tree;
use crate::{Span, Spanned};

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
    Range,
    UnaryPrefix,
    Exp,
    Postfix,
    Atom,
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
            Self::MulDiv => Self::Range,
            Self::Range => Self::UnaryPrefix,
            Self::UnaryPrefix => Self::Exp,
            Self::Exp => Self::Postfix,
            Self::Postfix => Self::Atom,
            Self::Atom => panic!("Tried to get operator precedence level beyond {:?}", self),
        }
    }
}
impl Default for OpPrecedence {
    fn default() -> Self {
        Self::lowest()
    }
}

/// Consumes an expression.
#[derive(Debug, Copy, Clone)]
pub struct Expr;
impl_display!(Expr, "expression");
impl SyntaxConstruct for Expr {
    type Output = Spanned<tree::Expr>;
    fn might_match(&self, tf: Parser<'_>) -> bool {
        ExprWithPrecedence::default().might_match(tf)
    }
    fn eat(&self, tf: &mut Parser<'_>) -> LangResult<Self::Output> {
        ExprWithPrecedence::default().build(tf)
    }
}

/// Consumes an expression with the given precedence level.
#[derive(Debug, Default, Copy, Clone)]
pub struct ExprWithPrecedence(pub OpPrecedence);
impl_display!(ExprWithPrecedence, "expression");
impl SyntaxConstruct for ExprWithPrecedence {
    type Output = Spanned<tree::Expr>;
    fn might_match(&self, tf: Parser<'_>) -> bool {
        // So many TokenClasses might match here that it's better to just
        // enumerate all of them to be sure we haven't missed any.
        use TokenClass::*;
        match tf.peek_next_class() {
            None => false,
            Some(t) => match t {
                Keyword(KeywordToken::Not) => true,
                Keyword(_) => false,
                Type(_) => true,
                Assignment(_) => false,
                Comparison(_) => false,
                Operator(_) => true,
                Punctuation(PunctuationToken::LParen) => true,
                Punctuation(PunctuationToken::LBracket) => true,
                Punctuation(_) => false,
                Integer(_) => true,
                String { .. } => true,
                Directive(_) => false,
                Tag(_) => true,
                Ident(_) => true,
                Unknown(_) => false,
                Comment => false, // shouldn't even be possible
            },
        }
    }
    fn eat(&self, tf: &mut Parser<'_>) -> LangResult<Self::Output> {
        // Get an expression at the given precedence level, which may
        // consist of expressions with higher precedence.
        use OperatorToken::*;
        match self.0 {
            OpPrecedence::LogicalOr => {
                eat_binary_op(tf, self.0, Associativity::Left, |t| match t {
                    TokenClass::Keyword(KeywordToken::Or) => Some(Box::new(tree::Expr::LogicalOr)),
                    _ => None,
                })
            }
            OpPrecedence::LogicalXor => {
                eat_binary_op(tf, self.0, Associativity::Left, |t| match t {
                    TokenClass::Keyword(KeywordToken::Xor) => {
                        Some(Box::new(tree::Expr::LogicalXor))
                    }
                    _ => None,
                })
            }
            OpPrecedence::LogicalAnd => {
                eat_binary_op(tf, self.0, Associativity::Left, |t| match t {
                    TokenClass::Keyword(KeywordToken::And) => {
                        Some(Box::new(tree::Expr::LogicalAnd))
                    }
                    _ => None,
                })
            }
            OpPrecedence::LogicalNot => eat_unary_op(tf, self.0, |t| match t {
                TokenClass::Keyword(KeywordToken::Not) => Some(Box::new(tree::Expr::LogicalNot)),
                _ => None,
            }),
            OpPrecedence::Comparison => ComparisonChain.build(tf),
            OpPrecedence::Is => eat_binary_op(tf, self.0, Associativity::Left, |t| match t {
                TokenClass::Keyword(KeywordToken::Is) => Some(Box::new(tree::Expr::Is)),
                _ => None,
            }),
            OpPrecedence::BitwiseOr => eat_left_binary_op(tf, self.0, &[Pipe]),
            OpPrecedence::BitwiseAnd => eat_left_binary_op(tf, self.0, &[Ampersand]),
            OpPrecedence::BitwiseXor => eat_left_binary_op(tf, self.0, &[Caret]),
            OpPrecedence::Bitshift => eat_left_binary_op(
                tf,
                self.0,
                &[DoubleLessThan, DoubleGreaterThan, TripleGreaterThan],
            ),
            OpPrecedence::AddSub => eat_left_binary_op(tf, self.0, &[Plus, Minus]),
            OpPrecedence::MulDiv => eat_left_binary_op(tf, self.0, &[Asterisk, Slash, Percent]),
            OpPrecedence::Range => eat_left_binary_op(tf, self.0, &[DotDot]),
            OpPrecedence::UnaryPrefix => eat_unary_op(tf, self.0, |t| match t {
                // Once rust-lang#54883 is resolved, this line can be simplified.
                TokenClass::Operator(op) if matches!(op, Tag | Plus | Minus | Tilde) => {
                    Some(Box::new(move |operand| tree::Expr::UnaryOp { op, operand }))
                }
                _ => None,
            }),
            OpPrecedence::Exp => eat_right_binary_op(tf, self.0, &[DoubleAsterisk]),
            OpPrecedence::Postfix => {
                let mut ret = tf.parse(Self(self.0.next()))?;
                while let Some(expr_wrapper) = feed_one_of!(
                    tf,
                    [
                        // Attribute access: x.y
                        AttrGetSuffix.map(Some),
                        // Function calls: x(y, z)
                        FnCallSuffix.map(Some),
                        // Indexing: x[y, z]
                        IndexSuffix.map(Some),
                        // No suffix
                        Epsilon.map(|_| None),
                    ]
                )? {
                    ret = expr_wrapper(Box::new(ret));
                }
                Ok(ret)
            }
            OpPrecedence::Atom => {
                feed_one_of!(
                    tf,
                    [
                        Identifier.map(tree::Expr::Ident).spanned().map(Some),
                        // Tag.map(tree::Expr::_).spanned(),
                        TypeName.map(tree::Expr::TypeName).spanned().map(Some),
                        IntLiteral.map(tree::Expr::Int).spanned().map(Some),
                        StencilLiteral.spanned().map(Some),
                        Surround::paren(Expr)
                            .map(Box::new)
                            .map(tree::Expr::ParenExpr)
                            .spanned()
                            .map(Some),
                        List::bracket_comma_sep(Expr)
                            .map(tree::Expr::Vector)
                            .spanned()
                            .map(Some),
                        Epsilon.map(|_| None),
                    ],
                )
                .transpose()
                .unwrap_or_else(|| tf.expected(self))
            }
        }
    }
}

/// Left- or right-associtivity for operators.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Associativity {
    // Left-associative (e.g. subtraction)
    Left,
    // Right-associative (e.g. exponentiation)
    Right,
}

/// Consumes left-associative binary operator expressions using the given
/// `OperatorToken`s.
fn eat_left_binary_op(
    tf: &mut Parser,
    precedence: OpPrecedence,
    ops: &[OperatorToken],
) -> LangResult<Spanned<tree::Expr>> {
    eat_binary_op(tf, precedence, Associativity::Left, |t| match t {
        TokenClass::Operator(op) if ops.contains(&op) => Some(Box::new(move |lhs, rhs| {
            tree::Expr::BinaryOp { lhs, op, rhs }
        })),
        _ => None,
    })
}

/// Consumes right-associative binary operator expressions using the given
/// `OperatorToken`s.
fn eat_right_binary_op(
    tf: &mut Parser,
    precedence: OpPrecedence,
    ops: &[OperatorToken],
) -> LangResult<Spanned<tree::Expr>> {
    eat_binary_op(tf, precedence, Associativity::Right, |t| match t {
        TokenClass::Operator(op) if ops.contains(&op) => Some(Box::new(move |lhs, rhs| {
            tree::Expr::BinaryOp { lhs, op, rhs }
        })),
        _ => None,
    })
}

/// Function that combines two expression ASTs into one, presumably using some
/// binary operator.
type BinaryOpExprConstructor =
    Box<dyn Fn(Box<Spanned<tree::Expr>>, Box<Spanned<tree::Expr>>) -> tree::Expr>;
/// Consumes binary operator expressions, given a function that maps tokens to
/// `BinaryOpExprConstructor`s.
fn eat_binary_op(
    tf: &mut Parser,
    precedence: OpPrecedence,
    assoc: Associativity,
    op_to_expr: impl Fn(TokenClass) -> Option<BinaryOpExprConstructor>,
) -> LangResult<Spanned<tree::Expr>> {
    let expr_eater = ExprWithPrecedence(precedence.next());
    // First, just make a list of operators and expressions. ops.len() should
    // always be equal to exprs.len() - 1.
    let mut ops = vec![];
    let mut exprs = vec![tf.parse(expr_eater)?];
    while let Some(op_fn) = tf.peek_next_class().and_then(&op_to_expr) {
        tf.next();
        ops.push(op_fn);
        exprs.push(tf.parse(expr_eater)?);
    }
    let mut ret;
    match assoc {
        Associativity::Left => {
            // Take out the first/leftmost expression; that's the deepest AST
            // node.
            let mut exprs_iter = exprs.into_iter();
            ret = exprs_iter.next().unwrap();
            // Pair up the remaining operators and expressions, and make a new
            // AST node using the previous iteration as the left-hand side of
            // the expression.
            for (op, rhs) in ops.into_iter().zip(exprs_iter) {
                let span = Span::merge(&ret, &rhs);
                ret = Spanned {
                    span,
                    inner: op(Box::new(ret), Box::new(rhs)),
                };
            }
        }
        Associativity::Right => {
            // Take out the last/rightmost expression; that's the deepest AST
            // node.
            ret = exprs.pop().unwrap();
            // Pair up the remaining operators and expressions, and make a new
            // AST node using the previous iteration as the right-hand side of
            // the expression.
            for (lhs, op) in exprs.into_iter().zip(ops) {
                let span = Span::merge(&lhs, &ret);
                ret = Spanned {
                    span,
                    inner: op(Box::new(lhs), Box::new(ret)),
                };
            }
        }
    }
    Ok(ret)
}

/// Function that transforms one expression AST into a new one, presumably using
/// some unary operator.
type UnaryOpExprConstructor = Box<dyn Fn(Box<Spanned<tree::Expr>>) -> tree::Expr>;
/// Consumes unary operator expressions, given a function that maps tokens to
/// `UnaryOpExprConstructor`s.
fn eat_unary_op(
    tf: &mut Parser,
    precedence: OpPrecedence,
    op_to_expr: impl Fn(TokenClass) -> Option<UnaryOpExprConstructor>,
) -> LangResult<Spanned<tree::Expr>> {
    let expr_eater = ExprWithPrecedence(precedence.next());
    // First, make a list of operators and their spans, in the order they appear
    // in the source code.
    let mut ops = vec![];
    let mut op_spans = vec![];
    while let Some(op_fn) = tf.peek_next_class().and_then(&op_to_expr) {
        op_spans.push(tf.next().unwrap().span);
        ops.push(op_fn);
    }
    let mut ret = tf.parse(expr_eater)?;
    for (op, op_span) in ops.into_iter().zip(op_spans) {
        // Now pop off the next-rightmost operator each time.
        ret = Spanned {
            span: Span::merge(op_span, &ret),
            inner: op(Box::new(ret)),
        };
    }
    Ok(ret)
}

/// Consumes a comparison expression.
struct ComparisonChain;
impl_display!(ComparisonChain, "comparison expression");
impl SyntaxConstruct for ComparisonChain {
    type Output = Spanned<tree::Expr>;
    fn might_match(&self, tf: Parser<'_>) -> bool {
        ExprWithPrecedence(OpPrecedence::Comparison.next()).might_match(tf)
    }
    fn eat(&self, tf: &mut Parser<'_>) -> LangResult<Self::Output> {
        let expr_eater = ExprWithPrecedence(OpPrecedence::Comparison.next());
        let mut exprs = vec![tf.parse(expr_eater)?];
        let mut cmps = vec![];
        loop {
            match tf.peek_next_class() {
                Some(TokenClass::Comparison(cmp)) => {
                    tf.next().unwrap();
                    cmps.push(cmp);
                    exprs.push(tf.parse(expr_eater)?);
                }
                _ => break,
            }
        }
        if cmps.is_empty() {
            Ok(exprs.pop().unwrap())
        } else {
            let span = Span::merge(exprs.first().unwrap(), exprs.last().unwrap());
            Ok(Spanned {
                span,
                inner: tree::Expr::Cmp { exprs, cmps },
            })
        }
    }
}

/// Consumes an attribute access suffix, such as `x.y` (not including `x`).
struct AttrGetSuffix;
impl_display!(AttrGetSuffix, "method/attribute access");
impl SyntaxConstruct for AttrGetSuffix {
    type Output = Box<dyn FnOnce(Box<Spanned<tree::Expr>>) -> Spanned<tree::Expr>>;
    fn might_match(&self, tf: Parser<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Punctuation(PunctuationToken::Period))
    }
    fn eat(&self, tf: &mut Parser<'_>) -> LangResult<Self::Output> {
        tf.parse(PunctuationToken::Period)?;
        let attribute = tf.parse(Identifier.spanned())?;
        Ok(Box::new(|object| Spanned {
            span: Span::merge(&*object, &attribute),
            inner: tree::Expr::GetAttr { object, attribute },
        }))
    }
}

/// Consumes a function call suffix, such as `f(x, y)` (not including `f`).
struct FnCallSuffix;
impl_display!(FnCallSuffix, "function call using parentheses");
impl SyntaxConstruct for FnCallSuffix {
    type Output = Box<dyn FnOnce(Box<Spanned<tree::Expr>>) -> Spanned<tree::Expr>>;
    fn might_match(&self, tf: Parser<'_>) -> bool {
        List::paren_comma_sep(Expr).might_match(tf)
    }
    fn eat(&self, tf: &mut Parser<'_>) -> LangResult<Self::Output> {
        let spanned_args = tf.parse(List::paren_comma_sep(Expr).spanned())?;
        let args_span = spanned_args.span;
        let args = spanned_args.inner;
        Ok(Box::new(move |func| Spanned {
            span: Span::merge(&*func, args_span),
            inner: tree::Expr::FnCall { func, args },
        }))
    }
}

/// Consumes an indexing suffix, such as `a[x, y]` (not including `a`).
struct IndexSuffix;
impl_display!(IndexSuffix, "indexing expression using square brackets");
impl SyntaxConstruct for IndexSuffix {
    type Output = Box<dyn FnOnce(Box<Spanned<tree::Expr>>) -> Spanned<tree::Expr>>;
    fn might_match(&self, tf: Parser<'_>) -> bool {
        List::bracket_comma_sep(Expr).might_match(tf)
    }
    fn eat(&self, tf: &mut Parser<'_>) -> LangResult<Self::Output> {
        let spanned_args = tf.parse(List::bracket_comma_sep(Expr).spanned())?;
        let args_span = spanned_args.span;
        let args = spanned_args.inner;
        Ok(Box::new(move |object| Spanned {
            span: Span::merge(&*object, args_span),
            inner: tree::Expr::Index { object, args },
        }))
    }
}
