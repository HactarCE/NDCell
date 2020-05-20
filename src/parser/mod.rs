//! Functions for producing a parse tree.
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

mod tree;

pub use tree::*;

use super::errors::*;
use super::lexer::*;
use super::{Span, Spanned};
use LangErrorMsg::{
    ElseWithoutIf, Expected, InternalError, InvalidDirectiveName, MissingSetKeyword, ReservedWord,
    TopLevelNonDirective, Unimplemented, Unmatched,
};

/// Parses the given tokens and returns a ParseTree.
pub fn parse(source_code: Rc<String>, tokens: &[Token]) -> LangResult<ParseTree> {
    let mut directives: HashMap<Directive, Vec<Spanned<DirectiveContents>>> = HashMap::new();
    for (directive, contents) in ParseBuilder::from(tokens).directives()?.into_iter() {
        directives.entry(directive).or_default().push(contents);
    }
    Ok(ParseTree {
        source_code,
        directives,
    })
}

/// Operator precedence table.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum OpPrecedence {
    // TODO: implement comma- and semicolon-separated lists
    // SemicolonList,
    // CommaList,
    LogicalOr,
    LogicalXor,
    LogicalAnd,
    LogicalNot,
    Comparison,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Bitshift,
    AddSub,
    MulDiv,
    UnaryPrefix,
    Exp,
    Dot,
    ArrayIndex,
    FunctionCall,
    Atom,
}
impl OpPrecedence {
    /// Returns the lowest precedence level.
    const fn lowest() -> Self {
        Self::LogicalOr
    }
    /// Returns the highest precedence level.
    const fn highest() -> Self {
        Self::Atom
    }
    /// Returns the next-highest precedence level. Panics if given
    /// OpPrecedence::Atom.
    fn next(self) -> Self {
        match self {
            Self::LogicalOr => Self::LogicalXor,
            Self::LogicalXor => Self::LogicalAnd,
            Self::LogicalAnd => Self::LogicalNot,
            Self::LogicalNot => Self::Comparison,
            Self::Comparison => Self::BitwiseOr,
            Self::BitwiseOr => Self::BitwiseXor,
            Self::BitwiseXor => Self::BitwiseAnd,
            Self::BitwiseAnd => Self::Bitshift,
            Self::Bitshift => Self::AddSub,
            Self::AddSub => Self::MulDiv,
            Self::MulDiv => Self::UnaryPrefix,
            Self::UnaryPrefix => Self::Exp,
            Self::Exp => Self::Dot,
            Self::Dot => Self::ArrayIndex,
            Self::ArrayIndex => Self::FunctionCall,
            Self::FunctionCall => Self::Atom,
            Self::Atom => panic!("Tried to get operator precedence level beyond {:?}", self),
        }
    }
}

/// Iterator over tokens that produces an untyped AST.
#[derive(Debug, Copy, Clone)]
struct ParseBuilder<'a> {
    /// Tokens to feed.
    tokens: &'a [Token<'a>],
    /// Index of the "current" token (None = before start).
    cursor: Option<usize>,
}
impl<'a> From<&'a [Token<'a>]> for ParseBuilder<'a> {
    fn from(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            cursor: None,
        }
    }
}
impl<'a> ParseBuilder<'a> {
    /// Moves the cursor forward and then returns the element at the cursor.
    fn next(&mut self) -> Option<Token<'a>> {
        // Add 1 or set to zero.
        self.cursor = Some(self.cursor.map(|idx| idx + 1).unwrap_or(0));
        self.current()
    }
    /// Moves the cursor back and then returns the element at the cursor.
    fn prev(&mut self) -> Option<Token<'a>> {
        // Subtract 1 if possible.
        self.cursor = self.cursor.and_then(|idx| idx.checked_sub(1));
        self.current()
    }
    /// Returns the element at the cursor.
    fn current(self) -> Option<Token<'a>> {
        self.cursor.and_then(|idx| self.tokens.get(idx).copied())
    }
    /// Returns the element after the one at the cursor, without mutably moving
    /// the cursor.
    fn peek_next(self) -> Option<Token<'a>> {
        let mut tmp = self;
        tmp.next()
    }

    /// Returns the span of the current token. If there is no current token,
    /// returns an empty span at the begining or end of the input appropriately.
    fn span(&self) -> Span {
        if self.cursor.is_none() {
            // This is the beginning of the token stream; return an empty span
            // at the beginning of the file.
            Span::empty(0)
        } else {
            match self.current() {
                // This is the middle of the token stream.
                Some(t) => t.span,
                // This is the end of the token stream; return an empty span at
                // the end of the file.
                None => self
                    .tokens
                    .last()
                    .map(|t| Span::empty(t.span.end))
                    .unwrap_or(Span::empty(0)),
            }
        }
    }

    /// Returns a LangResult::Err with the span of the current token.
    fn err<T>(&self, msg: LangErrorMsg) -> LangResult<T> {
        Err(msg.with_span(self.span()))
    }
    /// Consumes the next symbol and return return a Spanned { ... } of the
    /// result of the given closure if the closure returns LangResult::Ok;
    /// otherwise rewind the state of the ParseBuilder to before the closure was
    /// run and then return the LangResult::Err.
    fn expect<T>(&mut self, f: impl Fn(&mut Self) -> LangResult<T>) -> LangResult<Spanned<T>> {
        self.expect_spanned(|b| {
            b.next();
            let start = b.span();
            b.prev();
            let ret = f(b);
            let end = b.span();
            ret.map(|t| Spanned {
                span: Span::merge(start, end),
                inner: t,
            })
        })
    }
    fn expect_spanned<T>(
        &mut self,
        f: impl Fn(&mut Self) -> LangResult<Spanned<T>>,
    ) -> LangResult<Spanned<T>> {
        let prior_state = *self;
        let ret = f(self);
        if ret.is_err() {
            *self = prior_state;
        }
        ret
    }
    /// Returns true if the next token exists and has a class that is in the
    /// given list of TokenClasses.
    fn next_token_is_one_of(&self, token_classes: &[TokenClass]) -> bool {
        if let Some(t) = self.peek_next() {
            token_classes.contains(&t.class)
        } else {
            false
        }
    }
    /// Consumes a sequence of directives.
    fn directives(&mut self) -> LangResult<Vec<(Directive, Spanned<DirectiveContents>)>> {
        let mut directives = vec![];
        while self.peek_next().is_some() {
            let Spanned {
                span,
                inner: (dir, contents),
            } = self.expect(Self::directive)?;
            let inner = contents;
            directives.push((dir, Spanned { span, inner }));
        }
        Ok(directives)
    }
    /// Consumes a directive, which includes one or more arguments given to the
    /// directive. The number of arguments consumed depends on the name of the
    /// directive and the type of arguments passed.
    fn directive(&mut self) -> LangResult<(Directive, DirectiveContents)> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Directive(directive_name)) => Ok((
                Directive::try_from(directive_name)
                    .map_err(|_| InvalidDirectiveName.with_span(self.span()))?,
                self.expect(Self::expr_or_block)?.inner,
            )),
            Some(_) => self.err(TopLevelNonDirective),
            None => self.err(Expected("directive")),
        }
    }
    /// Consumes an expression or code block.
    fn expr_or_block(&mut self) -> LangResult<DirectiveContents> {
        match self.peek_next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::LBrace)) => {
                Ok(self.expect(Self::block)?.into())
            }
            Some(_) => Ok(self.expect(Self::expression)?.into()),
            None => self.err(Expected("expression or code block"))?,
        }
    }
    /// Consumes a block, which consists of statements.
    fn block(&mut self) -> LangResult<StatementBlock> {
        // Get a left brace.
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::LBrace)) => (),
            _ => self.err(Expected("code block"))?,
        }
        // Record the span of the left brace.
        let open_span = self.span();
        // Get statements.
        let mut statements = vec![];
        loop {
            match self.next().map(|t| t.class) {
                // There's the beginning of a statement.
                Some(TokenClass::Keyword(kw)) if kw.starts_statement() => {
                    self.prev();
                    statements.push(self.expect(Self::statement)?)
                }
                // There's a closing brace.
                Some(TokenClass::Punctuation(PunctuationToken::RBrace)) => break,
                // There's something else.
                Some(_) => self.err(Expected("statement or '}'"))?,
                // We've reached the end of the file without closing the block.
                None => Err(Unmatched('{', '}').with_span(open_span))?,
            }
        }
        Ok(statements)
    }
    /// Consumes a statement.
    fn statement(&mut self) -> LangResult<Statement> {
        use KeywordToken::*;
        match self.next().map(|t| t.class) {
            Some(TokenClass::Keyword(kw)) if kw.starts_statement() => match kw {
                Become => Ok(Statement::Become(self.expect(Self::expression)?)),
                Break => self.err(Unimplemented),
                Case => self.err(Unimplemented),
                Continue => self.err(Unimplemented),
                Else => self.err(ElseWithoutIf),
                For => self.err(Unimplemented),
                If => Ok(Statement::If {
                    cond_expr: self.expect(Self::expression)?,
                    if_true: self.expect(Self::block)?.inner,
                    if_false: if self.next_token_is_one_of(&[TokenClass::Keyword(Else)]) {
                        // There's an "else" clause.
                        self.next();
                        if self.next_token_is_one_of(&[TokenClass::Keyword(If)]) {
                            // This is actually an "else if" clause. Treat this
                            // as an "if" nested inside an "else."
                            vec![self.expect(Self::statement)?]
                        } else {
                            // This is just a normal "else" clause, not "else
                            // if."
                            self.expect(Self::block)?.inner
                        }
                    } else {
                        // There's no "else" clause, so just pretend that there
                        // is one and it's empty.
                        vec![]
                    },
                }),
                Remain => self.err(Unimplemented),
                Return => self.err(Unimplemented),
                Set => Ok({
                    // Get the variable name.
                    let var_expr = self.expect(Self::ident)?;
                    // Get the operator to use when assigning (if any). E.g.
                    // `+=` uses the `+` operator.
                    let assign_op = self.expect(Self::assign_op)?.inner;
                    // Get the expression to assign into the variable.
                    let value_expr = self.expect(Self::expression)?;
                    // Construct the statement.
                    Statement::SetVar {
                        var_expr,
                        assign_op,
                        value_expr,
                    }
                }),
                Unless => self.err(Unimplemented),
                While => self.err(Unimplemented),
                _ => self.err(Expected("statement")),
            },
            _ => {
                if let Some(TokenClass::Assignment(_)) = self.peek_next().map(|t| t.class) {
                    // Give the user a nicer error message if they forgot the
                    // `set` keyword.
                    self.err(MissingSetKeyword)
                } else {
                    self.err(Expected("statement"))
                }
            }
        }
    }
    /// Consumes a nested expression.
    fn expression(&mut self) -> LangResult<Expr> {
        // Start at the lowest precedence level.
        self.expression_with_precedence(OpPrecedence::lowest())
            .map(|spanned| spanned.inner)
    }
    /// Consumes a nested expression of the given precedence level and higher
    /// recursively using precedence climbing.
    fn expression_with_precedence(
        &mut self,
        precedence: OpPrecedence,
    ) -> LangResult<Spanned<Expr>> {
        // Get an expression at the given precedence level, which may
        // consist of expressions with higher precedence.
        match precedence {
            OpPrecedence::UnaryPrefix => self.unary_op(
                &[
                    TokenClass::Operator(OperatorToken::Tag),
                    TokenClass::Operator(OperatorToken::Minus),
                ],
                precedence,
            ),
            OpPrecedence::AddSub => self.left_binary_op(
                &[
                    TokenClass::Operator(OperatorToken::Plus),
                    TokenClass::Operator(OperatorToken::Minus),
                ],
                precedence,
            ),
            OpPrecedence::MulDiv => self.left_binary_op(
                &[
                    TokenClass::Operator(OperatorToken::Asterisk),
                    TokenClass::Operator(OperatorToken::Slash),
                    TokenClass::Operator(OperatorToken::Percent),
                ],
                precedence,
            ),
            OpPrecedence::Comparison => self.comparison_op(precedence),
            // TODO add remaining precedence levels
            OpPrecedence::Atom => match self.peek_next().map(|t| t.class) {
                Some(TokenClass::Punctuation(PunctuationToken::LParen)) => {
                    // Expressions inside parentheses always start again at the
                    // lowest precedence level.
                    self.expect_spanned(|tf| tf.paren(Self::expression))
                }
                Some(TokenClass::Integer(_)) => self.expect(Self::int),
                Some(TokenClass::String { .. }) => self.err(Unimplemented),
                Some(TokenClass::Tag(_)) => self.err(Unimplemented),
                Some(TokenClass::Ident(_)) => self.expect(Self::ident),
                _ => {
                    self.next();
                    self.err(Expected("expression"))
                }
            },
            _ => self.expression_with_precedence(precedence.next()),
        }
    }
    /// Consumes an expression consisting of any number of the given unary
    /// operators applied to an expression of the given precedence level or
    /// higher.
    fn unary_op(
        &mut self,
        token_classes: &[TokenClass],
        precedence: OpPrecedence,
    ) -> LangResult<Spanned<Expr>> {
        let mut op_tokens = vec![];
        // Get as many unary operators as possible.
        while self.next_token_is_one_of(token_classes) {
            op_tokens.push(self.next().unwrap());
        }
        // Get the actual expression that they are applied to.
        let mut ret = self.expression_with_precedence(precedence.next())?;
        // Process unary operators from right to left so that the leftmost is
        // the outermost.
        for op_token in op_tokens.iter().rev() {
            let operand = Box::new(ret);
            ret = Spanned {
                span: Span::merge(op_token, &*operand),
                inner: match op_token.class {
                    TokenClass::Operator(op) => Expr::UnaryOp { op, operand },
                    other => Err(InternalError(
                        format!("Invalid unary operator: {:?}", other).into(),
                    ))?,
                },
            };
        }
        Ok(ret)
    }
    /// Consumes an expression consisting of any number of the given
    /// left-associative binary operators applied to expressions of the given
    /// precedence level or higher.
    fn left_binary_op(
        &mut self,
        token_classes: &[TokenClass],
        precedence: OpPrecedence,
    ) -> LangResult<Spanned<Expr>> {
        // Get the leftmost expression.
        let initial = self.expression_with_precedence(precedence.next())?;
        // Alternate between getting an operator and an expression.
        let mut op_tokens_and_exprs = vec![];
        while self.next_token_is_one_of(token_classes) {
            op_tokens_and_exprs.push((
                self.next().unwrap(),
                self.expression_with_precedence(precedence.next())?,
            ));
        }
        // Process operations from left to right so that the rightmost is the
        // outermost.
        let mut ret = initial;
        for (op_token, rhs) in op_tokens_and_exprs {
            let lhs = Box::new(ret);
            let rhs = Box::new(rhs);
            ret = Spanned {
                span: Span::merge(&*lhs, &*rhs),
                inner: match op_token.class {
                    TokenClass::Operator(op) => Expr::BinaryOp { lhs, op, rhs },
                    other => Err(InternalError(
                        format!("Invalid unary operator: {:?}", other).into(),
                    ))?,
                },
            };
        }
        Ok(ret)
    }
    /// Consumes an expression consisting of any number of chained comparison
    /// operators. This function is similar to left_binary_op().
    fn comparison_op(&mut self, precedence: OpPrecedence) -> LangResult<Spanned<Expr>> {
        // Get the leftmost expression.
        let mut expressions = vec![self.expression_with_precedence(precedence.next())?];
        let mut comparisons = vec![];
        // Alternate between getting a comparison operator and an expression.
        while let Some(Token {
            class: TokenClass::Comparison(cmp_type),
            ..
        }) = self.peek_next()
        {
            self.next();
            comparisons.push(cmp_type);
            expressions.push(self.expression_with_precedence(precedence.next())?);
        }
        // If there are no comparisons happening, just return the expression.
        if comparisons.is_empty() {
            return Ok(expressions.drain(..).next().unwrap());
        }
        Ok(Spanned {
            // This comparison spans from the leftmost expression to the
            // rightmost expression.
            span: Span::merge(&expressions[0], expressions.last().unwrap()),
            inner: Expr::Cmp {
                exprs: expressions,
                cmps: comparisons,
            },
        })
    }
    /// Consumes an integer literal.
    fn int(&mut self) -> LangResult<Expr> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Integer(i)) => Ok(Expr::Int(i)),
            _ => self.err(Expected("integer")),
        }
    }
    /// Consumes an identifier.
    fn ident(&mut self) -> LangResult<Expr> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Ident(s)) => Ok(Expr::Ident(s.to_owned())),
            Some(TokenClass::Keyword(kw)) => self.err(ReservedWord(kw.to_string().into())),
            _ => self.err(Expected("identifier, e.g. variable name")),
        }
    }
    // Consumes an assignment token and returns the operator used in the
    // assignment, if any. (E.g. `+=` uses the `+` operator, while `=` does not
    // use any operator.)
    fn assign_op(&mut self) -> LangResult<AssignmentToken> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Assignment(a)) => Ok(a),
            _ => self.err(Expected("assignment symbol, e.g. '='")),
        }
    }
    /// Consumes a pair of parentheses with the given matcher run inside.
    fn paren<T>(
        &mut self,
        inner_matcher: impl Fn(&mut Self) -> LangResult<T>,
    ) -> LangResult<Spanned<T>> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::LParen)) => (),
            _ => self.err(Expected("parenthetical expression beginning with '('"))?,
        }
        // Record the span of the left paren.
        let open_span = self.span();
        let expr = self.expect(inner_matcher)?;
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::RParen)) => Ok(expr),
            Some(_) => self.err(Expected("')'")),
            None => Err(Unmatched('(', ')').with_span(open_span)),
        }
    }
}
