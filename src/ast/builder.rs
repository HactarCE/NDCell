use std::convert::TryFrom;

use super::super::errors::*;
use super::super::span::{Span, Spanned};
use super::components::{untyped::*, Cmp, Op};
use super::tokens::*;
use LangErrorMsg::{
    ElseWithoutIf, Expected, InvalidDirectiveName, MissingSetKeyword, ReservedWord,
    TopLevelNonDirective, Unimplemented, Unmatched,
};

/// Produce an AST from source code.
pub fn make_program(source_code: &str) -> LangResult<Program> {
    let tokens = tokenize(source_code)?;
    let directives = AstBuilder::from(&tokens[..]).directives()?;
    Program::try_from(directives)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum OpPrecedence {
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

#[derive(Debug, Copy, Clone)]
pub struct AstBuilder<'a> {
    /// Tokens to feed.
    tokens: &'a [Token<'a>],
    /// Index of the "current" token (None = before start).
    cursor: Option<usize>,
}
impl<'a> From<&'a [Token<'a>]> for AstBuilder<'a> {
    fn from(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            cursor: None,
        }
    }
}
impl<'a> AstBuilder<'a> {
    /// Moves the cursor forward and then returns the element at the cursor.
    pub fn next(&mut self) -> Option<Token<'a>> {
        // Add 1 or set to zero.
        self.cursor = Some(self.cursor.map(|idx| idx + 1).unwrap_or(0));
        self.current()
    }
    /// Moves the cursor back and then returns the element at the cursor.
    pub fn prev(&mut self) -> Option<Token<'a>> {
        // Subtract 1 if possible.
        self.cursor = self.cursor.and_then(|idx| idx.checked_sub(1));
        self.current()
    }
    /// Returns the element at the cursor.
    pub fn current(&self) -> Option<Token<'a>> {
        self.cursor.and_then(|idx| self.tokens.get(idx).copied())
    }
    /// Returns the element after the one at the cursor, without mutably moving
    /// the cursor.
    pub fn peek_next(mut self) -> Option<Token<'a>> {
        self.next()
    }
    /// Returns the element before the one at the cursor, without mutably moving
    /// the cursor.
    pub fn peek_prev(mut self) -> Option<Token<'a>> {
        self.prev()
    }

    /// Returns the span of the current token. If there is no current token,
    /// returns an empty span at the begining or end of the input appropriately.
    fn get_span(&self) -> Span {
        if self.cursor.is_none() {
            Span::default()
        } else {
            match self.current() {
                Some(t) => t.span,
                None => self
                    .tokens
                    .last()
                    .map(|t| Span {
                        start: t.span.end,
                        end: t.span.end,
                    })
                    .unwrap_or_default(),
            }
        }
    }

    /// Returns a LangResult::Err with the span of the current token.
    fn err<T>(&self, msg: LangErrorMsg) -> LangResult<T> {
        Err(msg.with_span(self.get_span()))
    }
    /// Consumes the next symbol and return return a Spanned { ... } of the
    /// result of the given closure if the closure returns LangResult::Ok;
    /// otherwise rewind the state of the AstBuilder to before the closure was
    /// run and then return the LangResult::Err.
    fn expect<T>(&mut self, f: impl Fn(&mut Self) -> LangResult<T>) -> LangResult<Spanned<T>> {
        let prior_state = *self;
        let first_token = self.peek_next();
        let ret = f(self);
        let start = first_token.unwrap().span.start;
        let end = self.get_span().end;
        if ret.is_err() {
            *self = prior_state;
        }
        ret.map(|t| Spanned {
            span: Span { start, end },
            inner: t,
        })
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
    fn directives(&mut self) -> LangResult<Vec<Spanned<Directive>>> {
        let mut directives = vec![];
        while self.peek_next().is_some() {
            directives.push(self.expect(Self::directive)?);
        }
        Ok(directives)
    }
    /// Consumes a directive, which includes one or more arguments given to the
    /// directive. The number of arguments consumed depends on the name of the
    /// directive and the type of arguments passed.
    fn directive(&mut self) -> LangResult<Directive> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Directive(directive_name)) => {
                match directive_name.to_ascii_lowercase().as_ref() {
                    "transition" => Ok(Directive::Transition(self.expect(Self::block)?.inner)),
                    _ => self.err(InvalidDirectiveName),
                }
            }
            Some(_) => self.err(TopLevelNonDirective),
            None => self.err(Expected("directive")),
        }
    }
    /// Consumes a block, which consists of statements
    fn block(&mut self) -> LangResult<StatementBlock> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::LBrace)) => (),
            _ => self.err(Expected("code block beginning with '{'"))?,
        }
        let open_span = self.get_span();
        let mut statements = vec![];
        loop {
            match self.next().map(|t| t.class) {
                Some(TokenClass::StatementKeyword(_)) => {
                    self.prev();
                    statements.push(self.expect(Self::statement)?)
                }
                Some(TokenClass::Punctuation(PunctuationToken::RBrace)) => {
                    break;
                }
                Some(_) => self.err(Expected("statement or '}'"))?,
                None => Err(Unmatched('{', '}').with_span(open_span))?,
            }
        }
        Ok(statements)
    }
    /// Consumes a statement.
    fn statement(&mut self) -> LangResult<Statement> {
        use StatementKeywordToken::*;
        match self.next().map(|t| t.class) {
            Some(TokenClass::StatementKeyword(kw)) => match kw {
                Become => Ok(Statement::Become(self.expect(Self::expression)?)),
                Break => self.err(Unimplemented),
                Case => self.err(Unimplemented),
                Continue => self.err(Unimplemented),
                Else => self.err(ElseWithoutIf),
                For => self.err(Unimplemented),
                If => Ok(Statement::If {
                    cond_expr: self.expect(Self::expression)?,
                    if_true: self.expect(Self::block)?.inner,
                    if_false: if self.next_token_is_one_of(&[TokenClass::StatementKeyword(Else)]) {
                        self.next();
                        if self.next_token_is_one_of(&[TokenClass::StatementKeyword(If)]) {
                            // Else if
                            vec![self.expect(Self::statement)?]
                        } else {
                            // Else
                            self.expect(Self::block)?.inner
                        }
                    } else {
                        vec![]
                    },
                }),
                Remain => self.err(Unimplemented),
                Return => self.err(Unimplemented),
                Set => Ok({
                    let var = self.expect(Self::var)?;
                    let assign_op = self.expect(Self::assign_op)?;
                    let expr = self.expect(Self::expression)?;
                    Statement::SetVar {
                        var_expr: var.clone(),
                        value_expr: if let Some(op) = assign_op.inner {
                            Spanned {
                                span: assign_op.span,
                                inner: Expr::Op(Box::new(var), op, Box::new(expr)),
                            }
                        } else {
                            expr
                        },
                    }
                }),
                Unless => self.err(Unimplemented),
                While => self.err(Unimplemented),
            },
            _ => {
                if let Some(TokenClass::Assignment(_)) = self.peek_next().map(|t| t.class) {
                    self.err(MissingSetKeyword)
                } else {
                    self.err(Expected("statement"))
                }
            }
        }
    }
    /// Consumes a nested expression.
    fn expression(&mut self) -> LangResult<Expr> {
        self.expression_with_precedence(OpPrecedence::lowest())
            .map(|spanned| spanned.inner)
    }
    /// Consumes a nested expression of the given precedence level and higher
    /// recursively using precedence climbing.
    fn expression_with_precedence(
        &mut self,
        precedence: OpPrecedence,
    ) -> LangResult<Spanned<Expr>> {
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
                    self.expect(|tf| tf.paren(Self::expression))
                }
                Some(TokenClass::Integer(_)) => self.expect(Self::int),
                Some(TokenClass::String { .. }) => self.err(Unimplemented),
                Some(TokenClass::Tag(_)) => self.err(Unimplemented),
                Some(TokenClass::Ident(_)) => self.expect(Self::var),
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
        while self.next_token_is_one_of(token_classes) {
            op_tokens.push(self.next().unwrap());
        }
        let mut ret = self.expression_with_precedence(precedence.next())?;
        // Read operators from right to left so that the leftmost is the
        // outermost.
        for op_token in op_tokens.iter().rev() {
            let operand = Box::new(ret);
            ret = Spanned {
                span: Span::merge(op_token, &*operand),
                inner: match op_token.class {
                    TokenClass::Operator(OperatorToken::Minus) => Expr::Neg(operand),
                    TokenClass::Operator(OperatorToken::Tag) => Expr::Tag(operand),
                    other => panic!("Invalid unary operator: {:?}", other),
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
        let initial = self.expression_with_precedence(precedence.next())?;
        let mut op_tokens_and_exprs = vec![];
        while self.next_token_is_one_of(token_classes) {
            op_tokens_and_exprs.push((
                self.next().unwrap(),
                self.expression_with_precedence(precedence.next())?,
            ));
        }
        let mut ret = initial;
        // Read operations from left to right so that the rightmost is the
        // outermost.
        for (op_token, rhs) in op_tokens_and_exprs {
            let lhs = Box::new(ret);
            let rhs = Box::new(rhs);
            let op = match op_token.class {
                TokenClass::Operator(OperatorToken::Plus) => Op::Add,
                TokenClass::Operator(OperatorToken::Minus) => Op::Sub,
                TokenClass::Operator(OperatorToken::Asterisk) => Op::Mul,
                TokenClass::Operator(OperatorToken::Slash) => Op::Div,
                TokenClass::Operator(OperatorToken::Percent) => Op::Rem,
                other => panic!("Invalid binary operator: {:?}", other),
            };
            ret = Spanned {
                span: Span::merge(&*lhs, &*rhs),
                inner: Expr::Op(lhs, op, rhs),
            };
        }
        Ok(ret)
    }
    /// Consumes an expression consisting of any number of chained comparison
    /// operators.
    fn comparison_op(&mut self, precedence: OpPrecedence) -> LangResult<Spanned<Expr>> {
        let initial = self.expression_with_precedence(precedence.next())?;
        let mut comparisons = vec![];
        while let Some(Token {
            class: TokenClass::Comparison(cmp_type),
            ..
        }) = self.peek_next()
        {
            self.next();
            comparisons.push((
                Cmp::from(cmp_type),
                self.expression_with_precedence(precedence.next())?,
            ));
        }

        if comparisons.is_empty() {
            // No comparison to do -- just return the value.
            return Ok(initial);
        }
        Ok(Spanned {
            span: Span::merge(&initial, &comparisons.last().unwrap().1),
            inner: Expr::Cmp(Box::new(initial), comparisons),
        })
    }
    /// Consumes an integer literal.
    fn int(&mut self) -> LangResult<Expr> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Integer(i)) => Ok(Expr::Int(i)),
            _ => self.err(Expected("integer")),
        }
    }
    /// Consumes a variable name.
    fn var(&mut self) -> LangResult<Expr> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Ident(s)) => Ok(Expr::Var(s.to_owned())),
            Some(TokenClass::Keyword(kw)) => self.err(ReservedWord(kw.to_string().into())),
            Some(TokenClass::StatementKeyword(kw)) => self.err(ReservedWord(kw.to_string().into())),
            _ => self.err(Expected("variable name")),
        }
    }
    // Consumes an assignment token.
    fn assign_op(&mut self) -> LangResult<Option<Op>> {
        use AssignmentToken::*;
        match self.next().map(|t| t.class) {
            Some(TokenClass::Assignment(assign_type)) => Ok(match assign_type {
                // TODO implement the remaining operators.
                Assign => None,
                AddAssign => Some(Op::Add),
                DivAssign => Some(Op::Div),
                // ExpAssign => Some(Op::Exp),
                MulAssign => Some(Op::Mul),
                RemAssign => Some(Op::Rem),
                // ShlAssign => Some(Op::Shl),
                // ShrAssign => Some(Op::Shr),
                SubAssign => Some(Op::Sub),
                _ => self.err(Unimplemented)?,
            }),
            _ => self.err(Expected("assignment symbol, e.g. '='")),
        }
    }
    /// Consumes a pair of parentheses with the given matcher run inside.
    fn paren<T>(&mut self, inner_matcher: impl Fn(&mut Self) -> LangResult<T>) -> LangResult<T> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::LParen)) => (),
            _ => self.err(Expected("parenthetical expression beginning with '('"))?,
        }
        let open_span = self.get_span();
        let expr = self.expect(inner_matcher)?.inner;
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::RParen)) => Ok(expr),
            Some(_) => self.err(Expected("')'")),
            None => Err(Unmatched('(', ')').with_span(open_span)),
        }
    }
}
