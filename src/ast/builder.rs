use std::convert::TryFrom;

use super::super::errors::*;
use super::super::span::{Span, Spanned};
use super::components::{untyped::*, Cmp, MathOp, Op};
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

/// Operator precedence table.
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

/// Iterator over tokens that produces an untyped AST.
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
            // This is the beginning of the token stream; return an empty span
            // at the beginning of the file.
            Span::default()
        } else {
            match self.current() {
                // This is the middle of the token stream.
                Some(t) => t.span,
                // This is the end of the token stream; return an empty span at
                // the end of the file.
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
        // Get a left brace.
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::LBrace)) => (),
            _ => self.err(Expected("code block beginning with '{'"))?,
        }
        // Record the span of the left brace.
        let open_span = self.get_span();
        // Get statements.
        let mut statements = vec![];
        loop {
            match self.next().map(|t| t.class) {
                // There's the beginning of a statement.
                Some(TokenClass::StatementKeyword(_)) => {
                    self.prev();
                    statements.push(self.expect(Self::statement)?)
                }
                // There's a closing brace.
                Some(TokenClass::Punctuation(PunctuationToken::RBrace)) => {
                    break;
                }
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
                        // There's an "else" clause.
                        self.next();
                        if self.next_token_is_one_of(&[TokenClass::StatementKeyword(If)]) {
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
                    let var = self.expect(Self::var)?;
                    // Get the operator to use when assigning (if any). E.g.
                    // `+=` uses the `+` operator.
                    let assign_op = self.expect(Self::assign_op)?;
                    // Get the expression to assign into the variable.
                    let expr = self.expect(Self::expression)?;
                    // Construct the statement.
                    Statement::SetVar {
                        var_expr: var.clone(),
                        value_expr: if let Some(op) = assign_op.inner {
                            // Expand OpAssigns like `x += y` into `x = x + y`.
                            Spanned {
                                span: assign_op.span,
                                inner: Expr::Op {
                                    lhs: Box::new(var),
                                    op,
                                    rhs: Box::new(expr),
                                },
                            }
                        } else {
                            // Just a normal `=` assignment.
                            expr
                        },
                    }
                }),
                Unless => self.err(Unimplemented),
                While => self.err(Unimplemented),
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
            let op = match op_token.class {
                TokenClass::Operator(OperatorToken::Plus) => Op::Math(MathOp::Add),
                TokenClass::Operator(OperatorToken::Minus) => Op::Math(MathOp::Sub),
                TokenClass::Operator(OperatorToken::Asterisk) => Op::Math(MathOp::Mul),
                TokenClass::Operator(OperatorToken::Slash) => Op::Math(MathOp::Div),
                TokenClass::Operator(OperatorToken::Percent) => Op::Math(MathOp::Rem),
                other => panic!("Invalid binary operator: {:?}", other),
            };
            ret = Spanned {
                span: Span::merge(&*lhs, &*rhs),
                inner: Expr::Op { lhs, op, rhs },
            };
        }
        Ok(ret)
    }
    /// Consumes an expression consisting of any number of chained comparison
    /// operators. This function is similar to left_binary_op().
    fn comparison_op(&mut self, precedence: OpPrecedence) -> LangResult<Spanned<Expr>> {
        // Get the leftmost expression.
        let initial = self.expression_with_precedence(precedence.next())?;
        let mut comparisons = vec![];
        // Alternate between getting a comparison operator and an expression.
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
        // If there are no comparisons happening, just return the expression.
        if comparisons.is_empty() {
            return Ok(initial);
        }
        Ok(Spanned {
            // This comparison spans from the leftmost expression to the
            // rightmost expression.
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
    // Consumes an assignment token and returns the operator used in the
    // assignment, if any. (E.g. `+=` uses the `+` operator, while `=` does not
    // use any operator.)
    fn assign_op(&mut self) -> LangResult<Option<Op>> {
        use AssignmentToken::*;
        match self.next().map(|t| t.class) {
            Some(TokenClass::Assignment(Assign)) => Ok(None),
            Some(TokenClass::Assignment(OpAssign(op))) => Ok(Some(op)),
            _ => self.err(Expected("assignment symbol, e.g. '='")),
        }
    }
    /// Consumes a pair of parentheses with the given matcher run inside.
    fn paren<T>(&mut self, inner_matcher: impl Fn(&mut Self) -> LangResult<T>) -> LangResult<T> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::LParen)) => (),
            _ => self.err(Expected("parenthetical expression beginning with '('"))?,
        }
        // Record the span of the left paren.
        let open_span = self.get_span();
        let expr = self.expect(inner_matcher)?.inner;
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::RParen)) => Ok(expr),
            Some(_) => self.err(Expected("')'")),
            None => Err(Unmatched('(', ')').with_span(open_span)),
        }
    }
}
