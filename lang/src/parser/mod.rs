//! Functions for producing a parse tree.
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

mod tree;

pub use tree::*;

use crate::errors::*;
use crate::lexer::*;
use crate::types::LangInt;
use crate::{Span, Spanned};
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
            Self::Exp => Self::ArrayIndex,
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
    /// Consumes the next symbol and returns a Spanned { ... } of the result of
    /// the given closure if the closure returns LangResult::Ok; otherwise
    /// rewind the state of the ParseBuilder to before the closure was run and
    /// then return the LangResult::Err.
    fn expect<T>(&mut self, f: impl FnOnce(&mut Self) -> LangResult<T>) -> LangResult<Spanned<T>> {
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
    /// Executes the given closure, reverting the state of the ParseBuilder if
    /// it returns LangResult::Err.
    fn expect_spanned<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> LangResult<Spanned<T>>,
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
            directives.push(self.expect(Self::directive)?.inner);
        }
        Ok(directives)
    }
    /// Consumes a directive, which includes one or more arguments given to the
    /// directive. The number of arguments consumed depends on the name of the
    /// directive and the type of arguments passed.
    fn directive(&mut self) -> LangResult<(Directive, Spanned<DirectiveContents>)> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Directive(directive_name)) => {
                let directive = Directive::try_from(directive_name)
                    .map_err(|_| InvalidDirectiveName.with_span(self.span()))?;
                let contents = match directive {
                    Directive::Function => self.expect(Self::function_definition)?,
                    _ => self.expect(Self::simple_directive_contents)?,
                };
                Ok((directive, contents))
            }
            Some(_) => self.err(TopLevelNonDirective),
            None => self.err(Expected("directive")),
        }
    }
    /// Consumes a function definition.
    fn function_definition(&mut self) -> LangResult<DirectiveContents> {
        Ok(DirectiveContents::Func(HelperFunc {
            return_type: self.expect(Self::type_name)?,
            name: self.expect(Self::ident)?,
            args: self
                .expect(|pb| {
                    pb.pair("(", ")", |pb| {
                        pb.list(
                            &[TokenClass::Punctuation(PunctuationToken::Comma)],
                            &[TokenClass::Punctuation(PunctuationToken::RParen)],
                            Self::function_param,
                            "function parameter",
                        )
                    })
                })?
                .inner
                .inner,
            body: self.expect(Self::block)?,
        }))
    }
    /// Consumes a parmeter definition, consisting of a type followed by an
    /// identifier.
    fn function_param(&mut self) -> LangResult<(Spanned<TypeToken>, Spanned<String>)> {
        let ty = self.expect(Self::type_name)?;
        let name = self.expect(Self::ident)?;
        Ok((ty, name))
    }
    /// Consumes the contents of a (syntactically) simple directive, either an
    /// expression or code block.
    fn simple_directive_contents(&mut self) -> LangResult<DirectiveContents> {
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
                Some(_) => {
                    if let Some(TokenClass::Assignment(_)) = self.peek_next().map(|t| t.class) {
                        self.err(MissingSetKeyword)?
                    } else {
                        self.err(Expected("statement or '}'"))?
                    }
                }
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
                Assert => Ok(Statement::Assert {
                    expr: self.expect(Self::expression)?,
                    msg: {
                        if self.next_token_is_one_of(&[TokenClass::Punctuation(
                            PunctuationToken::Comma,
                        )]) {
                            self.next();
                            Some(self.expect(Self::string)?)
                        } else {
                            None
                        }
                    },
                }),
                Become => Ok(Statement::Become(self.expect(Self::expression)?)),
                Break => self.err(Unimplemented),
                Case => self.err(Unimplemented),
                Continue => self.err(Unimplemented),
                Else => self.err(ElseWithoutIf),
                Error => Ok(Statement::Error {
                    msg: {
                        match self.peek_next().map(|t| t.class) {
                            Some(TokenClass::String { .. }) => Some(self.expect(Self::string)?),
                            _ => None,
                        }
                    },
                }),
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
                Return => Ok(Statement::Return(self.expect(Self::expression)?)),
                Set => Ok({
                    // Get the variable name.
                    let var_name = self.expect(Self::ident)?;
                    // Get the operator to use when assigning (if any). E.g.
                    // `+=` uses the `+` operator.
                    let assign_op = self.expect(Self::assign_op)?.inner;
                    // Get the expression to assign into the variable.
                    let value_expr = self.expect(Self::expression)?;
                    // Construct the statement.
                    Statement::SetVar {
                        var_name,
                        assign_op,
                        value_expr,
                    }
                }),
                Unless => self.err(Unimplemented),
                While => self.err(Unimplemented),
                _ => self.err(Expected("statement")),
            },
            _ => self.err(Expected("statement")),
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
            OpPrecedence::Comparison => self.comparison_op(precedence),
            OpPrecedence::BitwiseOr => {
                self.left_binary_op(&[TokenClass::Operator(OperatorToken::Pipe)], precedence)
            }
            OpPrecedence::BitwiseXor => {
                self.left_binary_op(&[TokenClass::Operator(OperatorToken::Caret)], precedence)
            }
            OpPrecedence::BitwiseAnd => self.left_binary_op(
                &[TokenClass::Operator(OperatorToken::Ampersand)],
                precedence,
            ),
            OpPrecedence::Bitshift => self.left_binary_op(
                &[
                    TokenClass::Operator(OperatorToken::DoubleLessThan),
                    TokenClass::Operator(OperatorToken::DoubleGreaterThan),
                    TokenClass::Operator(OperatorToken::TripleGreaterThan),
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
            OpPrecedence::UnaryPrefix => self.unary_op(
                &[
                    TokenClass::Operator(OperatorToken::Tag),
                    TokenClass::Operator(OperatorToken::Minus),
                ],
                precedence,
            ),
            // TODO add remaining precedence levels
            OpPrecedence::FunctionCall => {
                // Function calls include attribute/method access.
                let mut ret = self.expression_with_precedence(precedence.next())?;
                const PERIOD_TOKEN: TokenClass = TokenClass::Punctuation(PunctuationToken::Period);
                const LPAREN_TOKEN: TokenClass = TokenClass::Punctuation(PunctuationToken::LParen);
                while self.next_token_is_one_of(&[PERIOD_TOKEN, LPAREN_TOKEN]) {
                    let mut args = vec![];
                    let mut is_method = false;
                    let mut span = ret.span;
                    // Handle `.function_name()`.
                    if self.next_token_is_one_of(&[PERIOD_TOKEN]) {
                        is_method = true;
                        self.next();
                        args.push(ret);
                        ret = self.expect(Self::ident)?.map(Expr::Ident);
                        span = Span::merge(args[0].span, ret.span);
                    }
                    // Handle `(arg1, arg2)`.
                    if self.next_token_is_one_of(&[LPAREN_TOKEN]) {
                        let args_list = self.expect(|pb| {
                            Ok(pb
                                .pair("(", ")", |pb| {
                                    pb.list(
                                        &[TokenClass::Punctuation(PunctuationToken::Comma)],
                                        &[TokenClass::Punctuation(PunctuationToken::RParen)],
                                        Self::expression,
                                        "expression",
                                    )
                                })?
                                .inner)
                        })?;
                        args.extend(args_list.inner);
                        span = Span::merge(span, args_list.span);
                    }
                    ret = Spanned {
                        span,
                        inner: Expr::FnCall {
                            is_method,
                            func: Box::new(ret),
                            args,
                        },
                    }
                }
                Ok(ret)
            }
            OpPrecedence::Atom => match self.peek_next().map(|t| t.class) {
                Some(TokenClass::Punctuation(PunctuationToken::LBracket)) => self
                    .expect_spanned(Self::vector)
                    .map(|s| s.map(Expr::Vector)),
                Some(TokenClass::Punctuation(PunctuationToken::LParen)) => {
                    // Expressions inside parentheses always start again at the
                    // lowest precedence level.
                    self.expect_spanned(|pb| pb.pair("(", ")", Self::expression))
                }
                Some(TokenClass::Integer(_)) => self.expect(Self::int).map(|s| s.map(Expr::Int)),
                Some(TokenClass::String { .. }) => self.err(Unimplemented),
                Some(TokenClass::Tag(_)) => self.err(Unimplemented),
                Some(TokenClass::Ident(_)) => self
                    .expect(Self::ident)
                    .map(|spanned| spanned.map(Expr::Ident)),
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
                    TokenClass::Keyword(KeywordToken::Or) => Expr::LogicalOr { lhs, rhs },
                    TokenClass::Keyword(KeywordToken::Xor) => Expr::LogicalXor { lhs, rhs },
                    TokenClass::Keyword(KeywordToken::And) => Expr::LogicalAnd { lhs, rhs },
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
    fn int(&mut self) -> LangResult<LangInt> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Integer(i)) => Ok(i),
            _ => self.err(Expected("integer")),
        }
    }
    /// Consumes a type name.
    fn type_name(&mut self) -> LangResult<TypeToken> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Type(ty)) => Ok(ty),
            _ => self.err(Expected("type name; e.g. int, cellstate, vec, vec1..256")),
        }
    }
    /// Consumes an identifier.
    fn ident(&mut self) -> LangResult<String> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Ident(s)) => Ok(s.to_owned()),
            Some(TokenClass::Keyword(kw)) => self.err(ReservedWord(kw.to_string().into())),
            Some(TokenClass::Type(ty)) => self.err(ReservedWord(ty.to_string().into())),
            _ => self.err(Expected("identifier, i.e. variable or function name")),
        }
    }
    /// Consumes a string literal.
    fn string(&mut self) -> LangResult<StringLiteral> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::String {
                prefix,
                quote,
                contents,
            }) => Ok(StringLiteral {
                prefix,
                quote,
                contents: contents.to_owned(),
            }),
            _ => self.err(Expected("string")),
        }
    }
    fn vector(&mut self) -> LangResult<Spanned<Vec<Spanned<Expr>>>> {
        self.expect_spanned(|pb| {
            pb.pair("[", "]", |pb| {
                pb.list(
                    &[TokenClass::Punctuation(PunctuationToken::Comma)],
                    &[TokenClass::Punctuation(PunctuationToken::RBracket)],
                    Self::expression,
                    "expression",
                )
            })
        })
    }
    /// Consumes an assignment token and returns the operator used in the
    /// assignment, if any. (E.g. `+=` uses the `+` operator, while `=` does not
    /// use any operator.)
    fn assign_op(&mut self) -> LangResult<AssignmentToken> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Assignment(a)) => Ok(a),
            _ => self.err(Expected("assignment symbol, e.g. '=' or '+='")),
        }
    }
    /// Consumes a pair of symbols (parentheses, brackets, etc.) with the given
    /// matcher run inside. TODO: use pair() for statement blocks as well
    fn pair<T>(
        &mut self,
        open: &'static str,
        close: &'static str,
        inner_matcher: impl FnOnce(&mut Self) -> LangResult<T>,
    ) -> LangResult<Spanned<T>> {
        if self.next().map(|t| t.class) != Some(TokenClass::Punctuation(open.parse().unwrap())) {
            self.err(Expected("parenthetical expression beginning with '('"))?;
        }
        // Record the span of the left paren.
        let open_span = self.span();
        let expr = self.expect(inner_matcher)?;
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(punc)) if punc == close.parse().unwrap() => Ok(expr),
            Some(_) => self.err(Expected(close)),
            None => Err(
                // TODO: this is ugly; how to do it better?
                Unmatched(open.chars().next().unwrap(), close.chars().next().unwrap())
                    .with_span(open_span),
            ),
        }
    }
    /// Consumes a list of things (using the given matcher) separated by a given
    /// separator token and ending with any of the given end tokens.
    ///
    /// Does not consume the end token.
    fn list<T>(
        &mut self,
        sep_tokens: &[TokenClass],
        end_tokens: &[TokenClass],
        mut inner_matcher: impl FnMut(&mut Self) -> LangResult<T>,
        expected_msg: &'static str,
    ) -> LangResult<Vec<Spanned<T>>> {
        let mut ret = vec![];
        loop {
            if self.next_token_is_one_of(&end_tokens) {
                return Ok(ret);
            }
            ret.push(self.expect(&mut inner_matcher)?);
            if self.next_token_is_one_of(&end_tokens) {
                return Ok(ret);
            } else if self.next_token_is_one_of(&sep_tokens) {
                self.next();
                continue;
            } else {
                self.next();
                return self.err(Expected(expected_msg));
            }
        }
    }
}
