use super::*;

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
pub struct TokenFeeder<'a> {
    /// Tokens to feed.
    tokens: &'a [Token<'a>],
    /// Index of the "current" token (None = before start).
    cursor: Option<usize>,
}
impl<'a> From<&'a [Token<'a>]> for TokenFeeder<'a> {
    fn from(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            cursor: None,
        }
    }
}
impl<'a> TokenFeeder<'a> {
    /// Moves the cursor forward and then returns the element at the cursor.
    pub fn next(&mut self) -> Option<Token<'a>> {
        self.skip();
        self.current()
    }
    /// Moves the cursor back and then returns the element at the cursor.
    pub fn prev(&mut self) -> Option<Token<'a>> {
        self.skip_back();
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
    /// Moves the cursor forward without reading a token.
    pub fn skip(&mut self) {
        // Add 1 or set to zero.
        self.cursor = Some(self.cursor.map(|idx| idx + 1).unwrap_or(0));
    }
    /// Moves the cursor backward without reading a token.
    pub fn skip_back(&mut self) {
        // Subtract 1 if possible.
        self.cursor = self.cursor.and_then(|idx| idx.checked_sub(1));
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
    /// Returns a tuple with the span of the most recently returned token that
    /// can be used to make a LangResult::Err.
    fn error(&self, msg: &'static str) -> LangError {
        lang_error(self.get_span(), msg)
    }
    fn err<T>(&self, msg: &'static str) -> LangResult<T> {
        lang_err(self.get_span(), msg)
    }
    fn expect<T, F: Fn(&mut Self) -> LangResult<T>>(&mut self, f: F) -> LangResult<Spanned<T>> {
        let start = self.get_span().start;
        let ret = f(self);
        let end = self.get_span().end;
        ret.map(|t| Spanned {
            span: Span { start, end },
            inner: t,
        })
    }
    fn accept<T, F: Fn(&mut Self) -> LangResult<T>>(&mut self, f: F) -> Option<Spanned<T>> {
        let prior_state = *self;
        match self.expect(f) {
            Ok(ret) => Some(ret),
            Err(_) => {
                *self = prior_state;
                None
            }
        }
    }
    fn accept_any<T>(
        &mut self,
        fs: &[fn(&mut Self) -> LangResult<T>],
        error_msg: &'static str,
    ) -> LangResult<Spanned<T>> {
        for f in fs {
            if let Some(ret) = self.accept(f) {
                return Ok(ret);
            }
        }
        self.next();
        self.err(error_msg)
    }
    fn unimplemented<T>(self) -> LangResult<T> {
        self.err("Unimplemented")
    }
    fn token_is_one_of(token: Option<Token<'a>>, token_classes: &[TokenClass]) -> bool {
        if let Some(t) = token {
            token_classes.contains(&t.class)
        } else {
            false
        }
    }
    pub fn program(&mut self) -> LangResult<Vec<Spanned<Directive>>> {
        let mut directives = vec![];
        while self.peek_next().is_some() {
            directives.push(self.expect(Self::directive)?);
        }
        Ok(directives)
    }
    fn directive(&mut self) -> LangResult<Directive> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Directive(directive_name)) => {
                match directive_name.to_ascii_lowercase().as_ref() {
                    "transition" => Ok(Directive::Transition(self.expect(Self::block)?.inner)),
                    _ => self.err(
                        "Invalid directive name; valid directives are @ndca, @states, @transition",
                    ),
                }
            }
            Some(_) => self
                .err("Only directives, which begin with @, may appear at the top level of a file"),
            None => self.err("Expected directive"),
        }
    }
    fn block(&mut self) -> LangResult<Vec<Spanned<Statement>>> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::LBrace)) => (),
            _ => self.err("Expected code block beginning with '{'")?,
        }
        let open_span = self.current().unwrap().span;
        let mut statements = vec![];
        loop {
            match self.peek_next().map(|t| t.class) {
                Some(TokenClass::StatementKeyword(_)) => {
                    statements.push(self.expect(Self::statement)?)
                }
                Some(TokenClass::Punctuation(PunctuationToken::RBrace)) => {
                    self.next();
                    break;
                }
                Some(_) => self.err("Expected statement or '}'")?,
                None => lang_err(open_span, "This '{' has no matching '}'")?,
            }
        }
        Ok(statements)
    }
    fn statement(&mut self) -> LangResult<Statement> {
        use StatementKeywordToken::*;
        match self.next().map(|t| t.class) {
            Some(TokenClass::StatementKeyword(kw)) => match kw {
                Become => Ok(Statement::Become(self.expect(Self::expression)?)),
                Break => unimplemented!(),
                Case => unimplemented!(),
                Continue => unimplemented!(),
                Else => unimplemented!(),
                For => unimplemented!(),
                If => unimplemented!(),
                Remain => unimplemented!(),
                Return => unimplemented!(),
                Set => unimplemented!(),
                Unless => unimplemented!(),
                While => unimplemented!(),
            },
            _ => self.err("Expected statement"),
        }
    }
    fn expression(&mut self) -> LangResult<Expr> {
        self.expression_at_precedence(OpPrecedence::lowest())
            .map(|spanned| spanned.inner)
    }
    fn expression_at_precedence(&mut self, precedence: OpPrecedence) -> LangResult<Spanned<Expr>> {
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
            // TODO add remaining precedence levels
            OpPrecedence::Atom => match self.peek_next().map(|t| t.class) {
                Some(TokenClass::Punctuation(PunctuationToken::LParen)) => self.expect(Self::paren),
                Some(TokenClass::Integer(_)) => self.expect(Self::int),
                Some(TokenClass::String { .. }) => self.unimplemented(),
                Some(TokenClass::Tag(_)) => self.unimplemented(),
                Some(TokenClass::Ident(_)) => self.expect(Self::var),
                _ => {
                    self.next();
                    self.err("Expected expression")
                }
            },
            _ => self.expression_at_precedence(precedence.next()),
        }
    }
    fn unary_op(
        &mut self,
        token_classes: &[TokenClass],
        precedence: OpPrecedence,
    ) -> LangResult<Spanned<Expr>> {
        let mut op_tokens = vec![];
        while Self::token_is_one_of(self.peek_next(), token_classes) {
            op_tokens.push(self.next().unwrap());
        }
        let mut ret = self.expression_at_precedence(precedence.next())?;
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
    fn left_binary_op(
        &mut self,
        token_classes: &[TokenClass],
        precedence: OpPrecedence,
    ) -> LangResult<Spanned<Expr>> {
        let initial = self.expression_at_precedence(precedence.next())?;
        let mut op_tokens_and_exprs = vec![];
        while Self::token_is_one_of(self.peek_next(), token_classes) {
            op_tokens_and_exprs.push((
                self.next().unwrap(),
                self.expression_at_precedence(precedence.next())?,
            ));
        }
        let mut ret = initial;
        // Read operations from left to right so that the rightmost is the
        // outermost.
        for (op_token, rhs) in op_tokens_and_exprs {
            let lhs = Box::new(ret);
            let rhs = Box::new(rhs);
            ret = Spanned {
                span: Span::merge(&*lhs, &*rhs),
                inner: match op_token.class {
                    TokenClass::Operator(OperatorToken::Plus) => Expr::Add(lhs, rhs),
                    TokenClass::Operator(OperatorToken::Minus) => Expr::Sub(lhs, rhs),
                    other => panic!("Invalid binary operator: {:?}", other),
                },
            };
        }
        Ok(ret)
    }
    fn int(&mut self) -> LangResult<Expr> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Integer(i)) => Ok(Expr::Int(i)),
            _ => self.err("Expected integer"),
        }
    }
    fn var(&mut self) -> LangResult<Expr> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Ident(s)) => Ok(Expr::Var(s.to_owned())),
            _ => self.err("Expected string"),
        }
    }
    fn paren(&mut self) -> LangResult<Expr> {
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::LParen)) => (),
            _ => self.err("Expected parenthetical expression beginning with '('")?,
        }
        let open_span = self.current().unwrap().span;
        let expr = self.expect(Self::expression)?.inner;
        match self.next().map(|t| t.class) {
            Some(TokenClass::Punctuation(PunctuationToken::RParen)) => Ok(expr),
            Some(_) => self.err("Expected ')'"),
            None => lang_err(open_span, "This '(' has no matching ')'"),
        }
    }
}

pub type Block = Vec<Spanned<Statement>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    // SetVar(Spanned<Var>, Spanned<Expr>),
    // If(
    //     // If
    //     Spanned<Expr>,
    //     Statements,
    //     // Elseif
    //     Vec<(Spanned<Expr>, Statements)>,
    //     // Else
    //     Option<Statements>,
    // ),
    // ForLoop(Spanned<Var>, Spanned<Expr>, Statements),
    // WhileLoop(Spanned<Expr>, Statements),
    // DoWhileLoop(Statements, Spanned<Expr>),
    // Break,
    // Continue,
    // Remain,
    Become(Spanned<Expr>),
    // Return(Spanned<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Transition(Vec<Spanned<Statement>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Int(i64),
    Tag(Box<Spanned<Expr>>),
    Neg(Box<Spanned<Expr>>),
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Var(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[allow(irrefutable_let_patterns)]
    fn test_ast() {
        let source_code = "
// @states [ #dead, #live ]
@transition {
    become #(1 - (2 + -3) + 12)
}
";
        let tokens = tokenizer::tokenize(source_code).expect("Tokenization failed");
        let ast = TokenFeeder::from(&tokens[..])
            .program()
            .expect("AST generation failed");
        let mut correct = false;
        if let Directive::Transition(ast) = &ast[0].inner {
            if let Statement::Become(ast) = &ast[0].inner {
                if let Expr::Tag(ast) = &ast.inner {
                    if let Expr::Add(lhs, rhs) = &ast.inner {
                        if let (Expr::Sub(lhs, rhs), Expr::Int(12)) = (&lhs.inner, &rhs.inner) {
                            if let (Expr::Int(1), Expr::Add(lhs, rhs)) = (&lhs.inner, &rhs.inner) {
                                if let (Expr::Int(2), Expr::Neg(ast)) = (&lhs.inner, &rhs.inner) {
                                    if let Expr::Int(3) = &ast.inner {
                                        correct = true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if !correct {
            println!("Tokens:");
            println!("{:?}", tokens);
            println!();
            println!("AST:");
            println!("{:?}", ast);
            println!();
            panic!("AST is incorrect");
        }
    }
}
