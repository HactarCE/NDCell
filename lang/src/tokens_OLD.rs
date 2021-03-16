use codemap::{File, Span};
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use std::fmt;
use std::str::FromStr;
use std::sync::Arc;

use crate::data::LangInt;
use crate::errors::SpanConvertExt;

/// A list of token patterns, arranged roughly from least to most general.
const TOKEN_PATTERNS: &'static [&'static str] = &[
    // Line comment.
    r#"//[^\r\n]*"#,
    // Block comment, using the non-greedy `[\s\S]*?` to match the contents of
    // the comment.
    r#"/\*[\s\S]*?\*/"#,
    // Unterminated block comment -- this should raise an error.
    r#"/\*"#,
    // Double-quoted string, with an optional modifier character in front. Note
    // that like Rust strings, this can span multiple lines.
    r#"\w?"[^"]*""#,
    // Single-quoted string, with an optional modifier character in front. Note
    // that like Rust strings, this can span multiple lines.
    r#"\w?'[^']*'"#,
    // Unterminated string -- this should raise an error.
    r#"\w?["']"#,
    // Unsigned binary/hexadecimal/octal integer literal.
    r#"0[box][A-Za-z\d_]*"#,
    // Unsigned decimal literal.
    r#"\d[\d_]*"#,
    // Identifier consisting of a letter or underscore followed by any letters,
    // digits, and/or underscores, with an optional `#` (for tags) or `@` (for
    // directives) in front.
    r#"[A-Za-z_][A-Za-z_\d]*"#,
    // In-place arithmetic operators `**=`, `<<=`, `>>=`, and `>>>=`.
    r#"(\*\*|<<|>>>?)="#,
    // In-place arithmetic operators `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, and `^=`.
    r#"[+\-*/%&|^]="#,
    // Operators `->`, `..`, `**`, `<<`, `>>`, and `>>>`.
    r#"(->|\.\.|\*\*|<<|>>>?)"#,
    // Equality checks `==`, `!=`, `<=`, and `>=`.
    r#"[=!<>]="#,
    // Any other single character from the letter, numeral, punctuation, or
    // symbol Unicode categories.
    r#"[\pL\pN\pP\pS]"#,
];

lazy_static! {
    /// A single regex that matches any token, including comments and strings,
    /// by joining each member of TOKEN_PATTERNS with '|'.
    pub static ref TOKEN_PATTERN: Regex = Regex::new(&TOKEN_PATTERNS.join("|")).unwrap();

    /// A regex that matches any valid identifier (or keyword, but that's fine).
    pub static ref IDENT_PATTERN: Regex = Regex::new(r#"^[A-Za-z_][A-Za-z_\d]*$"#).unwrap();
    /// A regex that matches any valid integer literal (and some invalid ones).
    pub static ref INT_PATTERN: Regex = Regex::new(r#"^0[box][A-Za-z\d_]*|\d[\d_]*$"#).unwrap();
    /// A regex that matches any string literal.
    pub static ref STRING_PATTERN: Regex = Regex::new(r#"^(\w?)(["'])(?:([\s\S]*)["'])?$"#).unwrap();
    /// A regex that matches the beginning of a block comment.
    pub static ref BLOCK_COMMENT_PATTERN: Regex = Regex::new(r#"^/\*"#).unwrap();
    /// A regex that matches the beginning of a line comment.
    pub static ref LINE_COMMENT_PATTERN: Regex = Regex::new(r#"^//"#).unwrap();
    /// A regex that matches an assignment operator.
    pub static ref ASSIGN_PATTERN: Regex = Regex::new(r#"^(.?.?)=$"#).unwrap();

    /// A regex that matches a vector type name.
    pub static ref VEC_TYPE_PATTERN: Regex = Regex::new(r#"Vec(?:tor)?(\d+)"#).unwrap();
    /// A regex that matches a rectangle type name.
    pub static ref RECT_TYPE_PATTERN: Regex = Regex::new(r#"Rect(?:angle)?(\d+)"#).unwrap();
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    /// Kind of token.
    pub kind: Kind,
    /// Span in source code.
    pub span: Span,
    /// File.
    pub file: Arc<File>,
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            Kind::Keyword(t) => write!(f, "keyword '{}'", t),
            Kind::Type(t) => write!(f, "type '{}'", t),
            Kind::Assignment(t) => write!(f, "assignment operator '{}'", t),
            Kind::Comparison(t) => write!(f, "comparison operator '{}'", t),
            Kind::Operator(t) => write!(f, "operator '{}'", t),
            Kind::Punctuation(t) => write!(f, "punctuation '{}'", t),
            Kind::Integer(i) => write!(f, "integer '{}'", i),
            Kind::String { prefix: None, .. } => write!(f, "string"),
            Kind::String {
                prefix: Some(prefix),
                ..
            } => write!(f, "string with prefix {}", prefix),
            Kind::Ident => write!(f, "identifier '{}'", self.as_str()),
            Kind::Unknown => write!(f, "unknown symbol '{}'", self.as_str()),
        }
    }
}
impl SpanConvertExt for Token {
    fn to_span(&self) -> Option<Span> {
        Some(self.span)
    }
}
impl Token {
    /// Returns the source string for this token.
    pub fn as_str(&self) -> &str {
        self.file.source_slice(self.span)
    }
}

/// Kind of token.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Kind {
    /// Keyword.
    Keyword(Keyword),
    /// Type name.
    Type(Type),
    /// Assignment operator.
    Assignment(Assignment),
    /// Comparison operator.
    Comparison(Comparison),
    /// Other operator.
    Operator(Operator),
    /// Miscellaneous punctuation.
    Punctuation(Punctuation),
    /// Integer literal.
    Integer(LangInt),
    /// String literal.
    String {
        /// Optional single-character prefix (like Python's `r"..."` and
        /// `f"..."` strings).
        prefix: Option<char>,
        /// The quote character used (either single quote or double quote).
        quote: char,
        /// The contents of the string.
        contents: Span,
    },
    /// Identifier.
    Ident,
    /// Unknown character.
    Unknown,
}

enum_with_str_repr! {
    /// Keyword.
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum Keyword {
        // Loops
        For = "for",
        While = "while",

        // Loop control
        Break = "break",
        Continue = "continue",

        // Returning values
        Become = "become",
        Remain = "remain",
        Return = "return",

        // Branching
        Case = "case",
        Else = "else",
        If = "if",
        Match = "match",
        Unless = "unless",

        // Debugging
        Assert = "assert",
        Error = "error",

        // Boolean operators
        Or = "or",
        Xor = "xor",
        And = "and",
        Not = "not",

        // Boolean tests
        In = "in",
        Is = "is",

        // Stencil bounds
        Same = "same",
        Where = "where",

        // Unused reserved words
        Bind = "bind",
        Bound = "bound",
        Static = "static",
        With = "with",
    }

    /// Operator.
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum Operator {
        // Arithmetic operators
        Plus = "+",
        Minus = "-",
        Asterisk = "*",
        Slash = "/",
        Percent = "%",
        DoubleAsterisk = "**",

        // Bitshift operators
        DoubleLessThan = "<<",
        DoubleGreaterThan = ">>",
        TripleGreaterThan = ">>>",

        // Bitwise operators
        Ampersand = "&",
        Pipe = "|",
        Caret = "^",
        Tilde = "~",

        // Boolean operators and boolean tests are in KeywordToken.

        // Miscellaneous operators
        Arrow = "->",
        At = "@",
        DotDot = "..",
        Tag = "#",
    }

    /// Punctuation.
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum Punctuation {
        LParen = "(",
        RParen = ")",
        LBracket = "[",
        RBracket = "]",
        LBrace = "{",
        RBrace = "}",
        Comma = ",",
        Colon = ":",
        Semicolon = ";",
        Period = ".",
        Backtick = "`",
    }

    /// Comparison.
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum Comparison {
        /// Equal.
        Eql = "==",
        /// Not equal.
        Neq = "!=",
        /// Less than.
        Lt = "<",
        /// Greater than.
        Gt = ">",
        /// Less than or equal.
        Lte = "<=",
        /// Greater than or equal.
        Gte = ">=",
    }
}

impl From<Keyword> for Kind {
    fn from(kw: Keyword) -> Self {
        Self::Keyword(kw)
    }
}
impl Keyword {
    /// A list of all keywords that signal the beginning of a statement.
    pub const STATEMENT_STARTERS: &'static [Self] = &[
        Self::For,
        Self::While,
        Self::Break,
        Self::Continue,
        Self::Become,
        Self::Remain,
        Self::Return,
        Self::Case,
        Self::Else,
        Self::If,
        Self::Match,
        Self::Unless,
        Self::Assert,
        Self::Error,
    ];
    /// Returns true if this keyword signals the beginning of a statement, or
    /// false otherwise.
    pub fn starts_statement(self) -> bool {
        Self::STATEMENT_STARTERS.contains(&self)
    }
}

impl From<Operator> for Kind {
    fn from(kw: Operator) -> Self {
        Self::Operator(kw)
    }
}

impl From<Punctuation> for Kind {
    fn from(kw: Punctuation) -> Self {
        Self::Punctuation(kw)
    }
}
impl Punctuation {
    /// Returns an end-user-friendly name for this punctuation symbol, making no
    /// distinction between left/right pairs.
    pub fn name(self) -> &'static str {
        match self {
            Self::LParen => "parentheses",
            Self::RParen => "parentheses",
            Self::LBracket => "brackets",
            Self::RBracket => "brackets",
            Self::LBrace => "curly braces",
            Self::RBrace => "curly braces",
            Self::Colon => "colon",
            Self::Comma => "comma",
            Self::Semicolon => "semicolon",
            Self::Period => "period",
            Self::Backtick => "backtick",
        }
    }
}

impl From<Comparison> for Kind {
    fn from(kw: Comparison) -> Self {
        Self::Comparison(kw)
    }
}
impl Comparison {
    /// Returns true if this comparison is based only on equality, or false if
    /// it also requires an ordering.
    pub fn is_eq_only(self) -> bool {
        self == Self::Eql || self == Self::Neq
    }
    /// Returns the predicate used by Inkwell that performs this comparison
    /// operation, given whether the operands are signed or unsigned.
    pub fn inkwell_predicate(self, signed: bool) -> inkwell::IntPredicate {
        use inkwell::IntPredicate::*;
        if signed {
            match self {
                Self::Eql => EQ,
                Self::Neq => NE,
                Self::Lt => SLT,
                Self::Gt => SGT,
                Self::Lte => SLE,
                Self::Gte => SGE,
            }
        } else {
            match self {
                Self::Eql => EQ,
                Self::Neq => NE,
                Self::Lt => ULT,
                Self::Gt => UGT,
                Self::Lte => ULE,
                Self::Gte => UGE,
            }
        }
    }
    /// Evaluates this comparison using the given arguments.
    pub fn eval<T: PartialOrd>(self, lhs: T, rhs: T) -> bool {
        match self {
            Self::Eql => lhs == rhs,
            Self::Neq => lhs != rhs,
            Self::Lt => lhs < rhs,
            Self::Gt => lhs > rhs,
            Self::Lte => lhs <= rhs,
            Self::Gte => lhs >= rhs,
        }
    }
}

enum_with_str_repr! {

    /// Type name.
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum Type {
        Integer = "Integer",
        Cell = "Cell",
        Vector = "Vector",
        Array = "Array",

        IntegerSet = "IntegerSet",
        CellSet = "CellSet",
        VectorSet = "VectorSet",
        Pattern = "Pattern",
    }
}

/// Assignment operator.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Assignment {
    /// Simple `=` assignment.
    Assign,
    /// Assignment with an additional operator; e.g. `+=`.
    OpAssign(Operator),
}
impl fmt::Display for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Assign => write!(f, "="),
            Self::OpAssign(op) => write!(f, "{}=", op),
        }
    }
}
impl FromStr for Assignment {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        // Match the regex `^(.?.?)=$` and extract the first group, `(.?.?)`,
        // which gives the additional operator if any.
        if let Some(op_str) = ASSIGN_PATTERN
            .captures(s)
            .and_then(|captures| captures.get(1))
            .as_ref()
            .map(regex::Match::as_str)
        {
            if op_str.is_empty() {
                Ok(Self::Assign)
            } else if let Ok(op) = op_str.parse() {
                Ok(Self::OpAssign(op))
            } else {
                Err(())
            }
        } else {
            Err(())
        }
    }
}
impl From<Assignment> for Kind {
    fn from(t: Assignment) -> Self {
        Self::Assignment(t)
    }
}
impl Assignment {
    /// Returns the token of the operator, if any, that this assignmeent uses.
    pub fn op(self) -> Option<Operator> {
        match self {
            Self::Assign => None,
            Self::OpAssign(op) => Some(op),
        }
    }
}
