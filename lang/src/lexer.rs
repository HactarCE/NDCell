//! Functions for tokenization.

use lazy_static::lazy_static;
use regex::Regex;
use std::convert::TryFrom;
use std::fmt;
use std::str::FromStr;

use super::errors::*;
use super::types::LangInt;
use super::{Span, Type};
use LangErrorMsg::{UnknownSymbol, Unterminated};

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
    // Number with decimal point.
    r#"-?\d?\.\d+"#,
    // Number without decimal point.
    r#"-?\d+"#,
    // Identifier consisting of a letter or underscore followed by any letters,
    // digits, and/or underscores, with an optional `#` (for tags) or `@` (for
    // directives) in front.
    r#"[#@]?[A-Za-z_][A-Za-z_\d]*"#,
    // In-place arithmetic operators `**=`, `<<=`, `>>=`, and `>>>=`.
    r#"(\*\*|<<|>>>?)="#,
    // In-place arithmetic operators `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, and `^=`.
    r#"[+\-*/%&|^]="#,
    // Operators `..`, `**`, `<<`, `>>`, and `>>>`.
    r#"(\.\.|\*\*|<<|>>>?)"#,
    // Equality checks `==`, `!=`, `<=`, and `>=`.
    r#"[=!<>]="#,
    // Any other single character.
    r#"[^\s]"#,
];

lazy_static! {
    /// A single regex that matches any token, including comments and strings,
    /// by joining each member of TOKEN_PATTERNS with '|'.
    static ref TOKEN_PATTERN: Regex = Regex::new(&TOKEN_PATTERNS.join("|")).unwrap();

    /// A regex that matches any valid identifier (or keyword, but that's fine).
    static ref IDENT_PATTERN: Regex = Regex::new(r#"^[A-Za-z_][A-Za-z_\d]*$"#).unwrap();
    /// A regex that matches any named tag.
    static ref TAG_PATTERN: Regex = Regex::new(r#"^#[A-Za-z_][A-Za-z_\d]*$"#).unwrap();
    /// A regex that matches any directive.
    static ref DIRECTIVE_PATTERN: Regex = Regex::new(r#"^@[A-Za-z_][A-Za-z_\d]*$"#).unwrap();
    /// A regex that matches any string literal.
    static ref STRING_PATTERN: Regex = Regex::new(r#"^(\w?)(["'])(?:([\s\S]*)["'])?$"#).unwrap();
    /// A regex that matches the beginning of a block comment.
    static ref BLOCK_COMMENT_PATTERN: Regex = Regex::new(r#"^/\*"#).unwrap();
    /// A regex that matches the beginning of a line comment.
    static ref LINE_COMMENT_PATTERN: Regex = Regex::new(r#"^//"#).unwrap();
    /// A regex that matches an assignment operator.
    static ref ASSIGN_PATTERN: Regex = Regex::new(r#"^(.?.?)=$"#).unwrap();

    /// A regex that matches a vector type name.
    static ref VEC_TYPE_PATTERN: Regex = Regex::new(r#"vec(\d+)"#).unwrap();
}

/// Splits a string into tokens and returns them as a Vec, with all comments
/// removed.
pub fn tokenize<'a>(s: &'a str) -> LangResult<Vec<Token<'a>>> {
    let flat_tokens = TOKEN_PATTERN.find_iter(s).map(|m| {
        // Get the span of this token.
        let span = Span {
            start: m.start(),
            end: m.end(),
        };
        // Classify this token.
        let string = m.as_str();
        match TokenClass::try_from(string) {
            Ok(class) => Ok(Token {
                span,
                string,
                class,
            }),
            Err(msg) => Err(msg.with_span(span)),
        }
    });
    flat_tokens
        // Remove comments.
        .filter(|t| !t.as_ref().map_or(false, Token::is_comment))
        // Make a Vec<_>.
        .collect()
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    /// The span of text in the source code where this token occurs.
    pub span: Span,
    /// A reference to the substring of the source code where this token occurs.
    pub string: &'a str,
    /// A classification of this token.
    pub class: TokenClass<'a>,
}
impl<'a> Token<'a> {
    /// Returns whether this token is a comment.
    pub fn is_comment(&self) -> bool {
        self.class == TokenClass::Comment
    }
}
impl<'a> Into<Span> for Token<'a> {
    fn into(self) -> Span {
        self.span
    }
}
impl<'a> Into<Span> for &Token<'a> {
    fn into(self) -> Span {
        self.span
    }
}

/// Classification of a token.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenClass<'a> {
    /// Keyword.
    Keyword(KeywordToken),
    /// Type name.
    Type(TypeToken),
    /// Assignment operator.
    Assignment(AssignmentToken),
    /// Comparison operator.
    Comparison(ComparisonToken),
    /// Other operator.
    Operator(OperatorToken),
    /// Miscellaneous punctuation.
    Punctuation(PunctuationToken),
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
        contents: &'a str,
    },
    /// Directive (not including initial '@').
    Directive(&'a str),
    /// Named tag.
    Tag(&'a str),
    /// Identifier.
    Ident(&'a str),
    /// Comment.
    Comment,
}
impl<'a> fmt::Display for TokenClass<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Keyword(t) => write!(f, "keyword '{}'", t),
            Self::Type(t) => write!(f, "type '{}'", t),
            Self::Assignment(t) => write!(f, "assignment operator '{}'", t),
            Self::Comparison(t) => write!(f, "comparison operator '{}'", t),
            Self::Operator(t) => write!(f, "operator '{}'", t),
            Self::Punctuation(t) => write!(f, "punctuation '{}'", t),
            Self::Integer(i) => write!(f, "integer '{}'", i),
            Self::String { prefix: None, .. } => write!(f, "string"),
            Self::String {
                prefix: Some(prefix),
                ..
            } => write!(f, "string with prefix {}", prefix),
            Self::Directive(s) => write!(f, "directive '@{}'", s),
            Self::Tag(s) => write!(f, "tag '#{}'", s),
            Self::Ident(s) => write!(f, "identifier '{}'", s),
            Self::Comment => write!(f, "comment"),
        }
    }
}
// Implement TryFrom instead of FromStr because we need the lifetime of the &str
// to determine the lifetime of the TokenClass (since some TokenClasses keep a
// &str).
impl<'a> TryFrom<&'a str> for TokenClass<'a> {
    type Error = LangErrorMsg;
    fn try_from(s: &'a str) -> Result<Self, LangErrorMsg> {
        if let Ok(keyword) = s.parse() {
            Ok(Self::Keyword(keyword))
        } else if let Ok(ty) = s.parse() {
            Ok(Self::Type(ty))
        } else if let Ok(assignment) = s.parse() {
            Ok(Self::Assignment(assignment))
        } else if let Ok(comparison) = s.parse() {
            Ok(Self::Comparison(comparison))
        } else if let Ok(operator) = s.parse() {
            Ok(Self::Operator(operator))
        } else if let Ok(punctuation) = s.parse() {
            Ok(Self::Punctuation(punctuation))
        } else if let Ok(i) = s.parse() {
            Ok(Self::Integer(i))
        } else if let Some(captures) = STRING_PATTERN.captures(s) {
            if let Some(contents_capture) = captures.get(3) {
                let prefix = captures.get(1).unwrap().as_str().chars().next();
                let quote = captures.get(2).unwrap().as_str().chars().next().unwrap();
                let contents = contents_capture.as_str();
                Ok(Self::String {
                    prefix,
                    quote,
                    contents,
                })
            } else {
                Err(Unterminated("string"))
            }
        } else if DIRECTIVE_PATTERN.is_match(s) {
            Ok(Self::Directive(&s[1..]))
        } else if TAG_PATTERN.is_match(s) {
            Ok(Self::Tag(&s[1..]))
        } else if IDENT_PATTERN.is_match(s) {
            Ok(Self::Ident(s))
        } else if LINE_COMMENT_PATTERN.is_match(s) {
            Ok(Self::Comment)
        } else if BLOCK_COMMENT_PATTERN.is_match(s) {
            if s == "/*" {
                Err(Unterminated("block comment"))
            } else {
                Ok(Self::Comment)
            }
        } else {
            Err(UnknownSymbol)
        }
    }
}

enum_with_str_repr! {
    /// Keyword.
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum KeywordToken {
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
        Unless = "unless",

        // Variables
        Set = "set",

        // Boolean operators
        Or = "or",
        Xor = "xor",
        And = "and",

        // Boolean tests
        In = "in",
        Is = "is",

        // Unused reserved words
        Bind = "bind",
        Bound = "bound",
        Static = "static",
        Where = "where",
        With = "with",
    }

    /// Operator.
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum OperatorToken {
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

        // Boolean operators and boolean tests are in KeywordToken.

        // Miscellaneous operators
        Dot = ".",
        DotDot = "..",
        Tag = "#",
    }

    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum PunctuationToken {
        LParen = "(",
        RParen = ")",
        LBracket = "[",
        RBracket = "]",
        LBrace = "{",
        RBrace = "}",
        Comma = ",",
        Semicolon = ";",
    }

    /// Comparison.
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum ComparisonToken {
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

impl KeywordToken {
    pub fn starts_statement(self) -> bool {
        match self {
            Self::For
            | Self::While
            | Self::Break
            | Self::Continue
            | Self::Become
            | Self::Remain
            | Self::Return
            | Self::Case
            | Self::Else
            | Self::If
            | Self::Unless
            | Self::Set => true,
            Self::Or
            | Self::Xor
            | Self::And
            | Self::In
            | Self::Is
            | Self::Bind
            | Self::Bound
            | Self::Static
            | Self::Where
            | Self::With => false,
        }
    }
}

impl ComparisonToken {
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
                Self::Lt => SLT,
                Self::Gt => SGT,
                Self::Lte => SLE,
                Self::Gte => SGE,
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

/// Type name.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeToken {
    Int,
    CellState,
    Vector(Option<usize>),
}
impl fmt::Display for TypeToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::CellState => write!(f, "cellstate"),
            Self::Vector(None) => write!(f, "vec"),
            Self::Vector(Some(len)) => write!(f, "vec{}", len),
        }
    }
}
impl FromStr for TypeToken {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        // TODO: test this method
        if let Some(len_str) = VEC_TYPE_PATTERN
            .captures(s)
            .and_then(|captures| captures.get(1))
            .as_ref()
            .map(regex::Match::as_str)
        {
            if let Ok(len @ 1..=super::types::MAX_VECTOR_LEN) = len_str.parse() {
                return Ok(Self::Vector(Some(len)));
            }
        }
        match s {
            "int" => Ok(Self::Int),
            "cellstate" => Ok(Self::CellState),
            "vec" => Ok(Self::Vector(None)),
            _ => Err(()),
        }
    }
}
impl TypeToken {
    /// Returns the Type corresponding to the given TypeToken, in an automaton
    /// with the given number of dimensions.
    pub fn resolve(self, ndim: u8) -> Type {
        match self {
            Self::Int => Type::Int,
            Self::CellState => Type::CellState,
            Self::Vector(None) => Type::Vector(ndim as usize),
            Self::Vector(Some(len)) => Type::Vector(len),
        }
    }
}

/// Assignment operator.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AssignmentToken {
    /// Simple `=` assignment.
    Assign,
    /// Assignment with an additional operator; e.g. `+=`.
    OpAssign(OperatorToken),
}
impl fmt::Display for AssignmentToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Assign => write!(f, "="),
            Self::OpAssign(op) => write!(f, "{}=", op),
        }
    }
}
impl FromStr for AssignmentToken {
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
impl AssignmentToken {
    /// Returns the token of the operator, if any, that this assignmeent uses.
    pub fn op(self) -> Option<OperatorToken> {
        match self {
            Self::Assign => None,
            Self::OpAssign(op) => Some(op),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::span::TextPoint;
    use super::*;

    #[test]
    fn test_tokenizer() {
        let source_code = "
@states [ #dead, #live ]
if this is {
  case #dead
    // die.
    remain
  case #live
    /**
     * live
     */
    become ( #live )
}
";

        let tokens = tokenize(source_code).expect("Tokenization failed");
        println!("{:?}", tokens);

        // Check number of tokens.
        assert_eq!(20, tokens.len());
        // Check content of "@states" token on line 2.
        assert_eq!("@states", tokens[0].string);
        // Check content of "#dead" token on line 4.
        assert_eq!("#dead", tokens[11].string);
        // Check span of "this" token on line 3.
        assert_eq!(
            (TextPoint(3, 4), TextPoint(3, 8)),
            tokens[7].span.textpoints(source_code)
        );
    }
}
