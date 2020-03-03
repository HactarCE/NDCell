use lazy_static::lazy_static;
use regex::Regex;
use std::convert::{TryFrom, TryInto};
use std::fmt;

use super::{CompileResult, Span};

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
    r#"\d?\.\d+"#,
    // Number without decimal point.
    r#"\d+"#,
    // Identifier consisting of a letter or underscore followed by any letters,
    // digits, and/or underscores, with an optional `#` (for tags) or `@` (for
    // directives) in front.
    r#"[#@]?[A-Za-z_][A-Za-z_\d]*"#,
    // Literal `..`.
    r#"\.\."#,
    // Literal `**`.
    r#"\*\*"#,
    // Equality checks `==`, `!=`, `<=`, and `>=`.
    r#"[=!<>]="#,
    // In-place arithmetic operator `**=`.
    r#"\*\*="#,
    // In-place arithmetic operators `+=`, `-=`, `*=`, and `/=`.
    r#"[+\-*/]="#,
    // Any other single character.
    r#"[^\s]"#,
];

lazy_static! {
    /// A single regex that matches any token, including comments and strings,
    /// by joining each member of TOKEN_PATTERNS with '|'.
    static ref TOKEN_PATTERN: Regex = Regex::new(&TOKEN_PATTERNS.join("|")).unwrap();

    static ref IDENT_PATTERN: Regex = Regex::new(r#"^[A-Za-z_][A-Za-z_\d]*$"#).unwrap();
    static ref TAG_PATTERN: Regex = Regex::new(r#"^#[A-Za-z_][A-Za-z_\d]*$"#).unwrap();
    static ref DIRECTIVE_PATTERN: Regex = Regex::new(r#"^@[A-Za-z_][A-Za-z_\d]*$"#).unwrap();
    static ref STRING_PATTERN: Regex = Regex::new(r#"^(\w?)(["'])(?:([\s\S]*)["'])?$"#).unwrap();
    static ref BLOCK_COMMENT_PATTERN: Regex = Regex::new(r#"^/\*"#).unwrap();
    static ref LINE_COMMENT_PATTERN: Regex = Regex::new(r#"^//"#).unwrap();
}

/// Splits a string into tokens and returns them as a Vec, with all comments
/// removed.
pub fn tokenize<'a>(s: &'a str) -> CompileResult<Vec<Token<'a>>> {
    let flat_tokens = TOKEN_PATTERN.find_iter(s).map(|m| {
        let span = Span {
            start: m.start(),
            end: m.end(),
        };
        let string = m.as_str();
        match TokenClass::try_from(string) {
            Ok(class) => Ok(Token {
                span,
                string,
                class,
            }),
            Err(msg) => Err((span, msg)),
        }
    });
    flat_tokens
        // Remove comments.
        .filter(|t| !t.as_ref().map_or(false, Token::is_comment))
        .collect()
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub span: Span,
    pub string: &'a str,
    pub class: TokenClass<'a>,
}
impl<'a> Token<'a> {
    fn is_comment(&self) -> bool {
        self.class == TokenClass::Comment
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenClass<'a> {
    StatementKeyword(StatementKeywordToken),
    Keyword(KeywordToken),
    Operator(OperatorToken),
    Assignment(AssignmentToken),
    Comparison(ComparisonToken),
    Punctuation(PunctuationToken),
    Integer(i64),
    String {
        prefix: Option<char>,
        quote: char,
        contents: &'a str,
    },
    Directive(&'a str),
    Tag(&'a str),
    Ident(&'a str),
    Comment,
}
impl<'a> fmt::Display for TokenClass<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::StatementKeyword(t) => write!(f, "keyword '{}'", t),
            Self::Keyword(t) => write!(f, "keyword '{}'", t),
            Self::Operator(t) => write!(f, "operator '{}'", t),
            Self::Assignment(t) => write!(f, "assignment operator '{}'", t),
            Self::Comparison(t) => write!(f, "comparison operator '{}'", t),
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
impl<'a> TryFrom<&'a str> for TokenClass<'a> {
    type Error = &'static str;
    fn try_from(s: &'a str) -> Result<Self, &'static str> {
        if let Ok(statement_keyword) = s.try_into() {
            Ok(Self::StatementKeyword(statement_keyword))
        } else if let Ok(keyword) = s.try_into() {
            Ok(Self::Keyword(keyword))
        } else if let Ok(operator) = s.try_into() {
            Ok(Self::Operator(operator))
        } else if let Ok(assignment) = s.try_into() {
            Ok(Self::Assignment(assignment))
        } else if let Ok(comparison) = s.try_into() {
            Ok(Self::Comparison(comparison))
        } else if let Ok(punctuation) = s.try_into() {
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
                Err("This string never ends")
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
                Err("This block comment never ends")
            } else {
                Ok(Self::Comment)
            }
        } else {
            Err("Unknown symbol")
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum StatementKeywordToken {
    Become,
    Break,
    Case,
    Continue,
    Else,
    For,
    If,
    Remain,
    Return,
    Set,
    Unless,
    While,
}
impl fmt::Display for StatementKeywordToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Become => "become",
                Self::Break => "break",
                Self::Case => "case",
                Self::Continue => "continue",
                Self::Else => "else",
                Self::For => "for",
                Self::If => "if",
                Self::Remain => "remain",
                Self::Return => "return",
                Self::Set => "set",
                Self::Unless => "unless",
                Self::While => "while",
            }
        )
    }
}
impl TryFrom<&str> for StatementKeywordToken {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, ()> {
        match s {
            "become" => Ok(Self::Become),
            "break" => Ok(Self::Break),
            "case" => Ok(Self::Case),
            "continue" => Ok(Self::Continue),
            "else" => Ok(Self::Else),
            "for" => Ok(Self::For),
            "if" => Ok(Self::If),
            "remain" => Ok(Self::Remain),
            "return" => Ok(Self::Return),
            "set" => Ok(Self::Set),
            "unless" => Ok(Self::Unless),
            "while" => Ok(Self::While),
            _ => Err(()),
        }
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum KeywordToken {
    In,
    Is,
    Where,
}
impl fmt::Display for KeywordToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::In => "in",
                Self::Is => "is",
                Self::Where => "where",
            }
        )
    }
}
impl TryFrom<&str> for KeywordToken {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, ()> {
        match s {
            "in" => Ok(Self::In),
            "is" => Ok(Self::Is),
            "where" => Ok(Self::Where),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OperatorToken {
    Ampersand,
    And,
    Asterisk,
    Caret,
    Dot,
    DotDot,
    DoubleAsterisk,
    Minus,
    Or,
    Pipe,
    Plus,
    Slash,
    Tag,
    Xor,
}
impl fmt::Display for OperatorToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Ampersand => "&",
                Self::And => "and",
                Self::Asterisk => "*",
                Self::Caret => "^",
                Self::Dot => ".",
                Self::DotDot => "..",
                Self::DoubleAsterisk => "**",
                Self::Minus => "-",
                Self::Or => "or",
                Self::Pipe => "|",
                Self::Plus => "+",
                Self::Slash => "/",
                Self::Tag => "#",
                Self::Xor => "xor",
            }
        )
    }
}
impl TryFrom<&str> for OperatorToken {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, ()> {
        match s {
            "&" => Ok(Self::Ampersand),
            "and" => Ok(Self::And),
            "*" => Ok(Self::Asterisk),
            "^" => Ok(Self::Caret),
            "." => Ok(Self::Dot),
            ".." => Ok(Self::DotDot),
            "**" => Ok(Self::DoubleAsterisk),
            "-" => Ok(Self::Minus),
            "or" => Ok(Self::Or),
            "|" => Ok(Self::Pipe),
            "+" => Ok(Self::Plus),
            "/" => Ok(Self::Slash),
            "#" => Ok(Self::Tag),
            "xor" => Ok(Self::Xor),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AssignmentToken {
    Assign,
    AddAssign,
    DivAssign,
    MulAssign,
    SubAssign,
}
impl fmt::Display for AssignmentToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Assign => "=",
                Self::AddAssign => "+=",
                Self::DivAssign => "/=",
                Self::MulAssign => "*=",
                Self::SubAssign => "-=",
            }
        )
    }
}
impl TryFrom<&str> for AssignmentToken {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, ()> {
        match s {
            "=" => Ok(Self::Assign),
            "+=" => Ok(Self::AddAssign),
            "/=" => Ok(Self::DivAssign),
            "*=" => Ok(Self::MulAssign),
            "-=" => Ok(Self::SubAssign),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ComparisonToken {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}
impl fmt::Display for ComparisonToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Equal => "==",
                Self::NotEqual => "!=",
                Self::LessThan => "<",
                Self::GreaterThan => ">",
                Self::LessThanOrEqual => "<=",
                Self::GreaterThanOrEqual => ">=",
            }
        )
    }
}
impl TryFrom<&str> for ComparisonToken {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, ()> {
        match s {
            "==" => Ok(Self::Equal),
            "!=" => Ok(Self::NotEqual),
            "<" => Ok(Self::LessThan),
            ">" => Ok(Self::GreaterThan),
            "<=" => Ok(Self::LessThanOrEqual),
            ">=" => Ok(Self::GreaterThanOrEqual),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PunctuationToken {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
}
impl fmt::Display for PunctuationToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::LParen => "(",
                Self::RParen => ")",
                Self::LBracket => "[",
                Self::RBracket => "]",
                Self::LBrace => "{",
                Self::RBrace => "}",
                Self::Comma => ",",
                Self::Semicolon => ";",
            }
        )
    }
}
impl TryFrom<&str> for PunctuationToken {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, ()> {
        match s {
            "(" => Ok(Self::LParen),
            ")" => Ok(Self::RParen),
            "[" => Ok(Self::LBracket),
            "]" => Ok(Self::RBracket),
            "{" => Ok(Self::LBrace),
            "}" => Ok(Self::RBrace),
            "," => Ok(Self::Comma),
            ";" => Ok(Self::Semicolon),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::TextPoint;
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
        // println!("{:?}", tokens);
        // Check number of tokens.
        assert_eq!(20, tokens.len());
        // Check content of "@states" token on line 2.
        assert_eq!("@states", tokens[0].string);
        // Check content of "#dead" token on line 4.
        assert_eq!("#dead", tokens[11].string);
        // Check span of "this" token on line 3.
        assert_eq!(
            (TextPoint(3, 4), TextPoint(3, 8)),
            tokens[3].span.textpoints(source_code)
        );
    }
}
