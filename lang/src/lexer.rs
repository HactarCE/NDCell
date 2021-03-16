use codemap::{File, Spanned};
use lazy_static::lazy_static;
use regex::Regex;
use std::fmt;

pub fn tokenize<'a>(file: &'a File) -> impl 'a + Iterator<Item = Spanned<Token>> {
    let mut token_start = 0;
    std::iter::from_fn(move || {
        Token::consume_from_input(file, token_start).map(|(token, token_end)| {
            let span = file.span.subspan(token_start as u64, token_end as u64);
            token_start = token_end;
            Spanned { node: token, span }
        })
    })
}

/// Identifier consisting of a letter or underscore followed by any letters,
/// digits, and/or underscores, with an optional `#` in front for tags or `@` in
/// front for directives.
const IDENT_PATTERN: &str = r#"[#@]?[A-Za-z_][A-Za-z_\d]*"#;

/// Unsigned decimal/binary/octal/hexadecimal integer literal.
const INTEGER_LITERAL_PATTERN: &str = r#"0[box][A-Za-z\d_]*|\d[\d_]*"#;

/// Single-quoted string, with an optional lowercase modifier character in
/// front. Note that like Rust strings, this can span multiple lines.
const SINGLE_QUOTE_STRING_LITERAL_PATTERN: &str = r#"[a-z]?'([^'\\]|\\[\s\S])*'"#;
/// Double-quoted string, with an optional lowercase modifier character in
/// front. Note that like Rust strings, this can span multiple lines.
const DOUBLE_QUOTE_STRING_LITERAL_PATTERN: &str = r#"[a-z]?"([^"\\]|\\[\s\S])*""#;
/// Unterminated string literal.
const UNTERMINATED_STRING_LITERAL_PATTERN: &str = r#"[a-z]?["']"#;

/// List of token patterns, arranged roughly from least to most general.
const TOKEN_PATTERNS: &[&str] = &[
    // In-place arithmetic operators `**=`, `<<=`, `>>=`, and `>>>=`.
    r#"(\*\*|<<|>>>?)="#,
    // In-place arithmetic operators `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, and `^=`.
    r#"[+\-*/%&|^]="#,
    // Operators `->`, `..`, `**`, `<<`, `>>`, and `>>>`.
    r#"->|\.\.|\*\*|<<|>>>?"#,
    // Comparison operators `==`, `!=`, `<=`, and `>=`.
    r#"[=!<>]="#,
    // Line comment.
    r#"//[^\n]*"#,
    // Start of a block comment (block comment has special handling).
    r#"/\*"#,
    // String literal.
    SINGLE_QUOTE_STRING_LITERAL_PATTERN,
    DOUBLE_QUOTE_STRING_LITERAL_PATTERN,
    UNTERMINATED_STRING_LITERAL_PATTERN,
    // Integer literal.
    INTEGER_LITERAL_PATTERN,
    // Identifier.
    IDENT_PATTERN,
    // Whitespace.
    r#"\s+"#,
    // Any other single Unicode character.
    r#"[\s\S]"#,
];

lazy_static! {
    /// Single regex that matches any token, including comments and strings,
    /// by joining each member of `TOKEN_PATTERNS` with "|".
    pub static ref TOKEN_REGEX: Regex =
        Regex::new(&TOKEN_PATTERNS.join("|")).unwrap();

    /// Regex that matches a valid identifier or keyword.
    pub static ref IDENT_REGEX: Regex =
        Regex::new(IDENT_PATTERN).unwrap();

    /// Regex that matches a valid unsigned decimal, binary, octal, or
    /// hexadecimal integer literal and some invalid integer literals.
    pub static ref INTEGER_LITERAL_REGEX: Regex =
        Regex::new(INTEGER_LITERAL_PATTERN).unwrap();

    /// Regex that matches a valid string literal.
    pub static ref STRING_LITERAL_REGEX: Regex =
        Regex::new(&[
            SINGLE_QUOTE_STRING_LITERAL_PATTERN,
            DOUBLE_QUOTE_STRING_LITERAL_PATTERN,
        ].join("|")).unwrap();

    /// Regex that matches an unterminated string literal.
    pub static ref UNTERMINATED_STRING_LITERAL_REGEX: Regex =
        Regex::new(UNTERMINATED_STRING_LITERAL_PATTERN).unwrap();
}

#[rustfmt::skip]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Token {
    // Grouping
    LParen, LBracket, LBrace,
    RParen, RBracket, RBrace,

    // Other punctuation
    Backtick, Colon, Comma, Period, Semicolon,

    // Comparison operators
    Eql, Neq, Lt, Gt, Lte, Gte,

    // Arithmetic operators
    Plus, Minus, Asterisk, Slash, Percent, DoubleAsterisk,

    // Bitshift operators
    DoubleLessThan, DoubleGreaterThan, TripleGreaterThan,

    // Bitwise/set operators
    Ampersand, Pipe, Caret, Tilde,

    // Miscellaneous operators
    Arrow, DotDot, Octothorpe,

    // Assignment operators
    Assign,
    AssignPlus, AssignMinus, AssignAsterisk, AssignSlash, AssignPercent, AssignDoubleAsterisk,
    AssignDoubleLessThan, AssignDoubleGreaterThan, AssignTripleGreaterThan,
    AssignAmpersand, AssignPipe, AssignCaret,

    // Keywords (operators)
    KeywordOr, KeywordXor, KeywordAnd, KeywordNot,
    KeywordIs, KeywordIn,

    // Keywords (type names)
    KeywordInteger, KeywordVector, KeywordCell, KeywordArray,
    KeywordIntegerSet, KeywordVectorSet, KeywordCellSet, KeywordPattern,
    KeywordString,
    KeywordType,
    KeywordTag,

    // Keywords (loops)
    KeywordBreak, KeywordContinue, KeywordFor,

    // Keywords (returns)
    KeywordBecome, KeywordRemain, KeywordReturn,

    // Keywords (branching)
    KeywordIf, KeywordElse,
    KeywordUnless,
    KeywordCase, KeywordMatch,

    // Keywords (debugging)
    KeywordAssert, KeywordError,

    // Comments
    Comment, UnterminatedBlockComment,

    // Other special tokens
    StringLiteral, UnterminatedStringLiteral,
    IntegerLiteral,
    Ident,
    Whitespace,
    Unknown,
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::LParen => "'('",
            Self::LBracket => "'['",
            Self::LBrace => "'{'",
            Self::RParen => "')'",
            Self::RBracket => "']'",
            Self::RBrace => "'}'",

            Self::Backtick => "'`'",
            Self::Colon => "':'",
            Self::Comma => "','",
            Self::Period => "'.'",
            Self::Semicolon => "';'",

            Self::Eql => "'=='",
            Self::Neq => "'!='",
            Self::Lt => "'<'",
            Self::Gt => "'>'",
            Self::Lte => "'<='",
            Self::Gte => "'>='",

            Self::Plus => "'+'",
            Self::Minus => "'-'",
            Self::Asterisk => "'*'",
            Self::Slash => "'/'",
            Self::Percent => "'%'",
            Self::DoubleAsterisk => "'**'",

            Self::DoubleLessThan => "'<<'",
            Self::DoubleGreaterThan => "'>>'",
            Self::TripleGreaterThan => "'>>>'",

            Self::Ampersand => "'&'",
            Self::Pipe => "'|'",
            Self::Caret => "'^'",
            Self::Tilde => "'~'",

            Self::Arrow => "'->'",
            Self::DotDot => "'..'",
            Self::Octothorpe => "'#'",

            Self::Assign => "'='",
            Self::AssignPlus => "'+='",
            Self::AssignMinus => "'-='",
            Self::AssignAsterisk => "'*='",
            Self::AssignSlash => "'/='",
            Self::AssignPercent => "'%='",
            Self::AssignDoubleAsterisk => "'**='",
            Self::AssignDoubleLessThan => "'<<='",
            Self::AssignDoubleGreaterThan => "'>>='",
            Self::AssignTripleGreaterThan => "'>>>='",
            Self::AssignAmpersand => "'&='",
            Self::AssignPipe => "'|='",
            Self::AssignCaret => "'^='",

            Self::KeywordOr => "'or'",
            Self::KeywordXor => "'xor'",
            Self::KeywordAnd => "'and'",
            Self::KeywordNot => "'not'",
            Self::KeywordIs => "'is'",
            Self::KeywordIn => "'in'",

            Self::KeywordInteger => "'Integer'",
            Self::KeywordVector => "'Vector'",
            Self::KeywordCell => "'Cell'",
            Self::KeywordArray => "'Array'",
            Self::KeywordIntegerSet => "'IntegerSet'",
            Self::KeywordVectorSet => "'VectorSet'",
            Self::KeywordCellSet => "'CellSet'",
            Self::KeywordPattern => "'Pattern'",
            Self::KeywordString => "'String'",
            Self::KeywordType => "'Type'",
            Self::KeywordTag => "'Tag'",

            Self::KeywordBreak => "'break'",
            Self::KeywordContinue => "'continue'",
            Self::KeywordFor => "'for'",

            Self::KeywordBecome => "'become'",
            Self::KeywordRemain => "'remain'",
            Self::KeywordReturn => "'return'",

            Self::KeywordIf => "'if'",
            Self::KeywordElse => "'else'",
            Self::KeywordUnless => "'unless'",
            Self::KeywordCase => "'case'",
            Self::KeywordMatch => "'match'",

            Self::KeywordAssert => "'assert'",
            Self::KeywordError => "'error'",

            Token::Comment => "comment",
            Token::UnterminatedBlockComment => "unterminated block comment",
            Token::StringLiteral => "string literal",
            Token::UnterminatedStringLiteral => "unterminated string literal",
            Token::IntegerLiteral => "integer literal",
            Token::Ident => "indentifier",
            Token::Whitespace => "whitespace",
            Token::Unknown => "unknown symbol",
        };
        write!(f, "{}", s)
    }
}
impl Token {
    fn consume_from_input(f: &File, start: usize) -> Option<(Self, usize)> {
        // Find next token.
        TOKEN_REGEX.find_at(f.source(), start).map(|m| {
            let mut end = m.end();

            let token = match m.as_str() {
                // Match a keyword or symbol.
                "(" => Self::LParen,
                "[" => Self::LBracket,
                "{" => Self::LBrace,
                ")" => Self::RParen,
                "]" => Self::RBracket,
                "}" => Self::RBrace,

                "`" => Self::Backtick,
                ":" => Self::Colon,
                "," => Self::Comma,
                "." => Self::Period,
                ";" => Self::Semicolon,

                "==" => Self::Eql,
                "!=" => Self::Neq,
                "<" => Self::Lt,
                ">" => Self::Gt,
                "<=" => Self::Lte,
                ">=" => Self::Gte,

                "+" => Self::Plus,
                "-" => Self::Minus,
                "*" => Self::Asterisk,
                "/" => Self::Slash,
                "%" => Self::Percent,
                "**" => Self::DoubleAsterisk,

                "<<" => Self::DoubleLessThan,
                ">>" => Self::DoubleGreaterThan,
                ">>>" => Self::TripleGreaterThan,

                "&" => Self::Ampersand,
                "|" => Self::Pipe,
                "^" => Self::Caret,
                "~" => Self::Tilde,

                "->" => Self::Arrow,
                ".." => Self::DotDot,
                "#" => Self::Octothorpe,

                "=" => Self::Assign,
                "+=" => Self::AssignPlus,
                "-=" => Self::AssignMinus,
                "*=" => Self::AssignAsterisk,
                "/=" => Self::AssignSlash,
                "%=" => Self::AssignPercent,
                "**=" => Self::AssignDoubleAsterisk,
                "<<=" => Self::AssignDoubleLessThan,
                ">>=" => Self::AssignDoubleGreaterThan,
                ">>>=" => Self::AssignTripleGreaterThan,
                "&=" => Self::AssignAmpersand,
                "|=" => Self::AssignPipe,
                "^=" => Self::AssignCaret,

                "or" => Self::KeywordOr,
                "xor" => Self::KeywordXor,
                "and" => Self::KeywordAnd,
                "not" => Self::KeywordNot,
                "is" => Self::KeywordIs,
                "in" => Self::KeywordIn,

                "Integer" => Self::KeywordInteger,
                "Vector" => Self::KeywordVector,
                "Cell" => Self::KeywordCell,
                "Array" => Self::KeywordArray,
                "IntegerSet" => Self::KeywordIntegerSet,
                "VectorSet" => Self::KeywordVectorSet,
                "CellSet" => Self::KeywordCellSet,
                "Pattern" => Self::KeywordPattern,
                "String" => Self::KeywordString,
                "Type" => Self::KeywordType,
                "Tag" => Self::KeywordTag,

                "break" => Self::KeywordBreak,
                "continue" => Self::KeywordContinue,
                "for" => Self::KeywordFor,

                "become" => Self::KeywordBecome,
                "remain" => Self::KeywordRemain,
                "return" => Self::KeywordReturn,

                "if" => Self::KeywordIf,
                "else" => Self::KeywordElse,
                "unless" => Self::KeywordUnless,
                "case" => Self::KeywordCase,
                "match" => Self::KeywordMatch,

                "assert" => Self::KeywordAssert,
                "error" => Self::KeywordError,

                // Match a line comment.
                s if s.starts_with("//") => Self::Comment,

                // Match a block comment.
                s if s.starts_with("/*") => {
                    lazy_static! {
                        static ref COMMENT_BOUNDARY_PATTERN: Regex =
                            Regex::new(r"/\*|\*/").unwrap();
                    }
                    let mut depth = 0;
                    let mut comment_len = 0;
                    for m in COMMENT_BOUNDARY_PATTERN.find_iter(&f.source()[start..]) {
                        comment_len = m.end();
                        match m.as_str() {
                            "/*" => depth += 1,
                            "*/" => depth -= 1,
                            _ => (), // should be impossible
                        }
                        if depth <= 0 {
                            break;
                        }
                    }
                    if depth == 0 {
                        end = start + comment_len;
                        Self::Comment
                    } else {
                        Self::UnterminatedBlockComment
                    }
                }

                // Match anything else.
                s if STRING_LITERAL_REGEX.is_match(s) => Self::StringLiteral,
                s if UNTERMINATED_STRING_LITERAL_REGEX.is_match(s) => Self::StringLiteral,
                s if INTEGER_LITERAL_REGEX.is_match(s) => Self::IntegerLiteral,
                s if IDENT_REGEX.is_match(s) => Self::Ident,
                s if s.trim().is_empty() => Self::Whitespace,

                // Give up.
                _ => Self::Unknown,
            };

            (token, end)
        })
    }

    /// Returns whether this token is a comment or whitespace that should be
    /// skipped most of the time when parsing.
    pub fn is_skip(self) -> bool {
        matches!(self, Self::Comment | Self::Whitespace)
    }

    pub const STATEMENT_KEYWORDS: &'static [Self] = &[
        Self::KeywordAssert,
        Self::KeywordBecome,
        Self::KeywordBreak,
        Self::KeywordCase,
        Self::KeywordContinue,
        Self::KeywordElse,
        Self::KeywordError,
        Self::KeywordFor,
        Self::KeywordIf,
        Self::KeywordMatch,
        Self::KeywordRemain,
        Self::KeywordReturn,
        Self::KeywordUnless,
    ];
}

#[cfg(test)]
mod tests {
    use codemap::CodeMap;
    use itertools::Itertools;

    use super::*;
    use crate::data::LangInt;

    #[test]
    fn test_lex_block_comment() {
        test_block_comment(true, "/* basic */");
        test_block_comment(true, "/* line1 \n line2 */");
        test_block_comment(true, "/* */");
        test_block_comment(true, "/**/");
        test_block_comment(true, "/***/");
        test_block_comment(true, "/****/");
        test_block_comment(true, "/** spooky * scary\n ** block *** comments **/");
        test_block_comment(true, "/* nested /*/ oh my! ***/*/");

        test_block_comment(false, "/* /*");
        test_block_comment(false, "/*/");
    }
    fn test_block_comment(expected_to_end: bool, s: &str) {
        let mut codemap = CodeMap::new();
        let file = codemap.add_file("one_token_test.ndca".to_string(), s.to_string());
        let tokens = tokenize(&file).collect_vec();
        if expected_to_end {
            assert_eq!(1, tokens.len(), "Too many tokens: {:?}", tokens);
        }
        let expected = if expected_to_end {
            Token::Comment
        } else {
            Token::UnterminatedBlockComment
        };
        assert_eq!(
            expected,
            tokens[0].node,
            "Token is: {:?}",
            file.source_slice(tokens[0].span),
        );
    }

    #[test]
    fn test_lex_int_literal() {
        assert_parse_int_result(Ok(10), "10");
        assert_parse_int_result(Ok(10), "010");
        assert_parse_int_result(Ok(10), "0_1_0");
        assert_parse_int_result(Ok(10), "0xa");
        assert_parse_int_result(Ok(10), "0x_A");
        assert_parse_int_result(Ok(10), "0b__0000_1010__");
        assert_parse_int_result(Ok(10), "0o12");
        assert_parse_int_result(Ok(42), "42");
        assert_parse_int_result(Ok(0x42), "0x42");
        assert_parse_int_result(Ok(0o42), "0o42");
        assert_parse_int_result(Ok(LangInt::MAX), &LangInt::MAX.to_string());
        assert_parse_int_result(Ok(LangInt::MAX), "0x7FFFFFFFFFFFFFFF");

        // Too big
        assert_parse_int_result(
            Err("number too large to fit in target type"),
            "0x8000000000000000",
        );

        // Malformed
        assert_parse_int_result(Err("cannot parse integer from empty string"), "0x");
        assert_parse_int_result(Err("invalid digit found in string"), "0oO");

        // What's a 4? https://xkcd.com/953/
        assert_parse_int_result(Err("invalid digit found in string"), "0b2");
        assert_parse_int_result(Err("invalid digit found in string"), "0xG");
        assert_parse_int_result(Err("invalid digit found in string"), "0o8");
    }
    fn assert_parse_int_result(expected: Result<LangInt, &str>, s: &str) {
        let mut codemap = CodeMap::new();
        let file = codemap.add_file("parse_int_test.ndca".to_string(), s.to_string());
        let tokens = tokenize(&file).collect_vec();
        assert_eq!(1, tokens.len(), "Too many tokens: {:?}", tokens);
        let s = file.source_slice(tokens[0].span);
        assert_eq!(Token::IntegerLiteral, tokens[0].node, "Token is: {:?}", s);
        let parse_result = parse_int::parse::<LangInt>(s).map_err(|e| e.to_string());
        match expected {
            Ok(i) => {
                assert_eq!(Ok(i), parse_result)
            }
            Err(e) => {
                assert_eq!(Err(e.to_owned()), parse_result)
            }
        }
    }

    #[test]
    fn test_lexer_example() {
        let s = r#"
            @name "What's in a string?"
            @desc r'What\'s in a prefixed string?'
            @states[#dead,#live ]
            @transition {
                match this {
                    case#dead{
                        // die.
                        remain
                    }
                    case # live {
                        /**
                        * live
                        */
                        become(#50)
                    }
                }
            }
        "#;

        let mut codemap = CodeMap::new();
        let file = codemap.add_file("lexer_test.ndca".to_string(), s.to_string());
        let tokens = tokenize(&file);

        let mut it = tokens.map(|Spanned { node, span }| (file.source_slice(span), node));

        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("@name", Token::Ident));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(
            it.next().unwrap(),
            (r#""What's in a string?""#, Token::StringLiteral),
        );
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("@desc", Token::Ident));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(
            it.next().unwrap(),
            (r#"r'What\'s in a prefixed string?'"#, Token::StringLiteral),
        );
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("@states", Token::Ident));
        assert_eq!(it.next().unwrap(), ("[", Token::LBracket));
        assert_eq!(it.next().unwrap(), ("#dead", Token::Ident));
        assert_eq!(it.next().unwrap(), (",", Token::Comma));
        assert_eq!(it.next().unwrap(), ("#live", Token::Ident));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("]", Token::RBracket));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("@transition", Token::Ident));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("{", Token::LBrace));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("match", Token::KeywordMatch));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("this", Token::Ident));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("{", Token::LBrace));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("case", Token::KeywordCase));
        assert_eq!(it.next().unwrap(), ("#dead", Token::Ident));
        assert_eq!(it.next().unwrap(), ("{", Token::LBrace));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap().1, Token::Comment);
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("remain", Token::KeywordRemain));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("}", Token::RBrace));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("case", Token::KeywordCase));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("#", Token::Octothorpe));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("live", Token::Ident));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("{", Token::LBrace));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap().1, Token::Comment);
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("become", Token::KeywordBecome));
        assert_eq!(it.next().unwrap(), ("(", Token::LParen));
        assert_eq!(it.next().unwrap(), ("#", Token::Octothorpe));
        assert_eq!(it.next().unwrap(), ("50", Token::IntegerLiteral));
        assert_eq!(it.next().unwrap(), (")", Token::RParen));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("}", Token::RBrace));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("}", Token::RBrace));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("}", Token::RBrace));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next(), None);
    }
}
