//! Token enum and tokenizer.
//!
//! Several of these regexes are taken from here:
//! https://github.com/maciejhirsz/logos/issues/133

use logos::{Lexer, Logos};
use regex::Regex;

#[derive(Logos, Debug, Copy, Clone, PartialEq, Eq)]
pub enum Token {
    #[error]
    #[regex(r"\d[A-Za-z_\d]+", priority = 0)] // Identifiers can't start with a digit
    Unknown,

    #[regex(r"\s+")]
    Whitespace,

    #[regex(r"//[^\n]*")]
    // Line comment
    // #[regex(r"/\*([^*]|\*+[^*/])*\*+/", priority = 10)] // Block comment
    // #[regex(r"/\*[\s\S]+(?<=\*)/", priority = 10)] // Block comment
    #[regex(r"/\*", lex_block_comment, priority = 10)]
    Comment,

    #[regex(r"/\*[\s\S]*", priority = 0)] // Unterminated block comment
    UnterminatedBlockComment,

    #[regex(r"[A-Za-z_][A-Za-z\d_]*")]
    Ident,

    #[regex(r"0[box][A-Za-z\d_]*")] // Bin/oct/hex literal
    #[regex(r"\d[\d_]*", priority = 10)]
    IntegerLiteral,

    #[regex(r#"[A-Za-z]?"([^"\\]|\[\s\S])*""#, priority = 10)]
    #[regex(r#"[A-Za-z]?'([^'\\]|\[\s\S])*'"#, priority = 10)]
    StringLiteral,

    // Punctuation
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(".")]
    Period,
    #[token("`")]
    Backtick,

    // Comparison operators
    #[token("==")]
    Eql,
    #[token("!=")]
    Neq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Lte,
    #[token(">=")]
    Gte,

    // Arithmetic operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("**")]
    DoubleAsterisk,

    // Bitshift operators
    #[token("<<")]
    DoubleLessThan,
    #[token(">>")]
    DoubleGreaterThan,
    #[token(">>>")]
    TripleGreaterThan,

    // Bitwise/set operators
    #[token("&")]
    Ampersand,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,

    // Miscellaneous operators
    #[token("@")]
    At,
    #[token("..")]
    DotDot,
    #[token("#")]
    Octothorpe,

    // Assignment operators
    #[token("=")]
    Assign,
    #[token("+=")]
    AssignPlus,
    #[token("-=")]
    AssignMinus,
    #[token("*=")]
    AssignAsterisk,
    #[token("/=")]
    AssignSlash,
    #[token("%=")]
    AssignPercent,
    #[token("**=")]
    AssignDoubleAsterisk,
    #[token("<<=")]
    AssignDoubleLessThan,
    #[token(">>=")]
    AssignDoubleGreaterThan,
    #[token(">>>=")]
    AssignTripleGreaterThan,
    #[token("&=")]
    AssignAmpersand,
    #[token("|=")]
    AssignPipe,
    #[token("^=")]
    AssignCaret,
    #[token("~=")]
    AssignTilde,

    // Keywords (type names)
    #[token("Integer")]
    KeywordInteger,
    #[token("Vector")]
    KeywordVector,
    #[token("Cell")]
    KeywordCell,
    #[token("Array")]
    KeywordArray,
    #[token("IntegerSet")]
    KeywordIntegerSet,
    #[token("VectorSet")]
    KeywordVectorSet,
    #[token("CellSet")]
    KeywordCellSet,
    #[token("Pattern")]
    KeywordPattern,
    #[token("Tag")]
    KeywordTag,

    // Keywords (loops)
    #[token("break")]
    KeywordBreak,
    #[token("continue")]
    KeywordContinue,
    #[token("for")]
    KeywordFor,
    #[token("while")]
    KeywordWhile,

    // Keywords (returns)
    #[token("become")]
    KeywordBecome,
    #[token("remain")]
    KeywordRemain,
    #[token("return")]
    KeywordReturn,

    // Keywords (branching)
    #[token("case")]
    KeywordCase,
    #[token("else")]
    KeywordElse,
    #[token("if")]
    KeywordIf,
    #[token("match")]
    KeywordMatch,
    #[token("unless")]
    KeywordUnless,

    // Keywords (debugging)
    #[token("assert")]
    KeywordAssert,
    #[token("error")]
    KeywordError,

    // Keywords (operators)
    #[token("or")]
    KeywordOr,
    #[token("xor")]
    KeywordXor,
    #[token("and")]
    KeywordAnd,
    #[token("not")]
    KeywordNot,
    #[token("is")]
    KeywordIs,
    #[token("in")]
    KeywordIn,
}

fn lex_block_comment(lex: &mut Lexer<Token>) -> bool {
    lazy_static! {
        static ref COMMENT_BOUNDARY_PATTERN: Regex = Regex::new(r"/\*|\*/").unwrap();
    }
    let mut depth = 1;
    for m in COMMENT_BOUNDARY_PATTERN.find_iter(lex.remainder()) {
        match m.as_str() {
            "/*" => depth += 1,
            "*/" => {
                depth -= 1;
                if depth <= 0 {
                    lex.bump(m.end());
                    return true;
                }
            }
            _ => (), // should be impossible
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::LangInt;

    #[test]
    fn test_lex_block_comment() {
        assert_one_token(Token::Comment, "/* basic */");
        assert_one_token(Token::Comment, "/* line1 \n line2 */");
        assert_one_token(Token::Comment, "/* */");
        assert_one_token(Token::Comment, "/**/");
        assert_one_token(Token::Comment, "/***/");
        assert_one_token(Token::Comment, "/****/");
        assert_one_token(
            Token::Comment,
            "/** spooky * scary\n ** block *** comments **/",
        );
        assert_one_token(Token::Comment, "/* nested /*/ oh my! ***/*/");

        assert_one_token(Token::UnterminatedBlockComment, "/* /*");
        assert_one_token(Token::UnterminatedBlockComment, "/*/");
    }
    fn assert_one_token(expected: Token, s: &str) {
        let mut lex = Token::lexer(s);
        assert_eq!(Some(expected), lex.next(), "Token is: {:?}", lex.slice());
        assert_eq!(None, lex.next(), "Token is: {:?}", lex.slice());
    }

    #[test]
    fn test_lex_int_literal() {
        assert_parse_int_result(Ok(10), "10");
        assert_parse_int_result(Ok(10), "010");
        assert_parse_int_result(Ok(10), "0_1_0");
        assert_parse_int_result(Ok(10), "0x_A");
        assert_parse_int_result(Ok(10), "0xa");
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
        let mut lex = Token::lexer(s);
        let token = lex.next().unwrap();
        assert_eq!(Token::IntegerLiteral, token);
        match expected {
            Ok(i) => {
                assert_eq!(Ok(i), parse_int::parse(lex.slice()))
            }
            Err(e) => {
                assert_eq!(
                    Err(e.to_owned()),
                    parse_int::parse::<LangInt>(lex.slice()).map_err(|e| e.to_string()),
                )
            }
        }
        assert_eq!(None, lex.next());
    }

    #[test]
    fn test_lexer_example() {
        let s = "
            @name \"What's in a string?\"
            @desc r\"What's in a prefixed string?\"
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
        ";
        let lex = Token::lexer(s);

        let mut it = lex.spanned().map(|(tok, span)| (&s[span], tok));

        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("@", Token::At));
        assert_eq!(it.next().unwrap(), ("name", Token::Ident));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(
            it.next().unwrap(),
            ("\"What's in a string?\"", Token::StringLiteral),
        );
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("@", Token::At));
        assert_eq!(it.next().unwrap(), ("desc", Token::Ident));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(
            it.next().unwrap(),
            ("r\"What's in a prefixed string?\"", Token::StringLiteral),
        );
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("@", Token::At));
        assert_eq!(it.next().unwrap(), ("states", Token::Ident));
        assert_eq!(it.next().unwrap(), ("[", Token::LBracket));
        assert_eq!(it.next().unwrap(), ("#", Token::Octothorpe));
        assert_eq!(it.next().unwrap(), ("dead", Token::Ident));
        assert_eq!(it.next().unwrap(), (",", Token::Comma));
        assert_eq!(it.next().unwrap(), ("#", Token::Octothorpe));
        assert_eq!(it.next().unwrap(), ("live", Token::Ident));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);
        assert_eq!(it.next().unwrap(), ("]", Token::RBracket));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("@", Token::At));
        assert_eq!(it.next().unwrap(), ("transition", Token::Ident));
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
        assert_eq!(it.next().unwrap(), ("#", Token::Octothorpe));
        assert_eq!(it.next().unwrap(), ("dead", Token::Ident));
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
        assert_eq!(it.next().unwrap(), ("}", Token::RBrace));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("}", Token::RBrace));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next().unwrap(), ("}", Token::RBrace));
        assert_eq!(it.next().unwrap().1, Token::Whitespace);

        assert_eq!(it.next(), None);
    }
}

// /// Returns the token for a symbol or keyword.
// macro_rules! T {
//     [ '(' ] => {
//         Token::LParen
//     };
//     [ ')' ] => {
//         Token::RParen
//     };
//     [ '[' ] => {
//         Token::LBracket
//     };
//     [ ']' ] => {
//         Token::RBracket
//     };
//     [ '{' ] => {
//         Token::LBrace
//     };
//     [ '}' ] => {
//         Token::RBrace
//     };
//     [ , ] => {
//         Token::Comma
//     };
//     [ ; ] => {
//         Token::Semicolon
//     };
//     [ . ] => {
//         Token::Period
//     };
//     [ '`' ] => {
//         Token::Backtick
//     };
//     [ == ] => {
//         Token::Eql
//     };
//     [ != ] => {
//         Token::Neq
//     };
//     [ < ] => {
//         Token::Lt
//     };
//     [ > ] => {
//         Token::Gt
//     };
//     [ <= ] => {
//         Token::Lte
//     };
//     [ >= ] => {
//         Token::Gte
//     };
//     [ + ] => {
//         Token::Plus
//     };
//     [ - ] => {
//         Token::Minus
//     };
//     [ * ] => {
//         Token::Asterisk
//     };
//     [ / ] => {
//         Token::Slash
//     };
//     [ % ] => {
//         Token::Percent
//     };
//     [ ** ] => {
//         Token::DoubleAsterisk
//     };
//     [ << ] => {
//         Token::DoubleLessThan
//     };
//     [ >> ] => {
//         Token::DoubleGreaterThan
//     };
//     [ >>> ] => {
//         Token::TripleGreaterThan
//     };
//     [ & ] => {
//         Token::Ampersand
//     };
//     [ | ] => {
//         Token::Pipe
//     };
//     [ ^ ] => {
//         Token::Caret
//     };
//     [ ~ ] => {
//         Token::Tilde
//     };
//     [ @ ] => {
//         Token::At
//     };
//     [ .. ] => {
//         Token::DotDot
//     };
//     [ # ] => {
//         Token::Octothorpe
//     };
//     [ = ] => {
//         Token::Assign
//     };
//     [ += ] => {
//         Token::PlusAssign
//     };
//     [ -= ] => {
//         Token::MinusAssign
//     };
//     [ *= ] => {
//         Token::AsteriskAssign
//     };
//     [ /= ] => {
//         Token::SlashAssign
//     };
//     [ %= ] => {
//         Token::PercentAssign
//     };
//     [ **= ] => {
//         Token::DoubleAsteriskAssign
//     };
//     [ <<= ] => {
//         Token::DoubleLessThanAssign
//     };
//     [ >>= ] => {
//         Token::DoubleGreaterThanAssign
//     };
//     [ >>>= ] => {
//         Token::TripleGreaterThanAssign
//     };
//     [ &= ] => {
//         Token::AmpersandAssign
//     };
//     [ |= ] => {
//         Token::PipeAssign
//     };
//     [ ^= ] => {
//         Token::CaretAssign
//     };
//     [ ~= ] => {
//         Token::TildeAssign
//     };
//     [ Integer ] => {
//         Token::IntegerType
//     };
//     [ Vector ] => {
//         Token::VectorType
//     };
//     [ Cell ] => {
//         Token::CellType
//     };
//     [ Array ] => {
//         Token::ArrayType
//     };
//     [ IntegerSet ] => {
//         Token::IntegerSet
//     };
//     [ VectorSet ] => {
//         Token::VectorSet
//     };
//     [ CellSet ] => {
//         Token::CellSetType
//     };
//     [ Pattern ] => {
//         Token::PatternType
//     };
//     [ Tag ] => {
//         Token::TagType
//     };
//     [ become ] => {
//         Token::BecomeKeyword
//     };
//     [ remain ] => {
//         Token::RemainKeyword
//     };
//     [ return ] => {
//         Token::ReturnKeyword
//     };
// }
