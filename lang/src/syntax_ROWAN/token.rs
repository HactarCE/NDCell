//! Token enum and tokenizer.
//!
//! Several of these regexes are taken from here:
//! https://github.com/maciejhirsz/logos/issues/133

use logos::Logos;

use crate::types::LangInt;

#[derive(Logos, Debug, Copy, Clone, PartialEq, Eq)]
#[logos(subpattern dec = r"[0-9][_0-9]*")]
#[logos(subpattern hex = r"[0-9a-fA-F][_0-9a-fA-F]*")]
#[logos(subpattern oct = r"[0-7][_0-7]*")]
#[logos(subpattern bin = r"[01][_01]*")]
#[logos(subpattern ident = r"[A-Za-z_][A-Za-z\d_]*")]
pub enum Token {
    #[error]
    #[regex(r"\d[A-Za-z_\d]+", priority = 0)] // Identifiers can't start with a digit
    #[regex(r"/\*[\s\S]*")] // Unterminated block comment
    Error,

    #[regex(r"\s+")]
    Whitespace,

    #[regex(r"//[^\n]*")] // Line comment
    #[regex(r"/\*([^*]|\*+[^*/])*\*+/")] // Block comment
    Comment,

    #[regex(r"(?&ident)")]
    Ident,

    #[regex(r"0[box][A-Za-z\d_]*", |lex| parse_int::parse(lex.slice()).ok())] // Bin/oct/hex literal
    #[regex(r"\d[\d_]*", |lex| parse_int::parse(lex.slice()).ok(), priority = 10)]
    Integer(LangInt),

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
    PlusAssign,
    #[token("-=")]
    MinusAssign,
    #[token("*=")]
    AsteriskAssign,
    #[token("/=")]
    SlashAssign,
    #[token("%=")]
    PercentAssign,
    #[token("**=")]
    DoubleAsteriskAssign,
    #[token("<<=")]
    DoubleLessThanAssign,
    #[token(">>=")]
    DoubleGreaterThanAssign,
    #[token(">>>=")]
    TripleGreaterThanAssign,
    #[token("&=")]
    AmpersandAssign,
    #[token("|=")]
    PipeAssign,
    #[token("^=")]
    CaretAssign,
    #[token("~=")]
    TildeAssign,

    // Keyword (type name)
    #[token("Integer")]
    IntegerType,
    #[token("IntegerSet")]
    IntegerSet,
    #[token("Vector")]
    VectorType,
    #[token("VectorSet")]
    VectorSet,
    #[token("Cell")]
    CellType,
    #[token("CellSet")]
    CellSetType,
    #[token("Array")]
    ArrayType,
    #[token("Pattern")]
    PatternType,
    #[token("Mask")]
    MaskType,
    #[token("Tag")]
    TagType,

    // Keyword (returns)
    #[token("become")]
    Become,
    #[token("remain")]
    Remain,
    #[token("return")]
    Return,
}

#[cfg(test)]
mod tests {
    use super::*;

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
            "/** spooky * scary ** block *** comments **/",
        );
        assert_one_token(Token::Comment, "/* /* /*/");

        assert_one_token(Token::Error, "/* /*");
        assert_one_token(Token::Error, "/*/");
    }

    #[test]
    fn test_lex_int_literal() {
        assert_one_token(Token::Integer(10), "10");
        assert_one_token(Token::Integer(10), "010");
        assert_one_token(Token::Integer(10), "0_1_0");
        assert_one_token(Token::Integer(10), "0x_A");
        assert_one_token(Token::Integer(10), "0xa");
        assert_one_token(Token::Integer(10), "0b__0000_1010__");
        assert_one_token(Token::Integer(10), "0o12");
        assert_one_token(Token::Integer(42), "42");
        assert_one_token(Token::Integer(0x42), "0x42");
        assert_one_token(Token::Integer(0o42), "0o42");
        assert_one_token(Token::Integer(LangInt::MAX), &LangInt::MAX.to_string());
        assert_one_token(Token::Integer(LangInt::MAX), "0x7FFFFFFFFFFFFFFF");

        // Too big
        assert_one_token(Token::Error, "0x8000000000000000");

        // Malformed
        assert_one_token(Token::Error, "0_b1");
        assert_one_token(Token::Error, "0x");
        assert_one_token(Token::Error, "0oO");

        // What's a 4? https://xkcd.com/953/
        assert_one_token(Token::Error, "0b2");
        assert_one_token(Token::Error, "0xG");
        assert_one_token(Token::Error, "0o8");
        assert_one_token(Token::Error, "0A");
    }

    fn assert_one_token(expected: Token, s: &str) {
        let mut lex = Token::lexer(s);
        assert_eq!(Some(expected), lex.next(), "Token is: {:?}", lex.slice());
        assert_eq!(None, lex.next(), "Token is: {:?}", lex.slice());
    }
}

/// Returns the token for a symbol or keyword.
macro_rules! T {
    [ '(' ] => {
        Token::LParen
    };
    [ ')' ] => {
        Token::RParen
    };
    [ '[' ] => {
        Token::LBracket
    };
    [ ']' ] => {
        Token::RBracket
    };
    [ '{' ] => {
        Token::LBrace
    };
    [ '}' ] => {
        Token::RBrace
    };
    [ , ] => {
        Token::Comma
    };
    [ ; ] => {
        Token::Semicolon
    };
    [ . ] => {
        Token::Period
    };
    [ '`' ] => {
        Token::Backtick
    };
    [ == ] => {
        Token::Eql
    };
    [ != ] => {
        Token::Neq
    };
    [ < ] => {
        Token::Lt
    };
    [ > ] => {
        Token::Gt
    };
    [ <= ] => {
        Token::Lte
    };
    [ >= ] => {
        Token::Gte
    };
    [ + ] => {
        Token::Plus
    };
    [ - ] => {
        Token::Minus
    };
    [ * ] => {
        Token::Asterisk
    };
    [ / ] => {
        Token::Slash
    };
    [ % ] => {
        Token::Percent
    };
    [ ** ] => {
        Token::DoubleAsterisk
    };
    [ << ] => {
        Token::DoubleLessThan
    };
    [ >> ] => {
        Token::DoubleGreaterThan
    };
    [ >>> ] => {
        Token::TripleGreaterThan
    };
    [ & ] => {
        Token::Ampersand
    };
    [ | ] => {
        Token::Pipe
    };
    [ ^ ] => {
        Token::Caret
    };
    [ ~ ] => {
        Token::Tilde
    };
    [ @ ] => {
        Token::At
    };
    [ .. ] => {
        Token::DotDot
    };
    [ # ] => {
        Token::Octothorpe
    };
    [ = ] => {
        Token::Assign
    };
    [ += ] => {
        Token::PlusAssign
    };
    [ -= ] => {
        Token::MinusAssign
    };
    [ *= ] => {
        Token::AsteriskAssign
    };
    [ /= ] => {
        Token::SlashAssign
    };
    [ %= ] => {
        Token::PercentAssign
    };
    [ **= ] => {
        Token::DoubleAsteriskAssign
    };
    [ <<= ] => {
        Token::DoubleLessThanAssign
    };
    [ >>= ] => {
        Token::DoubleGreaterThanAssign
    };
    [ >>>= ] => {
        Token::TripleGreaterThanAssign
    };
    [ &= ] => {
        Token::AmpersandAssign
    };
    [ |= ] => {
        Token::PipeAssign
    };
    [ ^= ] => {
        Token::CaretAssign
    };
    [ ~= ] => {
        Token::TildeAssign
    };
    [ Integer ] => {
        Token::IntegerType
    };
    [ IntegerSet ] => {
        Token::IntegerSet
    };
    [ Vector ] => {
        Token::VectorType
    };
    [ VectorSet ] => {
        Token::VectorSet
    };
    [ Cell ] => {
        Token::CellType
    };
    [ CellSet ] => {
        Token::CellSetType
    };
    [ Array ] => {
        Token::ArrayType
    };
    [ Pattern ] => {
        Token::PatternType
    };
    [ Mask ] => {
        Token::MaskType
    };
    [ Tag ] => {
        Token::TagType
    };
    [ become ] => {
        Token::Become
    };
    [ remain ] => {
        Token::Remain
    };
    [ return ] => {
        Token::Return
    };
}
