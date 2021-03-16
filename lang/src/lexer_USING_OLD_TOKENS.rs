//! Functions for tokenization.

use codemap::File;
use itertools::Itertools;
use std::sync::Arc;

use crate::errors::Error;
use crate::tokens::*;

/// Splits a string into tokens and returns them as a Vec, with all comments
/// removed.
pub fn tokenize(f: &Arc<File>) -> crate::Result<Vec<Token>> {
    TOKEN_PATTERN
        .find_iter(f.source())
        .filter_map(|m| {
            let s = m.as_str();
            let file = Arc::clone(f);

            // Get the span of the token.
            let span = f.span.subspan(m.start() as u64, m.end() as u64);

            // Classify the token.
            let kind = if let Ok(keyword) = s.parse() {
                Kind::Keyword(keyword)
            } else if let Ok(ty) = s.parse() {
                Kind::Type(ty)
            } else if let Ok(assignment) = s.parse() {
                Kind::Assignment(assignment)
            } else if let Ok(comparison) = s.parse() {
                Kind::Comparison(comparison)
            } else if let Ok(operator) = s.parse() {
                Kind::Operator(operator)
            } else if let Ok(punctuation) = s.parse() {
                Kind::Punctuation(punctuation)
            } else if INT_PATTERN.is_match(s) {
                match parse_int::parse(s) {
                    Ok(i) => Kind::Integer(i),
                    Err(e) => return Some(Err(Error::invalid_integer_literal(span, e))),
                }
            } else if let Some(captures) = STRING_PATTERN.captures(s) {
                if let Some(contents_capture) = captures.get(3) {
                    let prefix = captures.get(1).unwrap().as_str().chars().next();
                    let quote = captures
                        .get(2)
                        .unwrap()
                        .as_str()
                        .chars()
                        .exactly_one()
                        .unwrap();
                    let contents = span.subspan(
                        contents_capture.start() as u64,
                        contents_capture.end() as u64,
                    );
                    Kind::String {
                        prefix,
                        quote,
                        contents,
                    }
                } else {
                    return Some(Err(Error::unterminated(span, "string")));
                }
            } else if IDENT_PATTERN.is_match(s) {
                Kind::Ident
            } else if LINE_COMMENT_PATTERN.is_match(s) {
                return None; // Filter out comments.
            } else if BLOCK_COMMENT_PATTERN.is_match(s) {
                if s == "/*" {
                    return Some(Err(Error::unterminated(span, "block comment")));
                } else {
                    return None; // Filter out comments.
                }
            } else {
                Kind::Unknown
            };

            Some(Ok(Token { kind, span, file }))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use codemap::CodeMap;
    use itertools::Itertools;

    use super::*;
    use crate::data::LangInt;

    #[test]
    fn test_lex_block_comment() {
        assert_no_tokens("/* basic */");
        assert_no_tokens("/* line1 \n line2 */");
        assert_no_tokens("/* */");
        assert_no_tokens("/**/");
        assert_no_tokens("/***/");
        assert_no_tokens("/****/");
        assert_no_tokens("/** spooky * scary ** block *** comments **/");
        assert_no_tokens("/* /* /*/");

        assert_one_token(Err("This comment never ends"), "/* /*");
        assert_one_token(Err("This comment never ends"), "/*/");
    }
    fn assert_no_tokens(s: &str) {
        println!("Tokenizing {:?}", s);
        let mut codemap = CodeMap::new();
        let file = codemap.add_file("single_token.ndca".to_string(), s.to_string());
        let result = tokenize(&file);
        match result {
            Ok(tokens) => {
                if let Some(t) = tokens.iter().next() {
                    panic!("Expected nothing; got token: {:?}", t.kind);
                }
            }
            Err(e) => panic!("{:?}", e),
        }
    }

    #[test]
    fn test_lex_int_literal() {
        assert_one_token(Ok(Kind::Integer(10)), "10");
        assert_one_token(Ok(Kind::Integer(10)), "010");
        assert_one_token(Ok(Kind::Integer(10)), "0_1_0");
        assert_one_token(Ok(Kind::Integer(10)), "0x_A");
        assert_one_token(Ok(Kind::Integer(10)), "0xa");
        assert_one_token(Ok(Kind::Integer(10)), "0b__0000_1010__");
        assert_one_token(Ok(Kind::Integer(10)), "0o12");
        assert_one_token(Ok(Kind::Integer(42)), "42");
        assert_one_token(Ok(Kind::Integer(0x42)), "0x42");
        assert_one_token(Ok(Kind::Integer(0o42)), "0o42");
        assert_one_token(Ok(Kind::Integer(LangInt::MAX)), &LangInt::MAX.to_string());
        assert_one_token(Ok(Kind::Integer(LangInt::MAX)), "0x7FFFFFFFFFFFFFFF");

        // Too big
        let expected = Err("Can't parse this; it looks like an integer literal, but number too large to fit in target type");
        assert_one_token(expected, "0x8000000000000000");

        // Malformed
        let expected = Err(
            "Can't parse this; it looks like an integer literal, but invalid digit found in string",
        );
        assert_one_token(expected, "0_b1");
        let expected = Err("Can't parse this; it looks like an integer literal, but cannot parse integer from empty string");
        assert_one_token(expected, "0x");
        let expected = Err(
            "Can't parse this; it looks like an integer literal, but invalid digit found in string",
        );
        assert_one_token(expected, "0oO");

        // What's a 4? https://xkcd.com/953/
        assert_one_token(expected, "0b2");
        assert_one_token(expected, "0xG");
        assert_one_token(expected, "0o8");
    }
    fn assert_one_token(expected: Result<Kind, &str>, s: &str) {
        println!("Tokenizing {:?}", s);
        let mut codemap = CodeMap::new();
        let file = codemap.add_file("single_token.ndca".to_string(), s.to_string());
        let result = tokenize(&file);
        match expected {
            Ok(expected_ok) => {
                let tokens = result.unwrap();
                let mut it = tokens.iter();
                let t = it.next();
                assert_eq!(
                    expected_ok,
                    t.unwrap().kind,
                    "Token is {:?}",
                    t.unwrap().kind,
                );
                let t = it.next();
                assert!(t.is_none(), "Token is {:?}", t.unwrap().kind);
            }
            Err(expected_err) => {
                if let Ok(tokens) = result {
                    panic!(
                        "Expected no tokens; got some: {:?}",
                        tokens.iter().map(|t| t.kind).collect_vec(),
                    );
                }
            }
        }
    }

    #[test]
    fn test_tokenizer() {
        let mut codemap = CodeMap::new();
        let file = codemap.add_file(
            "test_tokenizer.ndca".to_string(),
            "
                @name \"What's in a string?\"
                @states [ #dead, #live ]
                @transition {
                    match this {
                        case #dead {
                            // die.
                            remain
                        }
                        case #live {
                            /**
                            * live
                            */
                            become ( #50 )
                        }
                    }
                }
            "
            .to_string(),
        );

        let tokens = tokenize(&file).expect("Tokenization failed");
        println!("{:?}", tokens);
        let mut it = tokens.iter().map(|t| (t.as_str(), t.kind));

        use Keyword::*;
        use Operator::*;
        use Punctuation::*;

        assert_eq!(it.next().unwrap(), ("@", Kind::Operator(At)));
        assert_eq!(it.next().unwrap(), ("name", Kind::Ident));
        let string = it.next().unwrap();
        assert_eq!(string.0, "\"What's in a string?\"");
        if let Kind::String {
            prefix,
            quote,
            contents,
        } = string.1
        {
            assert_eq!(prefix, None);
            assert_eq!(quote, '"');
            assert_eq!(file.source_slice(contents), "What's in a string?");
        } else {
            panic!("Expected string; got: {:?}", string);
        }

        assert_eq!(it.next().unwrap(), ("@", Kind::Operator(At)));
        assert_eq!(it.next().unwrap(), ("states", Kind::Ident));
        assert_eq!(it.next().unwrap(), ("[", Kind::Punctuation(LBracket)));
        assert_eq!(it.next().unwrap(), ("#", Kind::Operator(Tag)));
        assert_eq!(it.next().unwrap(), ("dead", Kind::Ident));
        assert_eq!(it.next().unwrap(), (",", Kind::Punctuation(Comma)));
        assert_eq!(it.next().unwrap(), ("#", Kind::Operator(Tag)));
        assert_eq!(it.next().unwrap(), ("live", Kind::Ident));
        assert_eq!(it.next().unwrap(), ("]", Kind::Punctuation(RBracket)));

        assert_eq!(it.next().unwrap(), ("@", Kind::Operator(At)));
        assert_eq!(it.next().unwrap(), ("transition", Kind::Ident));
        assert_eq!(it.next().unwrap(), ("{", Kind::Punctuation(LBrace)));

        assert_eq!(it.next().unwrap(), ("match", Kind::Keyword(Match)));
        assert_eq!(it.next().unwrap(), ("this", Kind::Ident));
        assert_eq!(it.next().unwrap(), ("{", Kind::Punctuation(LBrace)));

        assert_eq!(it.next().unwrap(), ("case", Kind::Keyword(Case)));
        assert_eq!(it.next().unwrap(), ("#", Kind::Operator(Tag)));
        assert_eq!(it.next().unwrap(), ("dead", Kind::Ident));
        assert_eq!(it.next().unwrap(), ("{", Kind::Punctuation(LBrace)));
        assert_eq!(it.next().unwrap(), ("remain", Kind::Keyword(Remain)));
        assert_eq!(it.next().unwrap(), ("}", Kind::Punctuation(RBrace)));

        assert_eq!(it.next().unwrap(), ("case", Kind::Keyword(Case)));
        assert_eq!(it.next().unwrap(), ("#", Kind::Operator(Tag)));
        assert_eq!(it.next().unwrap(), ("live", Kind::Ident));
        assert_eq!(it.next().unwrap(), ("{", Kind::Punctuation(LBrace)));
        assert_eq!(it.next().unwrap(), ("become", Kind::Keyword(Remain)));
        assert_eq!(it.next().unwrap(), ("(", Kind::Punctuation(LParen)));
        assert_eq!(it.next().unwrap(), ("#", Kind::Operator(Tag)));
        assert_eq!(it.next().unwrap(), ("50", Kind::Integer(50)));
        assert_eq!(it.next().unwrap(), (")", Kind::Punctuation(RParen)));
        assert_eq!(it.next().unwrap(), ("}", Kind::Punctuation(RBrace)));

        assert_eq!(it.next().unwrap(), ("}", Kind::Punctuation(RBrace)));

        assert_eq!(it.next().unwrap(), ("}", Kind::Punctuation(RBrace)));

        assert_eq!(it.next(), None);
    }
}
