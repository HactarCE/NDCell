use lazy_static::lazy_static;
use regex::Regex;

use super::*;

pub type Token<'a> = Spanned<&'a str>;

lazy_static! {
    /// A regular expression that matches valid tokens.
    ///
    /// See https://docs.rs/regex/1.3.4/regex/ for documentation on regexes in
    /// Rust.
    ///
    /// ```
    /// (#|@)?[[:word:]]+|\.\.|[^\s]
    ///
    ///       [[:word:]]+               Match an alphanumeric + underscores string
    /// (#|@)?                            optionally beginning with '@' or '#'
    ///                  |            OR
    ///                   \.\.          the string ".."
    ///                       |       OR
    ///                        [^\s]    any single non-whitespace character
    /// ```
    static ref TOKEN_PATTERN: Regex =
        Regex::new(r"(#|@)?[[:word:]]+|\.\.|[^\s]").expect("Failed to compile tokenizer regex");
}

pub fn tokenize(s: &str) -> impl Iterator<Item = Token> {
    s.lines()
        .enumerate()
        .map(|(line_number, line)| {
            TOKEN_PATTERN
                .find_iter(line)
                .map(move |regex_match| Spanned {
                    span: Span {
                        start: TextPoint {
                            line: line_number + 1,
                            column: regex_match.start(),
                        },
                        end: TextPoint {
                            line: line_number + 1,
                            column: regex_match.end(),
                        },
                    },
                    inner: regex_match.as_str(),
                })
        })
        .flatten()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenizer() {
        let source_code = "
@states [ #dead, #live ]
if this is {
  case #dead
    remain
  case #live
    become ( #live )
}
";
        let tokens: Vec<_> = tokenize(source_code).collect();
        println!("{:?}", tokens);
        // Check number of tokens.
        assert_eq!(20, tokens.len());
        // Check content of "@states" token on line 2.
        assert_eq!("@states", tokens[0].inner);
        // Check content of "#dead" token on line 4.
        assert_eq!("#dead", tokens[11].inner);
        // Check span of "this" token on line 3.
        assert_eq!(
            Spanned {
                span: Span {
                    start: TextPoint { line: 3, column: 3 },
                    end: TextPoint { line: 3, column: 7 }
                },
                inner: "this"
            },
            tokens[7]
        );
    }
}
