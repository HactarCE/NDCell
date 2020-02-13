#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lexeme<'a> {
    Integer(i64),
    Plus,
    Keyword(KeywordLexeme),
    Ident(&'a str),
    Unknown(&'a str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeywordLexeme {
    Set,
}
