/// Kind of node in the syntax tree.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    ERROR = 0,

    // Composite nodes
    PARENS,
    BRACKETS,
    BRACES,
    STATEMENT,
    ASSIGNMENT,

    IDENT,

    INTEGER,

    // Punctuation
    L_PAREN,
    R_PAREN,
    L_BRACKET,
    R_BRACKET,
    L_BRACE,
    R_BRACE,
    COMMA,
    SEMICOLON,
    PERIOD,
    BACKTICK,

    // Comparison operators
    EQL,
    NEQ,
    LT,
    GT,
    LTE,
    GTE,

    // Arithmetic operators
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    PERCENT,
    DOUBLE_ASTERISK,

    // Bitshift operators
    DOUBLE_LESS_THAN,
    DOUBLE_GREATER_THAN,
    TRIPLE_GREATER_THAN,

    // Bitwise/set operators
    AMPERSAND,
    PIPE,
    CARET,
    TILDE,

    // Miscellaneous operators
    AT,
    DOT_DOT,
    OCTOTHORPE,

    // Assignment operators
    ASSIGN,
    PLUS_ASSIGN,
    MINUS_ASSIGN,
    ASTERISK_ASSIGN,
    SLASH_ASSIGN,
    PERCENT_ASSIGN,
    DOUBLE_ASTERISK_ASSIGN,
    DOUBLE_LESS_THAN_ASSIGN,
    DOUBLE_GREATER_THAN_ASSIGN,
    TRIPLE_GREATER_THAN_ASSIGN,
    AMPERSAND_ASSIGN,
    PIPE_ASSIGN,
    CARET_ASSIGN,
    TILDE_ASSIGN,

    // Keyword (type name)
    INTEGER_TYPE,
    INTEGER_SET,
    VECTOR_TYPE,
    VECTOR_SET,
    CELL_TYPE,
    CELL_SET_TYPE,
    ARRAY_TYPE,
    PATTERN_TYPE,
    MASK_TYPE,
    TAG_TYPE,

    // Keyword (returns)
    BECOME,
    REMAIN,
    RETURN,

    // Special nodes
    WHITESPACE,
    COMMENT,
    ROOT,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::ROOT as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) } // grr unsafe
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}
