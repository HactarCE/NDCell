#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub inner: T,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: TextPoint,
    pub end: TextPoint,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextPoint {
    pub line: usize,
    pub column: usize,
}
impl Default for TextPoint {
    fn default() -> Self {
        Self { line: 1, column: 1 }
    }
}
