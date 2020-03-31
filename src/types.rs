use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Int,
    CellState,
    // Pattern,
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Void => "void",
                Self::Int => "integer",
                Self::CellState => "cell state",
                // Self::Pattern => "pattern",
            }
        )
    }
}
