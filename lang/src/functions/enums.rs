/// Whether to perform negation or absolute value.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NegOrAbsMode {
    /// Unconditionally negate the input.
    Negate,
    /// Return the absolute value of the input (used as a function).
    AbsFunc,
    /// Return the absolute value of the input (used as a method).
    AbsMethod,
}

/// Property of an integer range.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RangeProperty {
    Start = 0,
    End = 1,
    Step = 2,
}

enum_with_str_repr! {
    /// Which boolean operation to perform.
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum LogicalBinOpType {
        /// Logical OR.
        Or = "or",
        /// Logical XOR.
        Xor = "xor",
        /// Logicial AND.
        And = "and",
    }

    /// Whether to get the minimum or maximum component of a vector.
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum MinMaxMode {
        /// Get the maximum component. ("smax")
        Max = "max",
        /// Get the minimum component. ("smin")
        Min = "min",
    }

    /// Whether to take a sum or product of a vector.
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum SumOrProduct {
        /// Add up all the components.
        Sum = "sum",
        /// Multply all the components.
        Product = "product",
    }
}
