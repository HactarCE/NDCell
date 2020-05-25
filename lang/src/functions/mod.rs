//! Built-in functions, methods/properties, and operators.

pub mod cmp;
pub mod convert;
pub mod literals;
pub mod math;
pub mod misc;
pub mod vectors;

pub fn lookup_function_name(name: &str) -> Option<Box<dyn crate::ast::Function>> {
    match name {
        "abs" => todo!("absolute value function"),
        _ => None,
    }
}

pub fn lookup_method_name(ty: crate::Type, _name: &str) -> Option<Box<dyn crate::ast::Function>> {
    use crate::Type;
    match ty {
        Type::Int => todo!("methods on integers"),
        Type::CellState => todo!("methods on cell states"),
        Type::Vector(_) => todo!("methods on vectors"),
    }
}
