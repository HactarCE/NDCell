use super::{Type, Value};

#[derive(Debug, Clone)]
pub enum LlvmValue {
    Constant(Value),
}
impl LlvmValue {
    /// Returns the type of the value.
    pub fn ty(&self) -> Type {
        match self {
            LlvmValue::Constant(v) => v.ty(),
        }
    }
}
