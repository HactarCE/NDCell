use std::collections::HashMap;

use super::value::Value;
use crate::ast;

pub struct Ctx {
    pub vars: HashMap<ast::VarId, Value>,
}
