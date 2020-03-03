use super::ast::*;

trait OptimizeAst {
    fn optimize(&mut self);
}

impl OptimizeAst for Vec<Statement> {
    fn optimize(&mut self) {
        // no optimization
    }
}

impl OptimizeAst for Statement {
    fn optimize(&mut self) {
        // no optimization
    }
}

impl OptimizeAst for Expr {
    fn optimize(&mut self) {
        // no optimization
    }
}
