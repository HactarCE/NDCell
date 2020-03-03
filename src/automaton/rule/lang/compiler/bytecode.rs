use super::ast::*;
use super::Instruction;

trait IntoBytecode {
    fn into_bytecode(self) -> Vec<Instruction>;
}

impl IntoBytecode for Vec<Statement> {
    fn into_bytecode(self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl IntoBytecode for Statement {
    fn into_bytecode(self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl IntoBytecode for Expr {
    fn into_bytecode(self) -> Vec<Instruction> {
        unimplemented!()
    }
}
