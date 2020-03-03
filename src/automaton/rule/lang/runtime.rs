use super::*;
use instructions::Instruction;

// #[derive(Debug, Default, Clone)]
// pub struct TypeData<T: Copy> {
//     stack: Vec<T>,
//     variables: Vec<Option<T>>,
// }
// impl<T: Copy> TypeData<T> {
//     fn fetch_var(&mut self, id: usize) -> Result<(), ()> {
//         self.stack.push(self.variables[id].ok_or(())?);
//         Ok(())
//     }
//     fn assign_var(&mut self, id: usize) -> Result<(), ()> {
//         self.variables[id] = Some(self.stack.pop().ok_or(())?);
//         Ok(())
//     }
//     fn push(&mut self, value: T) -> Result<(), ()> {
//         self.stack.push(value);
//         Ok(())
//     }
//     fn pop(&mut self) -> Result<T, ()> {
//         self.stack.pop().ok_or(())
//     }
//     fn get(&mut self, id: usize) -> Result<T, ()> {
//         self.variables[id].ok_or(())
//     }
// }

#[derive(Debug, Clone)]
pub struct Runtime {
    instructions: Vec<Instruction>,
    instruction_spans: Vec<Span>,
    instruction_pointer: usize,
    // ints: TypeData<i32>,
}
impl Runtime {
    fn step(&mut self) -> Result<(), ()> {
        // match self.instructions.get(self.instruction_pointer).ok_or(())? {
        //     Instruction::VarAssign(ty, id) => match ty {
        //         vars::Type::Int => self.ints.assign_var(*id)?,
        //     },
        //     Instruction::VarFetch(ty, id) => match ty {
        //         vars::Type::Int => self.ints.fetch_var(*id)?,
        //     },
        //     Instruction::PushInt(value) => self.ints.push(*value)?,
        //     Instruction::AddInt => {
        //         let a = self.ints.pop()?;
        //         let b = self.ints.pop()?;
        //         self.ints.push(a + b)?;
        //     }
        // }
        unimplemented!()
        // Ok(())
    }
}
