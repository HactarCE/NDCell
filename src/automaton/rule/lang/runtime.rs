use super::*;
use instructions::*;

#[derive(Debug)]
pub struct StackFrame {
    function_id: usize,
    instruction_pointer: usize,
    vars: Vec<Value>,
}
impl StackFrame {
    fn new(functions: &[Function], function_id: usize) -> Self {
        Self {
            function_id,
            instruction_pointer: 0,
            vars: vec![Value::Null; functions[function_id].vars.len()],
        }
    }
}

#[derive(Debug)]
pub struct Runtime {
    functions: Vec<Function>,
    frame: StackFrame,
    call_stack: Vec<StackFrame>,
    value_stack: Vec<Value>,
}
impl Runtime {
    fn new(functions: Vec<Function>) -> Self {
        Self {
            frame: StackFrame::new(&functions, 0),
            call_stack: vec![],
            value_stack: vec![],
            functions,
        }
    }
    fn pop_value(&mut self) -> Value {
        self.value_stack
            .pop()
            .expect("Tried to pop from empty value stack")
    }
    fn pop_int(&mut self) -> i64 {
        if let Value::Int(i) = self.pop_value() {
            i
        } else {
            self.internal_type_error()
        }
    }
    fn push_value(&mut self, value: Value) {
        self.value_stack.push(value);
    }
    fn internal_type_error(&self) -> ! {
        panic!(
            "Internal type error in {:?}",
            self.get_current_instruction().unwrap()
        );
    }
    fn get_current_instruction(&self) -> Option<Spanned<Instruction>> {
        self.functions[self.frame.function_id]
            .instructions
            .get(self.frame.instruction_pointer)
            .cloned()
    }
    pub fn step(&mut self) -> LangResult<Option<Value>> {
        let instruction;
        if let Some(instr) = self.get_current_instruction() {
            instruction = instr;
        } else {
            return Ok(Some(Value::Void));
        }
        match instruction.inner {
            Instruction::VarAssign(var_id) => self.frame.vars[var_id] = self.pop_value(),
            Instruction::VarFetch(var_id) => self.push_value(self.frame.vars[var_id]),
            Instruction::PushInt(i) => self.push_value(Value::Int(i)),
            Instruction::GetStateFromInt => {
                // TODO check if this cell state is in bounds.
                let arg = self.pop_int();
                self.push_value(Value::CellState(arg))
            }
            Instruction::AddInt => {
                let (arg2, arg1) = (self.pop_int(), self.pop_int());
                self.push_value(Value::Int(arg1.checked_add(arg2).ok_or(lang_error(
                    instruction,
                    format!("Overflow during integer addition ({} + {})", arg1, arg2),
                ))?))
            }
            Instruction::SubInt => {
                let (arg2, arg1) = (self.pop_int(), self.pop_int());
                self.push_value(Value::Int(arg1.checked_sub(arg2).ok_or(lang_error(
                    instruction,
                    format!("Underflow during integer subtraction ({} - {})", arg1, arg2),
                ))?))
            }
            Instruction::NegInt => {
                let arg = self.pop_int();
                self.push_value(Value::Int(arg.checked_neg().ok_or(lang_error(
                    instruction,
                    format!("Cannot negate minimum integer value ({})", arg),
                ))?))
            }
            Instruction::Become | Instruction::Return => return Ok(Some(self.pop_value())),
        }
        self.frame.instruction_pointer += 1;
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime() {
        let source_code = "
// @states [ #dead, #live ]
@transition {
    become #(1 - (2 + -3) + 12)
}
";

        let program = compile(source_code).expect("Compilation failed");
        let mut runtime = Runtime::new(vec![program.transition_function]);

        let mut ret = None;
        for _ in 0..100 {
            ret = runtime.step().expect("Runtime error");
            if ret.is_some() {
                break;
            }
        }
        let ret = ret.expect("Program took too long (probably stuck in an infinite loop");
        assert_eq!(Value::CellState(1 - (2 + -3) + 12), ret);
    }
}
