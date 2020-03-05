use super::*;

#[derive(Debug, Clone)]
pub struct Runtime<'a> {
    /// Immutable reference to source function.
    function: &'a Function,
    /// List of instructions, inlined (kind of) for speed. This removes one
    /// indirection versus accessing instructions via the function.
    instructions: &'a [Instruction],
    /// Address of the instruction to execute next.
    instruction_pointer: usize,
    /// Stack of values, including local variables.
    stack: Vec<Value>,
}
impl<'a> Runtime<'a> {
    pub fn new(function: &'a Function) -> Self {
        let mut ret = Self {
            function,
            instructions: &function.instructions,
            instruction_pointer: 0,
            stack: vec![],
        };
        ret.reset();
        ret
    }
    pub fn reset(&mut self) {
        self.instruction_pointer = 0;
        self.stack.clear();
        self.stack.resize(self.function.vars.len(), Value::Null);
    }
    pub fn run(&mut self, max_steps: usize) -> LangResult<Value> {
        for _ in 0..max_steps {
            if let Some(ret) = self.step()? {
                return Ok(ret);
            }
        }
        Err(self.error(format!(
            "Execution exceeded maximum number of steps {}; this is the last instruction executed",
            max_steps
        )))
    }

    fn push_value(&mut self, value: Value) {
        self.stack.push(value);
    }
    fn pop_value(&mut self) -> Value {
        if self.stack.len() <= self.function.vars.len() {
            panic!("Tried to pop from local variables in stack");
        }
        self.stack
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

    fn error<M: Into<LangErrorMsg>>(&self, msg: M) -> LangError {
        (
            self.function
                .instruction_spans
                .get(self.instruction_pointer)
                .copied()
                .unwrap_or_default(),
            msg.into(),
        )
    }
    fn internal_type_error(&self) -> ! {
        panic!(
            "Internal type error in {:?}",
            self.get_current_instruction().unwrap()
        );
    }

    fn get_current_instruction(&self) -> Option<Instruction> {
        self.instructions.get(self.instruction_pointer).cloned()
    }

    pub fn step(&mut self) -> LangResult<Option<Value>> {
        let instruction;
        if let Some(instr) = self.get_current_instruction() {
            instruction = instr;
        } else {
            return Ok(Some(Value::Void));
        }
        match instruction {
            Instruction::VarAssign(var_id) => self.stack[var_id] = self.pop_value(),
            Instruction::VarFetch(var_id) => self.push_value(self.stack[var_id]),
            Instruction::PushInt(i) => self.push_value(Value::Int(i)),
            Instruction::GetStateFromInt => {
                // TODO check if this cell state is in bounds.
                let arg = self.pop_int();
                self.push_value(Value::CellState(arg))
            }
            Instruction::AddInt => {
                let (arg2, arg1) = (self.pop_int(), self.pop_int());
                self.push_value(Value::Int(arg1.checked_add(arg2).ok_or(self.error(
                    format!("Overflow during integer addition ({} + {})", arg1, arg2),
                ))?))
            }
            Instruction::SubInt => {
                let (arg2, arg1) = (self.pop_int(), self.pop_int());
                self.push_value(Value::Int(arg1.checked_sub(arg2).ok_or(self.error(
                    format!("Underflow during integer subtraction ({} - {})", arg1, arg2),
                ))?))
            }
            Instruction::NegInt => {
                let arg = self.pop_int();
                self.push_value(Value::Int(arg.checked_neg().ok_or(
                    self.error(format!("Cannot negate minimum integer value ({})", arg)),
                )?))
            }
            Instruction::Return => return Ok(Some(self.pop_value())),
        }
        self.instruction_pointer += 1;
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

        let ast = ast::make_ast(source_code).expect("AST generation failed");
        let transition_function_block = if let ast::Directive::Transition(block) = &ast[0].inner {
            block
        } else {
            panic!("Could not find transition function");
        };
        let transition_function =
            compile_transition_function(&transition_function_block).expect("Compilation failed");
        let mut runtime = Runtime::new(&transition_function);

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
