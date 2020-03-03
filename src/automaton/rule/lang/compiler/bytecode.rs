use std::convert::TryFrom;
use std::fmt;

use super::*;

#[derive(Debug)]
pub struct ProgramFunction {
    pub instructions: Instructions,
    pub vars: VarMapping,
}
impl fmt::Display for ProgramFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "    Variables [")?;
        for id in 0..self.vars.len() {
            writeln!(
                f,
                "  {:<7} {:<15?} {}",
                id,
                self.vars.get_type(id),
                self.vars.get_name(id)
            )?;
        }
        writeln!(f, "    ]")?;
        writeln!(f, "    Instructions [")?;
        for instruction in &self.instructions {
            writeln!(
                f,
                "      {:<6}  {:?}",
                instruction.span.end, instruction.inner
            )?;
        }
        write!(f, "    ]")?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct Program {
    pub transition_function: ProgramFunction,
}
impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Program [")?;
        writeln!(
            f,
            "  Transition Function [\n{}\n  ]",
            self.transition_function
        )?;
        write!(f, "]")?;
        Ok(())
    }
}
impl TryFrom<&[Spanned<Directive>]> for Program {
    type Error = LangError;
    fn try_from(directives: &[Spanned<Directive>]) -> LangResult<Self> {
        let mut transition_function = None;
        for directive in directives {
            match &directive.inner {
                Directive::Transition(block) => {
                    if transition_function.is_some() {
                        lang_err(directive, "Multiple transition functions")?
                    } else {
                        let mut instructions = vec![];
                        let mut vars = VarMapping::new();
                        block.make_bytecode(&mut instructions, &mut vars)?;
                        transition_function = Some(ProgramFunction { instructions, vars });
                    }
                }
            }
        }
        let transition_function =
            transition_function.ok_or(lang_error(Span::default(), "No transition function"))?;
        Ok(Self {
            transition_function,
        })
    }
}

trait MakeBytecode {
    fn make_bytecode(
        &self,
        instructions: &mut Instructions,
        vars: &mut VarMapping,
    ) -> LangResult<Type>;
}

impl MakeBytecode for Block {
    fn make_bytecode(
        &self,
        instructions: &mut Instructions,
        vars: &mut VarMapping,
    ) -> LangResult<Type> {
        for statement in self {
            statement.make_bytecode(instructions, vars)?;
        }
        Ok(Type::Void)
    }
}

impl MakeBytecode for Spanned<Statement> {
    fn make_bytecode(
        &self,
        instructions: &mut Instructions,
        vars: &mut VarMapping,
    ) -> LangResult<Type> {
        match &self.inner {
            Statement::Become(expr) => {
                expr.make_bytecode(instructions, vars)?;
                instructions.push(self.replace(Instruction::Become));
            }
        }
        Ok(Type::Void)
    }
}

impl MakeBytecode for Spanned<Expr> {
    fn make_bytecode(
        &self,
        instructions: &mut Instructions,
        vars: &mut VarMapping,
    ) -> LangResult<Type> {
        match &self.inner {
            Expr::Int(i) => {
                instructions.push(self.replace(Instruction::PushInt(*i)));
                Ok(Type::Int)
            }

            Expr::Tag(expr) => match expr.make_bytecode(instructions, vars)? {
                Type::Int => {
                    instructions.push(self.replace(Instruction::GetStateFromInt));
                    Ok(Type::CellState)
                }
                other_type => lang_err(
                    &**expr,
                    format!(
                        "Type error: unary '#' requires an integer, not {}",
                        other_type
                    ),
                ),
            },

            Expr::Neg(expr) => match expr.make_bytecode(instructions, vars)? {
                Type::Int => {
                    instructions.push(self.replace(Instruction::NegInt));
                    Ok(Type::Int)
                }
                other_type => lang_err(
                    &**expr,
                    format!(
                        "Type error: unary '-' requires an integer, not {}",
                        other_type
                    ),
                ),
            },

            Expr::Add(expr1, expr2) | Expr::Sub(expr1, expr2) => {
                let ty1 = expr1.make_bytecode(instructions, vars)?;
                let ty2 = expr2.make_bytecode(instructions, vars)?;
                let op_str = match &self.inner {
                    Expr::Add(_, _) => "+",
                    Expr::Sub(_, _) => "-",
                    _ => unreachable!(),
                };
                match (op_str, ty1, ty2) {
                    ("+", Type::Int, Type::Int) => {
                        instructions.push(self.replace(Instruction::AddInt));
                        Ok(Type::Int)
                    }
                    ("-", Type::Int, Type::Int) => {
                        instructions.push(self.replace(Instruction::SubInt));
                        Ok(Type::Int)
                    }
                    (op_str, other_ty1, other_ty2) => lang_err(
                        self,
                        format!(
                            "Type error: '{}' requires two integers, not {} and {}",
                            op_str, other_ty1, other_ty2
                        ),
                    ),
                }
            }

            Expr::Var(name) => {
                if vars.is_registered(name) {
                    let id = vars[name.as_ref()];
                    instructions.push(self.replace(Instruction::VarFetch(id)));
                    Ok(vars.get_type(id))
                } else {
                    lang_err(
                        self,
                        format!(
                            "Uninitialized variable: Variable '{}' must be initialized before is is used in an expression",
                            name
                        ),
                    )
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::tokenizer;
    use super::*;

    #[test]
    fn test_bytecode() {
        let source_code = "
// @states [ #dead, #live ]
@transition {
    become #(1 - (2 + -3) + 12)
}
";

        let tokens = tokenizer::tokenize(source_code).expect("Tokenization failed");
        let ast = TokenFeeder::from(&tokens[..])
            .program()
            .expect("AST generation failed");
        let program = Program::try_from(&ast[..]).expect("Instruction generation failed");
        assert_eq!(
            "Program [
  Transition Function [
    Variables [
    ]
    Instructions [
      57      PushInt(1)
      62      PushInt(2)
      67      PushInt(3)
      67      NegInt
      68      AddInt
      68      SubInt
      73      PushInt(12)
      74      AddInt
      74      GetStateFromInt
      74      Become
    ]
  ]
]",
            program.to_string()
        );
    }
}
