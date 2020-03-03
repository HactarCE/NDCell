use std::convert::TryFrom;
use std::fmt;

use super::*;

#[derive(Debug)]
pub struct Program {
    pub transition_function: Function,
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
                        let mut ctx = BytecodeContext::new(FunctionType::TransitionFunction);
                        block.make_bytecode(&mut ctx)?;
                        transition_function = Some(ctx.into());
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

#[derive(Debug)]
enum FunctionType {
    HelperFunction,
    TransitionFunction,
    StatesFunction,
}

impl Into<Function> for BytecodeContext {
    fn into(self) -> Function {
        Function {
            instructions: self.instructions,
            vars: self.vars,
        }
    }
}

struct BytecodeContext {
    fn_type: FunctionType,
    instructions: Instructions,
    vars: VarMapping,
}
impl BytecodeContext {
    fn new(fn_type: FunctionType) -> Self {
        Self {
            fn_type,
            instructions: vec![],
            vars: VarMapping::new(),
        }
    }
}

trait MakeBytecode {
    #[must_use]
    fn make_bytecode(&self, ctx: &mut BytecodeContext) -> LangResult<Type>;
}

impl MakeBytecode for Block {
    fn make_bytecode(&self, ctx: &mut BytecodeContext) -> LangResult<Type> {
        for statement in self {
            statement.make_bytecode(ctx)?;
        }
        Ok(Type::Void)
    }
}

impl MakeBytecode for Spanned<Statement> {
    fn make_bytecode(&self, ctx: &mut BytecodeContext) -> LangResult<Type> {
        match &self.inner {
            Statement::Become(expr) => match expr.make_bytecode(ctx)? {
                Type::CellState => {
                    ctx.instructions.push(self.replace(Instruction::Become));
                    Ok(Type::Void)
                }
                Type::Int => lang_err(
                    expr,
                    "Type error: use '#' to convert an integer to a cell state",
                ),
                other_type => lang_err(
                    expr,
                    format!(
                        "Type error: 'become' requires a cell state, not {}",
                        other_type
                    ),
                ),
            },
        }
    }
}

impl MakeBytecode for Spanned<Expr> {
    fn make_bytecode(&self, ctx: &mut BytecodeContext) -> LangResult<Type> {
        match &self.inner {
            Expr::Int(i) => {
                ctx.instructions
                    .push(self.replace(Instruction::PushInt(*i)));
                Ok(Type::Int)
            }

            Expr::Tag(expr) => match expr.make_bytecode(ctx)? {
                Type::Int => {
                    ctx.instructions
                        .push(self.replace(Instruction::GetStateFromInt));
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

            Expr::Neg(expr) => match expr.make_bytecode(ctx)? {
                Type::Int => {
                    ctx.instructions.push(self.replace(Instruction::NegInt));
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
                let ty1 = expr1.make_bytecode(ctx)?;
                let ty2 = expr2.make_bytecode(ctx)?;
                let op_str = match &self.inner {
                    Expr::Add(_, _) => "+",
                    Expr::Sub(_, _) => "-",
                    _ => unreachable!(),
                };
                match (op_str, ty1, ty2) {
                    ("+", Type::Int, Type::Int) => {
                        ctx.instructions.push(self.replace(Instruction::AddInt));
                        Ok(Type::Int)
                    }
                    ("-", Type::Int, Type::Int) => {
                        ctx.instructions.push(self.replace(Instruction::SubInt));
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
                if ctx.vars.is_registered(name) {
                    let id = ctx.vars[name.as_ref()];
                    ctx.instructions
                        .push(self.replace(Instruction::VarFetch(id)));
                    Ok(ctx.vars.get_type(id))
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
